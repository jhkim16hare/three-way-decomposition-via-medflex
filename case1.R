library(readxl)
library(medflex)
library(dplyr)
options(digits = 3)

setwd() #set your working directory
data = read_excel("KLoSA_EXCEL/Lt07.xlsx")

#data preparation
data$gender = factor(data$w07gender1) #gender
data$edu = data$w07edu #education
data$age = data$w07A002_age #age, continuous
data$marital = factor(ifelse(data$w07marital == 1, 1, 0)) #marital status
data$housetype = factor(data$w07enu_type) #house type
data$sido = factor(data$w07region1) #province, 17 sido
data$regiontype = factor(data$w07region3) #region type, Large city/mid-sized city/rural area
data$help = ifelse(is.na(data$w07livewith), 0, data$w07livewith) #Cohabiting family members , binary
data$cc = ifelse(data$w07chronic_sum > 0, data$w07chronic_sum, 0) #number of chronic disease
data$smoke = ifelse(is.na(data$w07smoke), 0, data$w07smoke) #smoking status
data$alc = ifelse(is.na(data$w07alc), 1, data$w07alc) #alcohol drinking
data$aid = factor(ifelse(data$w07C301 == 5, 1, 0)) #medicaid
data$phi = ifelse(data$w07C310 == 5, 1, 0) #private health insurance, binary
data$inc = ifelse(is.na(data$w07pinc) | data$w07pinc <= 0, 0, log(data$w07pinc)) #income, continuous, logged
data$wealth = ifelse(is.na(data$w07passets) | data$w07passets <= 0, 0, log(data$w07passets)) #wealth, continuous, logged
data$adl = data$w07adl #Activities of Daily Living
data$hexp = ifelse(is.na(data$w07E237) | data$w07E237 <= 0, 0, log(data$w07E237)) #health expenditure, continuous, logged

#by earning
sub = data %>% mutate(earned = ifelse(is.na(w07earned) | w07earned <0, 0, w07earned)) %>% 
  filter(w07earned <= 7530*209*12*2/10000) #twice of national minimum wage (2018)

sub = sub[,c('hexp','aid','cc','gender','edu','age','marital','housetype','sido','regiontype','help','smoke','alc','phi','inc','wealth')]

##### Aid - CC - Health Expenditure
formula = hexp ~ aid + cc + aid:cc + gender + edu + age + marital + housetype + sido + regiontype + help + smoke + alc + phi + inc + wealth
expData = neImpute(formula, family = 'gaussian', data = sub)

head(expData)

#set.seed(1234)  #when standard error is calculated by bootstrapping
neMod1 = neModel(hexp ~ aid0 + aid1 + aid0:aid1 + gender + edu + age + marital + housetype + sido + regiontype + help + smoke + alc + phi + inc + wealth, 
                family = gaussian, expData = expData, se = 'robust')

summary(neMod1)

cbind(coef(neMod1),confint(neMod1))[c(2,3,32),] #estimate

#Testing linear combination of decomposed effects
lht = neLht(neMod1, linfct = c("aid01 = 0",                              #pure direct effect  (natural direct effect)
                               "aid11 = 0",                              #pure indirect effect (not natural indirect effect)
                               "aid01:aid11 = 0",                         #mediated interaction
                               "aid01 + aid11 + aid01:aid11 = 0"            #total effect
))

summary(lht, test = adjusted()) 
