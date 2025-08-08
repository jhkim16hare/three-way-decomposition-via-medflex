library(medflex)
library(dplyr)
library(ggplot2)
options(digits = 3)

data(UPBdata)
head(UPBdata)

###########################Three-way Decomposition Sample Analysis####################################
#This section is for Online Resource 2. Three-way decomposition using UPB data                       #
######################################################################################################

#The relationship among continuous(att), categorical(attcat), binary(attbin)
UPBdata %>% mutate(binary = factor(attbin), continuous = att, categorical = attcat) %>% 
  ggplot(aes(x = categorical, y = continuous, group = binary, color = binary)) + geom_point(size = 7) + 
  theme_minimal(base_size = 20)

#######################################################################################################
# Setting 1                                                                                           #    
# Exposure : Continuous                                                                               #  
# Mediator : Continuous                                                                               #
# Outcome  : Binary                                                                                   #
#######################################################################################################

formula = UPB ~ att + negaff + att:negaff + gender + educ + age         #basic formula

expData = neImpute(formula, family = binomial('logit'), data = UPBdata) #expand data by imputation
head(expData[,c(1,2,3,6,8:11)])                                         #selected data for presentation

neMod1 = neModel(UPB ~ att0 + att1 + att0:att1 + gender + educ + age,   
                 #basic formula - mediator(negaff) was replaced by att1(x*)
                 family = binomial("logit"), expData = expData, se = "robust")
summary(neMod1)                                                         #model summary

#linking estimates with decomposed effects 
effdecomp = neEffdecomp(neMod1)                                         
summary(effdecomp)                                                      
#decomposition summary / Y(x, M(x*)) = Y(1, M(0)) -> pure(natural) direct effect + total(natural) indirect effect

#Calculating Odds Ratio for decomposed estimates
coef = summary(effdecomp)$coefficients[,1]
se = summary(effdecomp)$coefficients[,2] 
round(exp(cbind(Estimate = coef, 
                LCI = coef + qnorm(0.025)*se,
                UCI = coef + qnorm(0.975)*se)), 2)

#Plotting the decomposed effects
par(mfrow = c(1,2))
plot(effdecomp, xlab = 'log Odds Ratio')                                #in log Odds ratio scale
plot(effdecomp, xlab= 'Odds Ratio', transf = exp)                       #in Odds ratio scale

#Testing linear combination of decomposed effects
lht = neLht(neMod1, linfct = c("att0 = 0",                              #pure direct effect  (natural direct effect)
                               "att1 = 0",                              #pure indirect effect (not natural indirect effect)
                               "att0:att1 = 0",                         #mediated interaction
                               "att0 + att1 + att0:att1 = 0"            #total effect
                               )
            )

summary(lht, test = adjusted())                                         #statistical testing with adjustment for multiple comparison

#Plotting the three-way decomposition 
par(mfrow = c(1,2))
plot(lht, ylabels = c('pure direct effect','pure indirect effect', 'mediated interaction','total effect'), 
     xlab = 'log Odds Ratio', main = NULL) 
plot(lht, ylabels = c('pure direct effect','pure indirect effect', 'mediated interaction','total effect'), 
     xlab = 'Odds Ratio', main = NULL, transf = exp)


###Effect modification
#mediator - covariate (gender) interaction
formula = UPB ~ att + att:negaff + negaff * gender + educ + age         #basic formula with effect modification by covariate

expData = neImpute(formula, family = binomial('logit'), data = UPBdata) #expand data by imputation
head(expData[,c(1,2,3,6,8:11)])                                         #selected data for presentation

neMod12 = neModel(UPB ~ att0 + att0:att1 + att1 * gender + educ + age,   
                  #basic formula - mediator(negaff) was replaced by att1(x*)
                  family = binomial("logit"), expData = expData, se = "robust")
summary(neMod12)              

#Testing linear combination of decomposed effects
lht = neLht(neMod12, linfct = c("att0 = 0",                                  #pure direct effect (natural direct effect) 
                                "att1 = 0",                                  #pure indirect effect (not natural indirect effect) in female
                                "att0:att1 = 0",                             #mediated interactive effect in female
                                "att1 + att1:genderM = 0",                   #pure indirect effect (not natural indirect effect) in male
                                "att0 + att1 + att0:att1 = 0",               #total effect in female
                                "att0 + att1 + att1:genderM + att0:att1 = 0" #total effect in male
                                )
)

summary(lht, test = adjusted())    

#Plotting the three-way decomposition 
par(mfrow = c(1,2))
plot(lht, ylabels = c('pure direct effect','pure indirect effect in female', 'mediated interaction',
                      'pure indirect effect in male', 'total effect in female', 'total effect in male'), 
     xlab = 'log Odds Ratio', main = NULL) 
plot(lht, ylabels = c('pure direct effect','pure indirect effect in female', 'mediated interaction',
                      'pure indirect effect in male', 'total effect in female', 'total effect in male'), 
     xlab = 'Odds Ratio', main = NULL, transf = exp) 

#######################################################################################################
# Setting 2                                                                                           #    
# Exposure : Binary                                                                                   #  
# Mediator : Continuous                                                                               #
# Outcome  : Binary                                                                                   #
#######################################################################################################

formula = UPB ~ attbin + negaff + attbin:negaff + gender + educ + age         #basic formula

expData = neImpute(formula, family = binomial('logit'), data = UPBdata) #expand data by imputation
head(expData[,c(1,2,3,6,8:11)])                                         #selected data for presentation

neMod2 = neModel(UPB ~ attbin0 + attbin1 + attbin0:attbin1 + gender + educ + age,   
                 #basic formula - mediator(negaff) was replaced by att1(x*)
                 family = binomial("logit"), expData = expData, se = "robust")
summary(neMod2)                                                         #model summary

#linking estimates with decomposed effects 
effdecomp = neEffdecomp(neMod2)                                         
summary(effdecomp)                                                      
#decomposition summary / Y(x, M(x*)) = Y(1, M(0)) -> pure(natural) direct effect + total(natural) indirect effect

#Calculating Odds Ratio for decomposed estimates
coef = summary(effdecomp)$coefficients[,1]
se = summary(effdecomp)$coefficients[,2] 
round(exp(cbind(Estimate = coef, 
                LCI = coef + qnorm(0.025)*se,
                UCI = coef + qnorm(0.975)*se)), 2)

#Plotting the decomposed effects
par(mfrow = c(1,2))
plot(effdecomp, xlab = 'log Odds Ratio')                                #in log Odds ratio scale
plot(effdecomp, xlab= 'Odds Ratio', transf = exp)                       #in Odds ratio scale


#Testing linear combination of decomposed effects
lht = neLht(neMod2, linfct = c("attbin0 = 0",                              #pure direct effect  (natural direct effect)
                               "attbin1 = 0",                              #pure indirect effect (not natural indirect effect)
                               "attbin0:attbin1 = 0",                      #mediated interaction
                               "attbin0 + attbin1 + attbin0:attbin1 = 0"   #total effect
                               )
            )

summary(lht, test = adjusted())                                         #statistical testing with adjustment for multiple comparison

#Plotting the three-way decomposition 
par(mfrow = c(1,2))
plot(lht, ylabels = c('pure direct effect','pure indirect effect', 'mediated interaction','total effect'), 
     xlab = 'log Odds Ratio', main = NULL) 
plot(lht, ylabels = c('pure direct effect','pure indirect effect', 'mediated interaction','total effect'), 
     xlab = 'Odds Ratio', main = NULL, transf = exp) 



#######################################################################################################
# Setting 3                                                                                           #    
# Exposure : Categorical                                                                              #  
# Mediator : Continuous                                                                               #
# Outcome  : Binary                                                                                   #
#######################################################################################################

formula = UPB ~ attcat + negaff + attcat:negaff + gender + educ + age         #basic formula

expData = neImpute(formula, family = binomial('logit'), data = UPBdata) #expand data by imputation
head(expData[,c(1,2,3,6,8:11)])                                         #selected data for presentation

neMod3 = neModel(UPB ~ attcat0 + attcat1 + attcat0:attcat1 + gender + educ + age,   
                 #basic formula - mediator(negaff) was replaced by att1(x*)
                 family = binomial("logit"), expData = expData, se = "robust")
summary(neMod3)                                                         #model summary

#linking estimates with decomposed effects 
effdecomp = neEffdecomp(neMod3)                                         
summary(effdecomp)                                                      
#decomposition summary / Y(x, M(x*)) = Y(1, M(0)) -> pure(natural) direct effect + total(natural) indirect effect

#Calculating Odds Ratio for decomposed estimates
coef = summary(effdecomp)$coefficients[,1]
se = summary(effdecomp)$coefficients[,2] 
round(exp(cbind(Estimate = coef, 
                LCI = coef + qnorm(0.025)*se,
                UCI = coef + qnorm(0.975)*se)), 2)

#Plotting the decomposed effects
par(mfrow = c(1,2))
plot(effdecomp, xlab = 'log Odds Ratio')                                #in log Odds ratio scale
plot(effdecomp, xlab= 'Odds Ratio', transf = exp)                       #in Odds ratio scale


#Testing linear combination of decomposed effects
lht = neLht(neMod3, linfct = c("attcat0M = 0",                                 #pure direct effect (natural direct effect) for category M
                               "attcat1M = 0",                                 #pure indirect effect (not natural indirect effect) for category M
                               "attcat0M:attcat1M = 0",                        #mediated interaction for category M
                               "attcat0H = 0",                                 #pure direct effect (natural direct effect) for category H
                               "attcat1H = 0",                                 #pure indirect effect (not natural indirect effect) for category H
                               "attcat0H:attcat1H = 0",                        #mediated interaction for category H
                               "attcat0M + attcat1M + attcat0M:attcat1M = 0"   #total effect
                                )
            )

summary(lht, test = adjusted())                                         #statistical testing with adjustment for multiple comparison

#Plotting the three-way decomposition 
par(mfrow = c(1,2))
plot(lht, ylabels = c('pure direct effect for category M','pure indirect effect for category M', 'mediated interaction for category M',
                      'pure direct effect for category H','pure indirect effect for category H', 'mediated interaction for category H',
                      'total effect'), 
     xlab = 'log Odds Ratio', main = NULL) 
plot(lht, ylabels = c('pure direct effect for category M','pure indirect effect for category M', 'mediated interaction for category M',
                      'pure direct effect for category H','pure indirect effect for category H', 'mediated interaction for category H',
                      'total effect'), 
     xlab = 'Odds Ratio', main = NULL, transf = exp) 



###########################Additional Function of R medflex###########################################
#This section is for Online Resource 3. Additional functions of R medflex                            #
######################################################################################################

#1. Joint mediation
library(medflex)
impData = neImpute(UPB ~ att + att * initiator * negaff + gender + educ + age,
                    family = binomial("logit"), nMed = 2, data = UPBdata)
neMod = neModel(UPB ~ att0 + att1 + att0:att1 + gender + educ + age, 
                   family = binomial("logit"), expData = impData, se = "bootstrap")
summary(neMod)

(est = summary(neLht(neMod, linfct = c("att0 = 0", "att1 = 0", "att0:att1 = 0", 
                                       "att0 + att0:att1 = 0", "att1 + att0:att1 = 0",
                                       "att0 + att1 + att0:att1 = 0"))))

OR = cbind(exp(est$coefficients[,1])-1, est$coefficients[,2]))
OR[5,1]/OR[6,1]*100


#2. Population average estimate calculation
formula = UPB ~ att + negaff + att:negaff + gender + educ + age
expFit = glm(att ~ gender + educ + age, data = UPBdata)
impData = neImpute(formula, family = binomial('logit'), data = UPBdata) 

neMod1 = neModel(UPB ~ att0 + att1 + att0:att1 + gender + educ + age, family = binomial("logit"), expData = impData, se = "robust")
summary(neMod1)

neMod2 = neModel(UPB ~ att0 + att1 + att0:att1, family = binomial("logit"), expData = impData, xFit = expFit, se = "robust")
summary(neMod2)

