library(haven)
library(readr)
library(dplyr)
library(tidyr)

#######################Data prep
setwd() #set your working directory

#data loading & variable setting - YEAR 2014
text14 = read_csv("VS14LKBC.PublicUse.DUSDENOM_2019-08-22", col_names = FALSE, trim_ws = FALSE)

MAGER9 = sapply(text14, function(x) {substr(x, 79, 79)})
MRACEHISP = sapply(text14, function(x) {substr(x, 117, 117)})
MEDUC = sapply(text14, function(x) {substr(x, 124, 124)})
CIG1 = sapply(text14, function(x) {substr(x, 255, 256)})
CIG2 = sapply(text14, function(x) {substr(x, 257, 258)}) 
CIG3 = sapply(text14, function(x) {substr(x, 259, 260)}) 
MULTI = sapply(text14, function(x) {substr(x, 454, 454)})
COMBGEST = sapply(text14, function(x) {substr(x, 490, 491)})

data = data.frame(MAGER9, MRACEHISP, MEDUC, MULTI, COMBGEST, CIG1, CIG2, CIG3)
rm(text14)

###################################
names(data) = c('MAGER9','MRACEHISP','MEDUC','MULTI','COMBGEST','CIG1','CIG2','CIG3')

nrow(data) ##3998175
#exclude GA < 22 or 44 weeks (n = 36765)
data = data %>% filter(as.numeric(COMBGEST) >= 22 & as.numeric(COMBGEST) <= 44)
nrow(data) ##3961410

#exclude other race/ethnicity (n = 508360)
data = data %>% filter(as.numeric(MRACEHISP) %in% c(1,2,7))
nrow(data) ##3453050
data %>% group_by(MRACEHISP) %>% tally() #NHW(1) 2031802, NHB(2) 541135, Hispanic(7) 880113

##Exposure setting
data = data %>% mutate(race = factor(MRACEHISP, levels = c(1, 2, 7)))

##Mediator making
#smoking
data = data %>% mutate(smoke = factor(ifelse(as.numeric(CIG1) + as.numeric(CIG2) + as.numeric(CIG3) == 0 | 
                       is.na(as.numeric(CIG1) + as.numeric(CIG2) + as.numeric(CIG3)), 0, 1)))

##Outcome making
#prematurity < 37 wk
data = data %>% mutate(prem = factor(ifelse(as.numeric(COMBGEST) < 37, 1, 0)))

##Covariage setting
#AGE - ageg : Under 20 (1), 20s (2), 30s(3), 40 and over(4)
data = data %>% mutate(ageg = case_when
                       (MAGER9 %in% c(1,2) ~ 1,
                        MAGER9 %in% c(3,4) ~ 2,
                        MAGER9 %in% c(5,6) ~ 3, 
                        TRUE ~ 4)) %>% mutate(ageg = factor(ageg))

#MEDUC
data = data %>% mutate(medu = factor(ifelse(MEDUC %in% c(1,2,3, 9), 1, 2)))

#singleton
data = data %>% filter(MULTI == 1)

#exposure - race
#mediator - precare
#outcome - preterm
#covariate - ageg, medu

data = data[,c(9:13)]
