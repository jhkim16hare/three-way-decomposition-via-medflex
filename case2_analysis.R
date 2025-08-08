library(medflex)
options(digits = 3)

formula = prem ~ race + smoke + race:smoke + ageg + medu
expData = neImpute(formula, family = binomial('logit'), data = data)
head(expData)

#set.seed(1234) - when standard error is calculated by bootstrapping
neMod2 = neModel(prem ~ race0 + race1 + race0:race1 + ageg + medu, 
                 #basic formula - mediator(precare) was replaced by race1(x*)
                 family = binomial("logit"), expData = expData, se = 'robust') #se option - 'bootstrap'
summary(neMod2)                                                         #model summary

#linking estimates with decomposed effects 
effdecomp = neEffdecomp(neMod2, xRef = c(1,2)) #compare NHW(1) and NHB(2)                                      
summary(effdecomp)     #decomposition summary 

#Testing linear combination of decomposed effects for Non-hispanic Black (2)
lht1 = neLht(neMod2, linfct = c("race02 = 0",                              #pure direct effect  (natural direct effect)
                                "race12 = 0",                              #pure indirect effect (not natural indirect effect)
                                "race02:race12 = 0",                         #mediated interaction
                                "race02 + race12 + race02:race12 = 0"            #total effect
))

summary(lht1, test = adjusted())                                         #statistical testing with adjustment for multiple comparison


#Testing linear combination of decomposed effects for Hispanic (7)
lht2 = neLht(neMod2, linfct = c("race07 = 0",                              #pure direct effect  (natural direct effect)
                                "race17 = 0",                              #pure indirect effect (not natural indirect effect)
                                "race07:race17 = 0",                         #mediated interaction
                                "race07 + race17 + race07:race17 = 0"            #total effect
))

summary(lht2, test = adjusted())                                         #statistical testing with adjustment for multiple comparison


