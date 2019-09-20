#Now available on github

#load data on R
read.dta("jtpa")

# libraries
library(foreign)          # package to read Stata file
library(taRifx)           # package to destring
library(doBy)             # package for group summary
library(clubSandwich)     # package for standard errors
library(lmtest)           # package for standard errors
library(sandwich)         # package for standard errors
install.packages("gmodels") l
ibrary(gmodels)
library(pastecs)
library(ivpack)           # package for IV and 2SLS

#fix for missing variables
jtpa$hsorged[jtpa$hsorged!=0 & jtpa$hsorged!=1] <- NA
jtpa$hsorged[jtpa$hsorged!=0 & jtpa$hsorged!=1] <- NA

#select subset of variables we will use 

summary(jtpa)

females <- (data=subset(jtpa,sex==0))
summary(females)

males <- (data=subset(jtpa,sex==1))
summary(males)

notassigned <- (subset(jtpa,assignmt==0))
summary(notassigned)
assigned <- (subset(jtpa,assignmt==1))
summary(assigned)
enrolled <- (subset(jtpa,training==1))
summary(enrolled)
notenrolled <- (subset(jtpa,training==0))
summary(notenrolled)

assignedf<- (subset(assigned, sex==0))
assignedm<- (subset(assigned, sex==1))

notassignedf<- (subset(notassigned, sex==0))
notassignedm<- (subset(notassigned, sex==1))

enrolledf <- (subset(enrolled,sex==0))
enrolledm <- (subset(enrolled,sex==1))

notenrolledf <- (subset(notenrolled,sex==0))
notenrolledf <- (subset(notenrolled,sex==1))


#checking counterfactual fore females
femalesbaseline <- (data=subset(females, assignmt==0))
summary(femalesbaseline)

femalesbaselinea <- (data=subset(females, assignmt==1))
summary(femalesbaselinea)

##checking counterfactual fore males
malesbaseline <- (data=subset(males, assignmt==0))
summary(malesbaseline)

malesbaselinea <- (data=subset(males, assignmt==1))
summary(malesbaselinea)


#pre post for females (3a)
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(prevearn~ assignmt, data=subset(jtpa, sex==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#pre post for male (3a)
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(prevearn~ assignmt, data=subset(jtpa, sex==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#IV to estimate impact
IVa = ivreg(earnings ~ training| assignmt, data =subset(jtpa, sex==0))
summary(IVa, vcov = sandwich)

IVb = ivreg(earnings ~ training| assignmt, data =subset(jtpa, sex==0))
summary(IVb, vcov = sandwich)


#running simple OLS 
#we want to know the affect of trainings on earnings
ols1 <- lm(earnings ~ training + age + factor(married) + factor(black)  + factor(black) + factor(hispanic) + factor(hsorged) +factor(married), data=subset(jtpa, sex==0))
var.ols1 <- vcovHC(ols1, type = "HC1")
coeftest(ols1, var = var.ols1)

ols2 <- lm(earnings ~ training + age + factor(married) + factor(black)  + factor(black) + factor(hispanic) + factor(hsorged) +factor(married), data=subset(jtpa, sex==1))
var.ols2 <- vcovHC(ols1, type = "HC1")
coeftest(ols2, var = var.ols2)

IV in one stage 
IV1 = ivreg(earnings ~ training + age + factor(married) + factor(black)  + factor(black) + factor(hispanic) + factor(hsorged) +factor(married) | assignmt +  age + factor(married) +factor(black) + factor(hispanic) + factor(hsorged) +factor(married) , data =subset(jtpa, sex==0))
summary(IV1, vcov = sandwich)

IV2 = ivreg(earnings ~ training + age + factor(married) + factor(black)  + factor(black) + factor(hispanic) + factor(hsorged) +factor(married)  | assignmt +  age + factor(married) +factor(black) + factor(hispanic) + factor(hsorged) +factor(married) , data =subset(jtpa, sex==1))
summary(IV2, vcov = sandwich)


#estimating ITT for all classes for females
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & training==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~ assignmt, data=subset(jtpa, sex==0 & training==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#class training
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & training==1 & class_tr==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & training==0 & class_tr==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#on the job training
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & training==1 & ojt_jsa==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & training==0 & ojt_jsa==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#combination
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & training==1 & oth_serv==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0  & training==0 & oth_serv==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)


#estimating ITT for all classes for males

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & training==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~ assignmt, data=subset(jtpa, sex==1 & training==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#class training
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & training==0 & class_tr==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & training==1 & class_tr==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#on the job training
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & training==1 & ojt_jsa==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & training==0 & ojt_jsa==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#combination
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & training==1 & oth_serv==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & training==0 & oth_serv==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)


#for the black population females
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & black==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~ assignmt, data=subset(jtpa, sex==0 & black==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#for the black population males
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & black==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~ assignmt, data=subset(jtpa, sex==1 & black==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#for marital status females
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & married==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~ assignmt, data=subset(jtpa, sex==0 & married ==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#for marital status males
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & married ==1))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~ assignmt, data=subset(jtpa, sex==1 & married ==0))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#for age females 
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & age>30))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==0 & age<30))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

#for age males 
reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & age>30))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)

reg1=lm(earnings~assignmt, data=subset(jtpa, sex==1 & age<30))
var.reg1=vcovHC(reg1, type = "HC1")
coeftest(reg1, vcov = var.reg1)








