#################  HAMD (depression data)
# Clinical trial randomly assigned 100 patients with major depression to an experimental drug (D) or to placebo (P). 
# (Source: Dmitrienko et. al. (2005).
                                          
#Participants completed the Hamilton depression rating scale (HAMD) at baseline and again after 9-week treatment. 
#Study outcome was HAMD at the end; higher scores mean worse depression. Participants at 5 centers.

rm(list=ls())

# install.packages("EnvStats")
library(EnvStats)
library(MASS)
library(VIM) ###has hotdeck() function
library(mice)

# install.packages("VIM")
###########  READ IN DATA:
datahamd<-read.delim("C:/Users/18829.DESKTOP-PG2BS5Q/Desktop/flame-missing/hamd.txt")
str(datahamd)
table(datahamd$drug)

########### THE RESULTS OF THE COMPLETE SAMPLE:
mean(datahamd$final) 
mean(datahamd$baseline)
cov(datahamd$final,datahamd$baseline)
var(datahamd$final)
var(datahamd$baseline)
sd(datahamd$baseline)
sd(datahamd$final)
cor(datahamd$baseline,datahamd$final)
lm(datahamd$final~datahamd$baseline)
lm(datahamd$baseline~datahamd$final)

########### AD HOC METHODS UNDER MCAR:

#Generate some missing data under MCAR:
set.seed(2020)  #set the seed
missing<-rbinom(100,1,0.33)    #one tired is missing
dataMCAR<-datahamd

## missing is 0, observation=1
dataMCAR$final[missing==1]<-NA   
dataMCAR$R<-ifelse(is.na(dataMCAR$final),1,0)
table(dataMCAR$drug,dataMCAR$R)     #see how many missing how many 1
table(dataMCAR$center,dataMCAR$R)   #distribution


# different methods:
### Complete Case:
mean(dataMCAR$final,na.rm=T)
sd(dataMCAR$final,na.rm=T)
cor(dataMCAR$final,dataMCAR$baseline,use="complete.obs")
lm(dataMCAR$final~dataMCAR$baseline)
lm(dataMCAR$baseline~dataMCAR$final)
# just use avaliable ones

### Mean Substitution
dataMCAR2<-dataMCAR
dataMCAR2$final[is.na(dataMCAR2$final)]<-rep(mean(dataMCAR2$final,na.rm=T),sum(is.na(dataMCAR2$final)))
# just replace 0 with mean
mean(dataMCAR2$final) 
sd(dataMCAR2$final)
cor(dataMCAR2$baseline,dataMCAR2$final)
lm(dataMCAR2$final~dataMCAR2$baseline)
lm(dataMCAR2$baseline~dataMCAR2$final)


# HotDeck function
dataMCAR3<-dataMCAR
dataMCAR3<-hotdeck(data=dataMCAR, variable="final", ord_var="center")
### sort by center before imputing, final has missing, use center,centers are groups, do hotdeck within centers
# fill your data with target variable
mean(dataMCAR3$final) 
sd(dataMCAR3$final)
cor(dataMCAR3$baseline,dataMCAR3$final)
lm(dataMCAR3$final~dataMCAR3$baseline)
lm(dataMCAR3$baseline~dataMCAR3$final)

# Conditonal Mean
dataMCAR4<-dataMCAR
mCM<-lm(dataMCAR4$final~dataMCAR4$baseline + dataMCAR4$drug + as.factor(dataMCAR4$center))
dataMCAR4$final[is.na(dataMCAR4$final)]<-predict(mCM, data.frame(dataMCAR4))[is.na(dataMCAR4$final)]
# is.na: is final data missing?
# estimating from a regression liner model...
mean(dataMCAR4$final) 
sd(dataMCAR4$final)
cor(dataMCAR4$baseline,dataMCAR4$final)
lm(dataMCAR4$final~dataMCAR4$baseline)
lm(dataMCAR4$baseline~dataMCAR4$final)


# Predictive Distribution
dataMCAR5<-dataMCAR
mPD<-lm(dataMCAR5$final~dataMCAR5$baseline + dataMCAR5$drug + as.factor(dataMCAR5$center))
dataMCAR5$final[is.na(dataMCAR5$final)]<-
predict(mPD, data.frame(dataMCAR5))[is.na(dataMCAR5$final)]+rnorm(sum(is.na(dataMCAR5$final)),0,summary(mPD)$sigma)
# is na missing? +rnorm: how many obervation we missing? measn=0 var=sigma, sample from this distribution
mean(dataMCAR5$final) 
sd(dataMCAR5$final)
cor(dataMCAR5$baseline,dataMCAR5$final)
lm(dataMCAR5$final~dataMCAR5$baseline)
lm(dataMCAR5$baseline~dataMCAR5$final)


####################################
# MAR
########### AD HOC METHODS UNDER MAR:

# Generate some missing data under MAR:
dataMAR<-datahamd
#sort baseline values from lowest to highest
dataMAR<-dataMAR[order(dataMAR$baseline),]

## Create a missing indicator called "miss" where the first 49 observations are low values of Baseline 
## and missingness happens only among the high values (from observation 50 onwards)
miss<-c(rep(0,49),rbinom(51,1,0.66))
dataMAR$final[miss==1]<-NA
dataMAR$R<-ifelse(is.na(dataMAR$final),1,0)
table(dataMAR$drug,dataMAR$R)
table(dataMAR$center,dataMAR$R)

# Complete case
mean(dataMAR$final,na.rm=T)
sd(dataMAR$final,na.rm=T)
cor(dataMAR$final,dataMAR$baseline,use="complete.obs")
lm(dataMAR$final~dataMAR$baseline)
lm(dataMAR$baseline~dataMAR$final)

# Mean Substitution
dataMAR2<-dataMAR
dataMAR2$final[is.na(dataMAR2$final)]<-rep(mean(dataMAR2$final,na.rm=T),sum(is.na(dataMAR2$final)))

mean(dataMAR2$final) 
sd(dataMAR2$final)
cor(dataMAR2$baseline,dataMAR2$final)
lm(dataMAR2$final~dataMAR2$baseline)
lm(dataMAR2$baseline~dataMAR2$final)

# HotDeck
dataMAR3<-dataMAR
dataMAR3<-hotdeck(data=dataMAR3, variable="final", ord_var = "center")###sort by center before imputing

mean(dataMAR3$final) 
sd(dataMAR3$final)
cor(dataMAR3$baseline,dataMAR3$final)
lm(dataMAR3$final~dataMAR3$baseline)
lm(dataMAR3$baseline~dataMAR3$final)

# Conditional Mean
dataMAR4<-dataMAR
mCM<-lm(dataMAR4$final~dataMAR4$baseline + dataMAR4$drug + as.factor(dataMAR4$center))
dataMAR4$final[is.na(dataMAR4$final)]<-predict(mCM, data.frame(dataMAR4))[is.na(dataMAR4$final)]

mean(dataMAR4$final) 
sd(dataMAR4$final)
cor(dataMAR4$baseline,dataMAR4$final)
lm(dataMAR4$final~dataMAR4$baseline)
lm(dataMAR4$baseline~dataMAR4$final)

# Predicitive Distribution
dataMAR5<-dataMAR
mPD<-lm(dataMAR5$final~dataMAR5$baseline + dataMAR5$drug + as.factor(dataMAR5$center))
dataMAR5$final[is.na(dataMAR5$final)]<- predict(mPD, data.frame(dataMAR5))[is.na(dataMAR5$final)]+rnorm(sum(is.na(dataMAR5$final)),0,summary(mPD)$sigma)

mean(dataMAR5$final) 
sd(dataMAR5$final)
cor(dataMAR5$baseline,dataMAR5$final)
lm(dataMAR5$final~dataMAR5$baseline)
lm(dataMAR5$baseline~dataMAR5$final)

########### AD HOC METHODS UNDER MNAR:

# Generate some missing data under MNAR:
dataMNAR<-datahamd
dataMNAR<-dataMNAR[order(dataMNAR$final),]
missing2<-c(rep(0,50),rbinom(50,1,0.66))
mean(missing2)
dataMNAR$final[missing2==1]<-NA
dataMNAR$R<-ifelse(is.na(dataMNAR$final),1,0)
table(dataMNAR$drug,dataMNAR$R)
table(dataMNAR$center,dataMNAR$R)

# Complete Case:
mean(dataMNAR$final,na.rm=T)
sd(dataMNAR$final,na.rm=T)
cor(dataMNAR$final,dataMNAR$baseline,use="complete.obs")
lm(dataMNAR$final~dataMNAR$baseline)
lm(dataMNAR$baseline~dataMNAR$final)

# Mean Substitution
dataMNAR2<-dataMNAR
dataMNAR2$final[is.na(dataMNAR2$final)]<-rep(mean(dataMNAR2$final,na.rm=T),sum(is.na(dataMNAR2$final)))

mean(dataMNAR2$final) 
sd(dataMNAR2$final)
cor(dataMNAR2$baseline,dataMNAR2$final)
lm(dataMNAR2$final~dataMNAR2$baseline)
lm(dataMNAR2$baseline~dataMNAR2$final)

# HotDeck
dataMNAR3<-dataMNAR
dataMNAR3<-hotdeck(data=dataMNAR3, variable="final", ord_var = "center")###sort by center before imputing


mean(dataMNAR3$final) 
sd(dataMNAR3$final)
cor(dataMNAR3$baseline,dataMNAR3$final)
lm(dataMNAR3$final~dataMNAR3$baseline)
lm(dataMNAR3$baseline~dataMNAR3$final)

# Conditional Mean
dataMNAR4<-dataMNAR
mCM<-lm(dataMNAR4$final~dataMNAR4$baseline + dataMNAR4$drug + as.factor(dataMNAR4$center))
dataMNAR4$final[is.na(dataMNAR4$final)]<-predict(mCM, data.frame(dataMNAR4))[is.na(dataMNAR4$final)]

mean(dataMNAR4$final) 
sd(dataMNAR4$final)
cor(dataMNAR4$baseline,dataMNAR4$final)
lm(dataMNAR4$final~dataMNAR4$baseline)
lm(dataMNAR4$baseline~dataMNAR4$final)

# Predictive Distribution
dataMNAR5<-dataMNAR
mPD<-lm(dataMNAR5$final~dataMNAR5$baseline + dataMNAR5$drug + as.factor(dataMNAR5$center))
dataMNAR5$final[is.na(dataMNAR5$final)]<-
  predict(mPD, data.frame(dataMNAR5))[is.na(dataMNAR5$final)]+rnorm(sum(is.na(dataMNAR5$final)),0,summary(mPD)$sigma)

mean(dataMNAR5$final) 
sd(dataMNAR5$final)
cor(dataMNAR5$baseline,dataMNAR5$final)
lm(dataMNAR5$final~dataMNAR5$baseline)
lm(dataMNAR5$baseline~dataMNAR5$final)




