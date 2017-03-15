rm(list=ls())
gc()

.libPaths( c( .libPaths(), "U:/R") )

library(Amelia)
library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
library(ggplot2)
library(foreign)
library(haven)
library(rstanarm)
library(shinystan)
library(MASS)
library(arm)
library(parallel)
cores<-6

set.seed(1)
setwd("R:/Project/NCANDS/ncands-fc/")
dat.in<-as.data.frame(fread("R:/Project/NCANDS/ncands-csv/ncands-fc-merge.csv"))
dat.in<-read.csv("R:/Project/NCANDS/ncands-csv/ncands-fc-merge.csv")
### ID PROBLEM COUNTIES, where upperbound on CJ reports is more than 5 percent larger than reported count
# error.index<-which(with(dat, (pol.rpts.upper/pol.rpts)>1.05))
# missing.conservative<-dat[error.index,]

### Drop counties with more than 10 percent of cases missing
### also lots of missing on race

dat<-dat.in%>%mutate(arrest.rt=arrest/(adult.pop))
dat$race<-factor(dat$race)
dat$offense<-factor(dat$offense)
dat$FIPS<-factor(dat$FIPS)
dat$n_obs<-1:nrow(dat)
dat$arrest.rt.s<-as.numeric(scale(dat$arrest.rt))


error.index1<-dat[which(with(dat, missing.rpt/cases>0.1)), "n_obs"]
error.index2<-which((dat[dat$race!="all", "missing.race"]/dat[dat$race!="all", "cases"]>0.1)==TRUE)
error.index2<-dat[dat$race!="all","n_obs"]

### for now, drop all cases where missing.race or missing.rpt would create upper bound >1.1 x observed

dat<-dat%>%filter(n_obs%in%error.index1|n_obs%in%error.index2)

### results in losing ~ 1/3 of the data - need to think about imputation possibilities


####################################################################
# Models
####################################################################

### Model taxonomy with pol.rpts and arrest.all.tot.pc

train<-dat%>%filter(year<2012)
offense<-unique(train$offense)[-1]

test<-dat%>%filter(year==2012)

### FIT MODELS WITH 1/4 SAMPLE
# FIPS.samp<-sample(unique(test$FIPS), 40)
# train<-train%>%filter(FIPS%in%FIPS.samp)
# test<-test%>%filter(FIPS%in%FIPS.samp)

gender.train<-train%>%filter(gender=="male"|gender=="female")
gender.train<-gender.train[!(is.na(gender.train$offense)),]
gender.test<-train%>%filter(gender=="male"|gender=="female")
gender.test<-gender.test[!(is.na(gender.test$offense)),]

race.train<-train%>%filter(race!="all")
race.train<-race.train[!(is.na(race.train$offense)),]
race.test<-train%>%filter(race!="all")
race.test<-race.test[!(is.na(race.test$offense)),]

all.train<-train%>%filter(race=="all"&gender=="all")
all.train<-all.train[!(is.na(all.train$offense)),]
all.test<-train%>%filter(race=="all"&gender=="all")
all.test<-all.test[!(is.na(all.test$offense)),]

gender.dat<-dat%>%filter(gender=="male"|gender=="female")
gender.dat<-gender.dat[!(is.na(gender.dat$offense)),]

race.dat<-dat%>%filter(race!="all")
race.dat<-race.dat[!(is.na(race.dat$offense)),]

all.dat<-dat%>%filter(race=="all"&gender=="all")
all.dat<-all.dat[!(is.na(all.dat$offense)),]

### Do out of sample prediction
poisson0<-glm(cases~arrest.rt*offense-1, 
              data=all.train, family=poisson(), offset=log(child.pop))

###model is y_{cy}\sim Poisson(n_{cy} exp (\gamma_i + \nu_i * arrest.rt_i))
### where y_{cy} is report counts, n_{cy} is child pop, 
###i is offense type, and \nu is the sum of the arrest and arrest interaction coefficients for offense i
yhat<-exp(predict(poisson0, newdata=all.test))
RMSE<-sum(sqrt(abs((yhat^2)-(all.test$cases^2))), na.rm=TRUE)
plot(log(x=all.test$cases), log(yhat))
abline(0,1)


###visualize no pooling models - predicted arrest relationship for each type

### lm is way overpredicting

### KEEP WORKING THROUGH THESE - WRITE OUT MODEL
### set up separate models for each offense cat, then models for race, gender. one model for race==all, gender==all, one for race!=all, and one for gender!=all


### NO POOLING - WITHIN MODEL
w1.all<-glm(cases~arrest.rt.s*offense+scale(year)+scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
              scale(pop.density)+scale(year)+scale(infmort)+scale(MURDER_mav)+factor(county), 
            offset=log(child.pop), data=all.train, family=quasipoisson(link="log"))



w1.gender<-glm(cases~arrest.rt.s*gender+arrest.rt.s*offense+scale(year)+factor(FIPS), 
               offset=log(child.pop), data=gender.train, family=quasipoisson(link="log"))
w1.race<-glm(cases~arrest.rt.s*race+arrest.rt.s*offense+scale(year)+factor(FIPS), 
             offset=log(child.pop), data=race.train, family=quasipoisson(link="log"))

### GLMM - PARTIAL POOLING - OVERDISPERSED
runOffense<-function(data, model){
  temp<-list()
  for(i in 1:length(offense)){
    print(i)
    data<-data%>%filter(offense==offense[i])
    ptm <- proc.time()
    fit<-glmer(model, data=data, offset=log(child.pop), family=poisson, 
               control=glmerControl(optimizer = "bobyqa"), verbose=TRUE)
    proc.time() - ptm
    temp[[i]]<-fit
  }
  out(temp)
}


p1.all<-cases~arrest.rt.s+scale(year)+
                (1|FIPS)+(1|n_obs)

p1.race<-cases~arrest.rt.s+scale(year)+
                 (1|FIPS)+(1|n_obs)+(arrest.rt.s|race)

p1.gender<-cases~arrest.rt.s+scale(year)+
                   (1|FIPS)+(1|n_obs)+(arrest.rt.s|race) 
                 
all.p1list<-runOffense(all.dat, p1.all)
race.p1list<-runOffense(race.dat, p1.race)
gender.p1list<-runOffense(gender.dat, p1.gender)




### RE MODELS HAVING A HARD TIME CONVERGING RELATIVE TO INTERACTION MODELS maybe hold out for bayesian for full re specs
ptm <- proc.time()
proc.time() - ptm

yhat<-exp(predict(p1.gender, newdata=gender.test))
plot(log(x=p1.gender@frame$cases), log(fitted(p1.gender)))
abline(0,1)




p2.all<-glmer(cases~arrest.rt.s+offense+
                scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
                scale(pop.density)+scale(year)+scale(infmort)+scale(MURDER_mav)+
                (1|county)+(1|n_obs)+(1|state), 
              data=all.train, family=poisson, offset=log(child.pop), control=glmerControl(optimizer = "bobyqa"))

### CHECK FIT ON TEST SET FOR EACH

p2.gender<- glmer(cases~arrest.rt.s+offense+scale(year)+
                scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
                scale(pop.density)+scale(year)+scale(infmort)+scale(MURDER_mav)+
                (1|county)+(1|state)+(1|n_obs)+(1+arrest.rt.s|gender), 
                data=gender.train, offset=log(child.pop), family=poisson, control=glmerControl(optimizer = "bobyqa"))
p2.race<-glmer(cases~arrest.rt.s+offense+scale(year)+
              scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
              scale(pop.density)+scale(year)+scale(MURDER_mav)+
              scale(nonwht.infmort)+scale(wht.infmort)+
              (1+arrest.rt.s|race)+(1|county)+(1|state)+(1|n_obs), 
              data=race.train, offset=log(child.pop), family=poisson, control=glmerControl(optimizer = "bobyqa"))



p2.gender.int<- glmer(cases~arrest.rt.s+offense+arrest.rt.s*gender+scale(year)+
                scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
                scale(pop.density)+scale(year)+scale(infmort)+scale(MURDER_mav)+
                (1|county)+(1|state)+(1|n_obs), 
                data=gender.train, offset=log(child.pop), family=poisson, control=glmerControl(optimizer = "bobyqa"))
p2.race<-glmer(cases~arrest.rt.s+offense+arrest.rt.s*race+scale(year)+
              scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
              scale(pop.density)+scale(year)+scale(MURDER_mav)+
              scale(nonwht.infmort)+scale(wht.infmort)+
              (1|county)+(1|state)+(1|n_obs), 
              data=race.train, offset=log(child.pop), family=poisson, control=glmerControl(optimizer = "bobyqa"))


p2.gender.stan<- stan_glmer(cases~arrest.rt.s+offense+scale(year)+
                scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
                scale(pop.density)+scale(year)+scale(infmort)+scale(MURDER_mav)+
                (1|county)+(1|state)+(1|n_obs)+(1+arrest.rt.s|gender), 
                data=gender.train, offset=log(child.pop), family=poisson, cores=cores,
                prior=normal(0, 2.5),  prior_intercept = normal(0, 5), prior_covariance = decov(1,1,1,1),
                chains=1, iter=100)
p2.race.stan<-stan_glmer(cases~arrest.rt.s+offense+scale(year)+
              scale(I(child.pov/child.pop))+scale(officers/adult.pop)+
              scale(pop.density)+scale(year)+scale(MURDER_mav)+
              scale(nonwht.infmort):(race!="wht")+scale(wht.infmort):(race=="wht")+
              (1+arrest.rt.s|race)+(1|county)+(1|state)+(1|n_obs), 
              data=race.train, offset=log(child.pop), family=poisson, cores=cores)


### FEELING GOOD ABOUT THESE MODELS


# 
# ### weird problem with high residuals on small fitted. not sure how to adjust for that. 
# ### go through gelman model taxonomy formally, keep all in here, run diagnostic plots, do more datavis, store it