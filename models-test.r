(list=ls())
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

offense<-c("all", "viol", "drug", "prop", "qol")
race<-c("all", "blk", "ai", "wht")
gender<-c("all", "female", "male")


### ALSO RUN COUNTY MEAN GLMMMs if I can get them to fit. Check convergence on
### Bayes' models and add addtl mean predictors for w/in models. Compare fits to 
### FE quasipoisson glm fits


####Two steps - compare convergence of desired models for restricted numbers of REs - 
####estimate separate models by race, gender, ignore offense for now
####compare to results from models with interactions/variable slopes


## FIT MODELS WITH 1/4 SAMPLE
FIPS.samp<-sample(unique(dat$FIPS), 40)
test<-dat%>%filter(FIPS%in%FIPS.samp)

runRace.glmer<-function(data, model){
  temp<-list()
  for(i in 1:length(race)){
    print(i)
    data<-data%>%filter(race==race[i])
    ptm <- proc.time()
    fit<-glmer(model, data=data, offset=log(child.pop), family=poisson, 
               control=glmerControl(optimizer = "bobyqa"), verbose=0)
    print(proc.time() - ptm)
    temp[[i]]<-fit
  }
  names(temp)<-race
  return(temp)
}

runGender.glmer<-function(data, model){
  temp<-list()
  for(i in 1:length(race)){
    print(i)
    data<-data%>%filter(race==race[i])
    ptm <- proc.time()
    fit<-glmer(model, data=data, offset=log(child.pop), family=poisson, 
               control=glmerControl(optimizer = "bobyqa"), verbose=0)
    print(proc.time() - ptm)
    temp[[i]]<-fit
  }
  names(temp)<-race
  return(temp)
}

