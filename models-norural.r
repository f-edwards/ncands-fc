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
dat.in<-read.csv("R:/Project/NCANDS/ncands-csv/ncands-fc-merge.csv")
dat.in<-dat.in%>%filter(year>2005)

dat.rural<-dat.in%>%filter(gender=="all")%>%filter(race=="all")%>%filter(child.pop<50000)
rural.index<-unique(dat.rural$FIPS)
dat.in<-dat.in%>%filter(!(FIPS%in%rural.index))

#### for models excluding cases with small pops, drop those cases at threshold of inclusion - 
#### 0.05 quantile of cases pc is 0.02, would take population of 50000 to be included in data at low rpt rate
#### drop all places with total child pops < 50000
# 
# test<-amelia(dat.in, ts="year", cs=c("FIPS"), 
#              idvars=c("median.hh.income", "pct.race.pop", "state", "county", "stname"),
#              noms=c("race", "gender", "offense"), intercs=1)

### ID PROBLEM COUNTIES, where upperbound on CJ reports is more than 5 percent larger than reported count
# error.index<-which(with(dat, (pol.rpts.upper/pol.rpts)>1.05))
# missing.conservative<-dat[error.index,]

### Drop counties with more than 10 percent of cases missing
### also lots of missing on race


### IF I'M GOING TO DO IMPUTATION, DO IT BEFORE TRANSFORMATIONS

dat<-dat.in%>%mutate(arrest.rt=arrest/(adult.pop))
dat$race<-factor(dat$race, levels=c("all", "ai", "blk", "wht"))
dat$offense<-factor(dat$offense)
dat$FIPS<-factor(dat$FIPS)
dat$n_obs<-1:nrow(dat)

###don't have race/poverty data for 04, 05


error.index1<-which((dat$missing.rpt/dat$cases)>0.5)
error.index2<-which((dat[dat$race!="all", "missing.race"]/dat[dat$race!="all", "cases"]>0.5)==TRUE)
error<-union(error.index1, error.index2)

### for now, drop all cases where missing.race or missing.rpt would create upper bound >1.1 x observed

dat<-dat[-error,]
##drop missing in UCR, deal with imputation later
dat<-dat%>%filter(!(is.na(arrest)))

### THIS IS A VERY CONSERVATIVE APPROACH - DOING OTHERWISE WOULD TAKE SOME FANCY IMPUTATION - i.e. imputing the missingness
### as an additive error

### create county mean variables and county mean centered

within.dat<-dat
within.dat<-within.dat%>%group_by(FIPS)%>%
  mutate(mean.murder.pc=mean(murder.pc, na.rm=TRUE), mean.officers.pc=mean(officers.pc, na.rm=TRUE),
         mean.infmort=mean(infmort, na.rm=TRUE), mean.wht.infmort=mean(wht.infmort, na.rm=TRUE), 
         mean.nonwht.infmort=mean(nonwht.infmort, na.rm=TRUE),
         mean.pop.density=mean(pop.density, na.rm=TRUE),
         mean.pol.infl.pc=mean(pol.infl.pc, na.rm=TRUE))%>%ungroup
within.dat<-within.dat%>%group_by(FIPS, gender, race, offense)%>%mutate(mean.arrest.rt=mean(arrest.rt, na.rm=TRUE))%>%ungroup
within.dat<-within.dat%>%group_by(FIPS, race)%>%mutate(mean.child.pov.pc=mean(child.pov/child.pop, na.rm=TRUE),
                                                       mean.pct.race.pop=mean(pct.race.pop))%>%ungroup
within.dat<-within.dat%>%
  mutate(diff.murder.pc=murder.pc-mean.murder.pc, 
         diff.officers.pc=officers.pc-mean.officers.pc,
         diff.infmort=infmort-mean.infmort, diff.wht.infmort=wht.infmort-mean.wht.infmort,
         diff.nonwht.infmort=nonwht.infmort-mean.nonwht.infmort, 
         diff.arrest.rt=arrest.rt-mean.arrest.rt, 
         diff.child.pov.pc=(child.pov/child.pop)-mean.child.pov.pc,
         diff.pop.density=pop.density-mean.pop.density,
         diff.pol.infl.pc=pol.infl.pc-mean.pol.infl.pc,
         diff.pct.race.pop=mean.pct.race.pop-pct.race.pop)

### results in losing ~ 1/3 of the data - need to think about imputation possibilities

### restrict to large child pop for now for testing - drops all native am :
#within.dat<-within.dat%>%filter(child.pop>100000)

####################################################################
# Models
####################################################################

### Model taxonomy with pol.rpts and arrest.all.tot.pc

# train<-dat%>%filter(year<2012)
# 
##running only on all for figuring out fitting
#, "viol", "drug", "prop", "qol")


# test<-dat%>%filter(year==2012)

### ALSO RUN COUNTY MEAN GLMMMs if I can get them to fit. Check convergence on
### Bayes' models and add addtl mean predictors for w/in models. Compare fits to 
### FE quasipoisson glm fits


## FIT MODELS WITH 1/4 SAMPLE
# FIPS.samp<-sample(unique(test$FIPS), 40)
# train<-train%>%filter(FIPS%in%FIPS.samp)
# test<-test%>%filter(FIPS%in%FIPS.samp)
# 
# gender.train<-train%>%filter(gender=="male"|gender=="female")
# gender.train<-gender.train[!(is.na(gender.train$offense)),]
# gender.test<-train%>%filter(gender=="male"|gender=="female")
# gender.test<-gender.test[!(is.na(gender.test$offense)),]
# 
# race.train<-train%>%filter(gender=="all")
# race.train<-race.train[!(is.na(race.train$offense)),]
# race.test<-test%>%filter(gender=="all")
# race.test<-race.test[!(is.na(race.test$offense)),]
# 
# all.train<-train%>%filter(race=="all"&gender=="all")
# all.train<-all.train[!(is.na(all.train$offense)),]
# all.test<-train%>%filter(race=="all"&gender=="all")
# all.test<-all.test[!(is.na(all.test$offense)),]


### NO POOLING - WITHIN MODEL

runOffense.glm<-function(data, model){
  temp<-list()
  for(i in 1:length(offense)){
    print(i)
    data<-data%>%filter(offense==offense[i])
    ptm <- proc.time()
    fit<-glm(model, data=data, offset=log(child.pop), family=quasipoisson)
    proc.time() - ptm
    temp[[i]]<-fit
  }
  names(temp)<-offense
  return(temp)
}


### GLMM - PARTIAL POOLING - OVERDISPERSED
runOffense.glmer<-function(data, model){
  temp<-list()
  for(i in 1:length(offense)){
    off.cat<-offense[i]
    print(off.cat)
    off.data<-data%>%filter(offense==off.cat)
    ptm <- proc.time()
    fit<-glmer(model, data=off.data, offset=log(child.pop), family=poisson, 
               control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=20000)), verbose=0)
    print(proc.time() - ptm)
    temp[[i]]<-fit
  }
  names(temp)<-offense
  return(temp)
}

# t1.re<-runOffense.lmer(model=p1.race, data=race.dat)
# t1.int<-runOffense.glmer(model=p1.race.int, data=race.dat)

#Compare fits for models for arrest=1, year=0
# wht.re.b0<-fixef(t1.re[[1]])[1]+ranef(t1.re[[1]])$race[3,1]
# wht.re.b1<-fixef(t1.re[[1]])[2]+ranef(t1.re[[1]])$race[3, 2]
# 
# wht.int.b0<-fixef(t1.int[[1]])[1]+fixef(t1.int[[1]])[4]
# wht.int.b1<-fixef(t1.int[[1]])[2]+fixef(t1.int[[1]])[7]

# race.int<-cases~scale(arrest.rt)*race+
#   scale(officers.pc)*race+
#   scale(I(child.pov/child.pop))+scale(pop.density)+
#   scale(year)+scale(murder.pc)+
#   scale(nonwht.infmort)+scale(wht.infmort)+
#   (1|FIPS)+(1|state)+(1|n_obs)

### FOR FULLY ELABORATED MODEL NEED TO CREATE FIPS MEAN VARIABLES AND FIPS-MEAN-CENTERED MEASURES


# race.within<-cases~scale(diff.arrest.rt)*race+
#   offense*race+
#   scale(diff.officers.pc)*race+
#   scale(diff.pol.infl.pc)+
#   race+
#   scale(diff.child.pov.pc)+scale(diff.pop.density)+
#   scale(year)+scale(diff.murder.pc)+scale(diff.pct.race.pop)*race+
#   scale(diff.infmort)+
#   scale(mean.arrest.rt)*race+offense*race+
#   scale(mean.officers.pc)*race+
#   scale(mean.pol.infl.pc)+
#   scale(mean.murder.pc)+scale(mean.pop.density)+
#   scale(mean.infmort)+
#   scale(mean.child.pov.pc)+scale(mean.pct.race.pop)*race+
#   (1|state/FIPS)+(1|n_obs)

within.fe<-cases~arrest.rt+
#+officers.pc+pol.infl.pc+
  #I(child.pov/child.pop)+pop.density+year+murder.pc+infmort+
    FIPS

fe.all<-glm.nb(cases~arrest.rt+officers.pc+pol.infl.pc+I(child.pov/child.pop)+pop.density+year+murder.pc+infmort+FIPS, 
               data=within.dat%>%filter(race=="ai")%>%filter(gender=="all")%>%filter(offense=="all"))

within<-cases~scale(diff.arrest.rt)+
  scale(diff.officers.pc)+
  scale(diff.pol.infl.pc)+
  scale(diff.child.pov.pc)+scale(diff.pop.density)+
  scale(year)+scale(diff.murder.pc)+scale(diff.infmort)+
  scale(mean.arrest.rt)+scale(mean.officers.pc)+
  scale(mean.pol.infl.pc)+scale(mean.pop.density)+
  scale(mean.murder.pc)+
  scale(mean.child.pov.pc)+scale(mean.infmort)+
  (1|state/FIPS)+(1|n_obs)

within.wht<-cases~scale(diff.arrest.rt)+
  scale(diff.officers.pc)+
  scale(diff.pol.infl.pc)+
  scale(diff.child.pov.pc)+scale(diff.pop.density)+scale(diff.pct.race.pop)+
  scale(year)+scale(diff.murder.pc)+scale(diff.infmort)+
  scale(mean.arrest.rt)+scale(mean.officers.pc)+
  scale(mean.pol.infl.pc)+scale(mean.pop.density)+
  scale(mean.murder.pc)+scale(mean.pct.race.pop)+
  scale(mean.child.pov.pc)+scale(mean.wht.infmort)+
  (1|state/FIPS)+(1|n_obs)

within.nonwht<-cases~scale(diff.arrest.rt)+
  scale(diff.officers.pc)+
  scale(diff.pol.infl.pc)+
  scale(diff.child.pov.pc)+scale(diff.pop.density)+scale(diff.pct.race.pop)+
  scale(year)+scale(diff.murder.pc)+scale(diff.infmort)+
  scale(mean.arrest.rt)+scale(mean.officers.pc)+
  scale(mean.pol.infl.pc)+scale(mean.pop.density)+
  scale(mean.murder.pc)+scale(mean.pct.race.pop)+
  scale(mean.child.pov.pc)+scale(mean.nonwht.infmort)+
  (1|state/FIPS)+(1|n_obs)

# gender.within<-cases~scale(diff.arrest.rt)*gender+offense*gender+
#   scale(diff.officers.pc)+
#   scale(diff.pol.infl.pc)+
#   gender+
#   scale(diff.child.pov.pc)+scale(diff.pop.density)+
#   scale(year)+scale(diff.murder.pc)+scale(diff.infmort)+
#   scale(mean.arrest.rt)*gender*offense+scale(mean.officers.pc)+
#   scale(mean.pol.infl.pc)+
#   scale(mean.murder.pc)+scale(mean.infmort)+
#   scale(mean.child.pov.pc)+scale(mean.pop.density)+
#   (1|state/FIPS)+(1|n_obs)

# t3.re<-runOffense.glmer(race.dat, race)
# t3.int<-runOffense.glmer(race.train, race.int)

# m.race.within<-runOffense.glmer(race.dat, race.within)
# # m.race.within.int.arrest<-runOffense.glmer(race.dat, race.within.int.arrest)
# # m.race.within.int.officers<-runOffense.glmer(race.dat, race.within.int.officers)
# 
# m.all.within<-runOffense.glmer(all.dat, all.within)
# 

#### THE INTERACTION MODELS ARE RIDICULOUS. START SIMPLE - ALL ARRESTS, ALL REPORTS
#### THEN RUN SEPARATE MODELS FOR OFFENSE, RACE, GENDER - SUBSET RACE AND GENDER IF 
#### IT HELPS - FOCUS ON ALL, TAKE ON DRUGS AS AN ADDTL SENSITIVITY TEST


b.all.all<-stan_glmer(formula=within, 
                      data=within.dat%>%filter(offense=="all")%>%filter(race=="all")%>%filter(gender=="all"),
                      offset=log(child.pop), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models-within-norural.RData")

# f.men.all<-glmer(within, 
#                  data=within.dat%>%filter(offense=="all")%>%filter(race=="all")%>%filter(gender=="male"), 
#                  offset=log(child.pop), family=poisson, 
#                   control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=20000)))

save.image(file="models-within-norural.RData")

b.men.all<-stan_glmer(formula=within, 
                      data=within.dat%>%filter(offense=="all")%>%filter(gender=="male"), 
                      offset=log(child.pop), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.women.all<-stan_glmer(formula=within, 
                        data=within.dat%>%filter(offense=="all")%>%filter(gender=="female"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.all.white<-stan_glmer(formula=within.wht, 
                        data=within.dat%>%filter(offense=="all")%>%filter(race=="wht"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.all.black<-stan_glmer(formula=within.nonwht, 
                        data=within.dat%>%filter(offense=="all")%>%filter(race=="blk"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.all.amind<-stan_glmer(formula=within.nonwht, 
                        data=within.dat%>%filter(offense=="all")%>%filter(race=="ai"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

save.image(file="models-within-norural.RData")


#############################################################################
## Drug arrest models
#############################################################################

b.all.drug<-stan_glmer(formula=within, 
                       data=within.dat%>%filter(offense=="drug")%>%filter(race=="all")%>%filter(gender=="all"),
                       offset=log(child.pop), family=poisson, 
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models-within-norural.RData")

# f.men.all<-glmer(within, 
#                  data=within.dat%>%filter(offense=="all")%>%filter(race=="all")%>%filter(gender=="male"), 
#                  offset=log(child.pop), family=poisson, 
#                   control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=20000)))


b.white.drug<-stan_glmer(formula=within.wht, 
                         data=within.dat%>%filter(offense=="drug")%>%filter(race=="wht"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.black.drug<-stan_glmer(formula=within.nonwht, 
                         data=within.dat%>%filter(offense=="drug")%>%filter(race=="blk"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.amind.drug<-stan_glmer(formula=within.nonwht, 
                         data=within.dat%>%filter(offense=="drug")%>%filter(race=="ai"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

b.men.drug<-stan_glmer(formula=within, 
                       data=within.dat%>%filter(offense=="drug")%>%filter(gender=="male"), 
                       offset=log(child.pop), family=poisson, 
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.women.drug<-stan_glmer(formula=within, 
                         data=within.dat%>%filter(offense=="drug")%>%filter(gender=="female"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

#################################################
## Violent arrest models
#################################################


b.all.viol<-stan_glmer(formula=within, 
                       data=within.dat%>%filter(offense=="viol")%>%filter(race=="all")%>%filter(gender=="all"),
                       offset=log(child.pop), family=poisson, 
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models-within-norural.RData")


b.white.viol<-stan_glmer(formula=within.wht, 
                         data=within.dat%>%filter(offense=="viol")%>%filter(race=="wht"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.black.viol<-stan_glmer(formula=within.nonwht, 
                         data=within.dat%>%filter(offense=="viol")%>%filter(race=="blk"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.amind.viol<-stan_glmer(formula=within.nonwht, 
                         data=within.dat%>%filter(offense=="viol")%>%filter(race=="ai"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

b.men.viol<-stan_glmer(formula=within, 
                       data=within.dat%>%filter(offense=="viol")%>%filter(gender=="male"), 
                       offset=log(child.pop), family=poisson, 
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.women.viol<-stan_glmer(formula=within, 
                         data=within.dat%>%filter(offense=="viol")%>%filter(gender=="female"), 
                         offset=log(child.pop), family=poisson, 
                         verbose=1, cores=cores,
                         prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                         chains=4, iter=1000)

################################################
## QoL arrests
################################################


b.all.qol<-stan_glmer(formula=within, 
                      data=within.dat%>%filter(offense=="qol")%>%filter(race=="all")%>%filter(gender=="all"),
                      offset=log(child.pop), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models-within-norural.RData")


b.white.qol<-stan_glmer(formula=within.wht, 
                        data=within.dat%>%filter(offense=="qol")%>%filter(race=="wht"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.black.qol<-stan_glmer(formula=within.nonwht, 
                        data=within.dat%>%filter(offense=="qol")%>%filter(race=="blk"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.amind.qol<-stan_glmer(formula=within.nonwht, 
                        data=within.dat%>%filter(offense=="qol")%>%filter(race=="ai"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

b.men.qol<-stan_glmer(formula=within, 
                      data=within.dat%>%filter(offense=="qol")%>%filter(gender=="male"), 
                      offset=log(child.pop), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models-within-norural.RData")

b.women.qol<-stan_glmer(formula=within, 
                        data=within.dat%>%filter(offense=="qol")%>%filter(gender=="female"), 
                        offset=log(child.pop), family=poisson, 
                        verbose=1, cores=cores,
                        prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                        chains=4, iter=1000)

save.image(file="models-within-norural.RData")
