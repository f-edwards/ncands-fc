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
### why do I do this again? because of the ACS? Maybe drop it? is there another way to do poverty?
### could run all years w/o poverty, but that's skectchy
dat.in<-dat.in%>%filter(year>2005)
dat.in<-dat.in%>%filter(!(is.na(cases)))

dat.rural<-dat.in%>%filter(gender=="all")%>%filter(race=="all")%>%filter(child.pop<50000)
rural.index<-unique(dat.rural$FIPS)
dat.in<-dat.in%>%filter(!(FIPS%in%rural.index))

## PA ONLY INCLUDES SUBSTANTIATED CASES: MUST DROP
dat.in<-dat.in%>%filter(!(stname=="PA"))

###NYC IS SUPER WEIRD, drop the city counties for now
nyc<-c("36005","36047", "36061", "36081", "36085")
dat.in<-dat.in%>%filter(!(FIPS%in%nyc))

### drop unused variables
dat.in<-dat.in%>%dplyr::select(-median.hh.income, -child.pov.se)

### getting some missing.rpt NAs from the merge
### algorithm: find all NA missing.rpt, find match FIPS, year pair, assign missing.rpt, else leave NA

#### NEED TO DO IMPUTATION FOR BOTH UCR MISSINGNESS AND FOR ERROR IN NCANDS
#### GOING TO DO A QUICK FIX WHERE I DROP ALL COUNTIES WITH ZERO REPORTED ON ALL ARRESTS
### This looks resolved by better handling UCR non-reports as NA instead of 0
# dat.test<-dat.in%>%group_by(FIPS, year)%>%filter(offense=="all")%>%filter(race=="all")%>%filter(gender=="all")%>%filter(arrest==0)
# ucr.missing<-cbind(dat.test$FIPS, dat.test$year)
# ### drop all counties that report zero total arrests
# 
# m1=setkey(data.table(ucr.missing))
# m2=setkey(data.table(cbind(dat.in$FIPS, dat.in$year)))
# drop.index<-na.omit(m2[m1, which=TRUE])
# dat.in<-dat.in[-drop.index,]
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


error.index1<-which((dat[dat$race=="all", "missing.rpt"]/dat[dat$race=="all", "cases"])>0.4)
error.index2<-which((dat[dat$race=="all", "missing.race"]/dat[dat$race=="all", "cases"]>0.4)==TRUE)
error<-union(error.index1, error.index2)

### for now, drop all cases where missing.race or missing.rpt would create upper bound >1.1 x observed

dat<-dat[-error,]
##drop missing in UCR, deal with imputation later
dat<-dat%>%filter(!(is.na(arrest)))

### THIS IS A VERY CONSERVATIVE APPROACH - DOING OTHERWISE WOULD TAKE SOME FANCY IMPUTATION - i.e. imputing the missingness
### as an additive error. 

### thoughts about imputation. Could set cases with min=observed, max=observed+missing, and then set priors 
### for mean and variance that pull toward the observed. would just need to confine it to integers, set min-max
### and decide on a prior function. Could vary those priors for sensitivity. i can use the fit parameter of stanfit objects after compiling the model to re-run with different data
### also check how my parameter priors are affecting inference by playing with priors in b.all.all

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

####################################################################
# Models
####################################################################

### FOR FULLY ELABORATED MODEL NEED TO CREATE FIPS MEAN VARIABLES AND FIPS-MEAN-CENTERED MEASURES

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
