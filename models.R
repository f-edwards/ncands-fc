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
library(rstan)
cores<-6

set.seed(1)
setwd("R:/Project/NCANDS/ncands-fc/")
dat.in<-read.csv("R:/Project/NCANDS/ncands-csv/ncands-fc-merge.csv")

### to filter for ACS 5-year availability
dat.in<-dat.in%>%filter(year>2005)

### filter for child pop >50000, >1k censoring, 5th percentile reporting rate of 0.002, this should avoid problems with censoring
dat.in<-dat.in%>%filter(child>50000)

## PA ONLY INCLUDES SUBSTANTIATED CASES: MUST DROP
dat.in<-dat.in%>%filter(!(stname=="PA"))

###NYC IS SUPER WEIRD, drop the city counties for now
nyc<-c("36005","36047", "36061", "36081", "36085")
dat.in<-dat.in%>%filter(!(FIPS%in%nyc))

### drop unused variables


### IF I'M GOING TO DO IMPUTATION, DO IT BEFORE TRANSFORMATIONS
dat<-dat.in
dat$FIPS<-factor(dat$FIPS)
dat$stname<-factor(dat$stname)
dat$n_obs<-1:nrow(dat)

###don't have race/poverty data for 04, 05

### dropping counties missing more than 40 percent of reportsource or race in ncands
error.index1<-which((dat$NArpt/dat$totalrpt)>0.4)
error.index2<-which((dat$totalrpt.missing/dat$totalrpt)>0.4)
error<-union(error.index1, error.index2)

dat<-dat[-error,]

##### missing data imputation and overimputation for poverty with known error
#source("R:/Project/NCANDS/ncands-fc/imputation.R", verbose=TRUE)

##### transform imputed variables
predictors<-c("child.pov", "median.hh.income", "blk.chpov_pe", "ai.chpov_pe", "wht.chpov_pe", "infmort",
  "wht.infmort", "nonwht.infmort", "arrest.female", "arrest.male", "drug.female", "drug.male", "qol.female",
  "qol.male", "viol.female", "viol.male", "arrest.ai", "arrest.all", "arrest.blk", "arrest.wht", "drug.ai",
  "drug.all", "drug.blk", "drug.wht", "qol.ai", "qol.all", "qol.blk", "qol.wht", "viol.ai", "viol.all",
  "viol.blk", "viol.wht", "officers", "MURDER_mav", "pol.infl.pc")

makeMeanDiff<-function(x, vars){
  for(i in 1:length(vars)){
    t<-which(names(x)==vars[i])
    tempMean<-x%>%group_by(FIPS)%>%summarise_at(t, mean)
    names(tempMean)[2]<-paste("mean.", vars[i], sep="")
    x<-left_join(x, tempMean)
    tempDiff<-x[,t]-x[,ncol(x)]
    x<-cbind(x, tempDiff)
    names(x)[ncol(x)]<-paste("diff.", vars[i], sep="")
  }
  return(x)
}



dat<-dat%>%mutate(arrest.female=arrest.female/women, arrest.male=arrest.male/men,
  arrest.ai=arrest.ai/adult.ai, arrest.blk=arrest.blk/adult.blk, arrest.wht=arrest.wht/adult.wht, 
  drug.female=drug.female/women, drug.male=drug.male/men, drug.ai=drug.ai/adult.ai, drug.all=drug.all/adult,
  drug.blk=drug.blk/adult.blk, drug.wht=drug.wht/adult.wht, qol.ai=qol.ai/adult.ai, qol.all=qol.all/adult,
  qol.female=qol.female/women, qol.male=qol.male/men, qol.blk=qol.blk/adult.blk, qol.wht=qol.wht/adult.wht,
  viol.ai=viol.ai/adult.ai, viol.blk=viol.blk/adult.blk, viol.wht=viol.wht/adult.wht, viol.all=viol.all/adult, 
  viol.male=viol.male/men, viol.female=viol.female/women, officers=officers/(adult+child), MURDER_mav=MURDER_mav/adult,
  pct.blk=(adult.blk+child.blk)/(adult+child), pct.ai=(adult.ai+child.ai)/(adult+child))

dat<-makeMeanDiff(dat, predictors)


### for no imputations, drop se measures
### thoughts about imputation. Could set cases with min=observed, max=observed+missing, and then set priors 
### for mean and variance that pull toward the observed. would just need to confine it to integers, set min-max
### and decide on a prior function. Could vary those priors for sensitivity. i can use the fit parameter of stanfit objects after compiling the model to re-run with different data
### also check how my parameter priors are affecting inference by playing with priors in b.all.all

### create county mean variables and county mean centered

####################################################################
# Models
####################################################################

### FOR FULLY ELABORATED MODEL NEED TO CREATE FIPS MEAN VARIABLES AND FIPS-MEAN-CENTERED MEASURES

within<-polrpt~scale(diff.arrest.all)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.all)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)


b.all.all<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")


within<-polrpt~scale(diff.arrest.male)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.men.all<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.arrest.female)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.women.all<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.arrest.wht)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.wht.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.wht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.wht)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.wht.chpov_pe)+scale(mean.wht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.white<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.arrest.blk)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.black<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.arrest.ai)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.ai.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.ai)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.ai.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.ai)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.amind<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

#############################################################################
## Drug arrest models
#############################################################################

within<-polrpt~scale(diff.drug.all)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.drug.all)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.drug<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.arrest.male)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.men.drug<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)


within<-polrpt~scale(diff.arrest.female)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)


b.women.drug<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.drug.blk)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.drug.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.drug.black<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.drug.ai)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.ai.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.drug.ai)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.ai.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.ai)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.drug.amind<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

# #################################################
# ## Violent arrest models
# #################################################

within<-polrpt~scale(diff.viol.all)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.viol.all)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.viol<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.arrest.male)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.men.viol<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)


within<-polrpt~scale(diff.arrest.female)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)


b.women.viol<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.viol.blk)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.viol.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.viol.black<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.viol.ai)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.ai.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.viol.ai)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.ai.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.ai)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.viol.amind<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

# ################################################
# ## QoL arrests
# ################################################

within<-polrpt~scale(diff.qol.all)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.qol.all)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.qol<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.arrest.male)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.men.qol<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)


within<-polrpt~scale(diff.arrest.female)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)


b.women.qol<-stan_glmer(formula=within,
                       data=dat,
                       offset=log(child), family=poisson,
                       verbose=1, cores=cores,
                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                       chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.qol.blk)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.qol.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.qol.black<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")

within<-polrpt~scale(diff.qol.ai)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.ai.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.qol.ai)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.ai.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.ai)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.qol.amind<-stan_glmer(formula=within, 
                      data=dat,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)

save.image(file="models.RData")
