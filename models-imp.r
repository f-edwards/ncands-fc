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
cores<-4

set.seed(1)
setwd("D:/sync/ncands-fc/")
dat.in<-read.csv("D:/sync/ncands-csv/ncands-fc-merge.csv")

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
ptm<-proc.time()
##### missing data imputation and overimputation for poverty with known error
source("D:/sync/ncands-fc/imputation.R", verbose=TRUE)
proc.time()-ptm
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


for(j in 1:10){

  dat.imp$imputations[[j]]<-dat.imp$imputations[[j]]%>%mutate(arrest.female=arrest.female/women, arrest.male=arrest.male/men,
    arrest.ai=arrest.ai/adult.ai, arrest.blk=arrest.blk/adult.blk, arrest.wht=arrest.wht/adult.wht, 
    drug.female=drug.female/women, drug.male=drug.male/men, drug.ai=drug.ai/adult.ai, drug.all=drug.all/adult,
    drug.blk=drug.blk/adult.blk, drug.wht=drug.wht/adult.wht, qol.ai=qol.ai/adult.ai, qol.all=qol.all/adult,
    qol.female=qol.female/women, qol.male=qol.male/men, qol.blk=qol.blk/adult.blk, qol.wht=qol.wht/adult.wht,
    viol.ai=viol.ai/adult.ai, viol.blk=viol.blk/adult.blk, viol.wht=viol.wht/adult.wht, viol.all=viol.all/adult, 
    viol.male=viol.male/men, viol.female=viol.female/women, officers=officers/(adult+child), MURDER_mav=MURDER_mav/adult,
    pct.blk=(adult.blk+child.blk)/(adult+child), pct.ai=(adult.ai+child.ai)/(adult+child))
  
  dat.imp$imputations[[j]]<-makeMeanDiff(dat.imp$imputations[[j]], predictors)

}
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


b.all.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")


within<-polrpt~scale(diff.arrest.male)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.men.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.female)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.women.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.wht)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.wht.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.wht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.wht)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.wht.chpov_pe)+scale(mean.wht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.white<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.blk)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.black<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

# within<-polrpt~scale(diff.arrest.ai)+
#   scale(diff.officers)+scale(diff.pol.infl.pc)+
#   scale(diff.ai.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
#   scale(mean.arrest.ai)+scale(mean.officers)+scale(mean.pol.infl.pc)+
#   scale(mean.MURDER_mav)+scale(mean.ai.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
#   scale(pop.density)+scale(pct.ai)+scale(pct.ai)+scale(year)+
#   (1|state)+(1|FIPS)+(1|n_obs)

# b.all.amind<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
#                       data=x,
#                       offset=log(child), family=poisson, 
#                       verbose=1, cores=cores,
#                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
#                       chains=4, iter=1000)})

save.image(file="models-imp.RData")

#############################################################################
## Drug arrest models
#############################################################################


within<-polrpt~scale(diff.arrest.all)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.all)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)


b.all.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")


within<-polrpt~scale(diff.arrest.male)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.men.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.female)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.women.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.wht)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.wht.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.wht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.wht)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.wht.chpov_pe)+scale(mean.wht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.white<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.blk)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.black<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
                      data=x,
                      offset=log(child), family=poisson, 
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")
 
#################################################
## Violent arrest models
#################################################



within<-polrpt~scale(diff.arrest.all)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.all)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)


b.all.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within,
                      data=x,
                      offset=log(child), family=poisson,
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")


within<-polrpt~scale(diff.arrest.male)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.men.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within,
                      data=x,
                      offset=log(child), family=poisson,
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.female)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.women.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within,
                      data=x,
                      offset=log(child), family=poisson,
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.wht)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.wht.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.wht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.wht)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.wht.chpov_pe)+scale(mean.wht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.white<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within,
                      data=x,
                      offset=log(child), family=poisson,
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

within<-polrpt~scale(diff.arrest.blk)+
  scale(diff.officers)+scale(diff.pol.infl.pc)+
  scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
  scale(mean.arrest.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
  scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
  scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
  (1|state)+(1|FIPS)+(1|n_obs)

b.all.black<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within,
                      data=x,
                      offset=log(child), family=poisson,
                      verbose=1, cores=cores,
                      prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
                      chains=4, iter=1000)})

save.image(file="models-imp.RData")

# ################################################
# ## QoL arrests
# ################################################
# 
# 
# 
# within<-polrpt~scale(diff.arrest.all)+
#   scale(diff.officers)+scale(diff.pol.infl.pc)+
#   scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
#   scale(mean.arrest.all)+scale(mean.officers)+scale(mean.pol.infl.pc)+
#   scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
#   scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
#   (1|state)+(1|FIPS)+(1|n_obs)
# 
# 
# b.all.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
#                       data=x,
#                       offset=log(child), family=poisson, 
#                       verbose=1, cores=cores,
#                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
#                       chains=4, iter=1000)})
# 
# save.image(file="models-imp.RData")
# 
# 
# within<-polrpt~scale(diff.arrest.male)+
#   scale(diff.officers)+scale(diff.pol.infl.pc)+
#   scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
#   scale(mean.arrest.male)+scale(mean.officers)+scale(mean.pol.infl.pc)+
#   scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
#   scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
#   (1|state)+(1|FIPS)+(1|n_obs)
# 
# b.men.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
#                       data=x,
#                       offset=log(child), family=poisson, 
#                       verbose=1, cores=cores,
#                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
#                       chains=4, iter=1000)})
# 
# save.image(file="models-imp.RData")
# 
# within<-polrpt~scale(diff.arrest.female)+
#   scale(diff.officers)+scale(diff.pol.infl.pc)+
#   scale(diff.child.pov)+scale(diff.MURDER_mav)+scale(diff.infmort)+scale(diff.median.hh.income)+
#   scale(mean.arrest.female)+scale(mean.officers)+scale(mean.pol.infl.pc)+
#   scale(mean.MURDER_mav)+scale(mean.child.pov)+scale(mean.infmort)+scale(mean.median.hh.income)+
#   scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
#   (1|state)+(1|FIPS)+(1|n_obs)
# 
# b.women.all<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
#                       data=x,
#                       offset=log(child), family=poisson, 
#                       verbose=1, cores=cores,
#                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
#                       chains=4, iter=1000)})
# 
# save.image(file="models-imp.RData")
# 
# within<-polrpt~scale(diff.arrest.wht)+
#   scale(diff.officers)+scale(diff.pol.infl.pc)+
#   scale(diff.wht.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.wht.infmort)+scale(diff.median.hh.income)+
#   scale(mean.arrest.wht)+scale(mean.officers)+scale(mean.pol.infl.pc)+
#   scale(mean.MURDER_mav)+scale(mean.wht.chpov_pe)+scale(mean.wht.infmort)+scale(mean.median.hh.income)+
#   scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
#   (1|state)+(1|FIPS)+(1|n_obs)
# 
# b.all.white<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
#                       data=x,
#                       offset=log(child), family=poisson, 
#                       verbose=1, cores=cores,
#                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
#                       chains=4, iter=1000)})
# 
# save.image(file="models-imp.RData")
# 
# within<-polrpt~scale(diff.arrest.blk)+
#   scale(diff.officers)+scale(diff.pol.infl.pc)+
#   scale(diff.blk.chpov_pe)+scale(diff.MURDER_mav)+scale(diff.nonwht.infmort)+scale(diff.median.hh.income)+
#   scale(mean.arrest.blk)+scale(mean.officers)+scale(mean.pol.infl.pc)+
#   scale(mean.MURDER_mav)+scale(mean.blk.chpov_pe)+scale(mean.nonwht.infmort)+scale(mean.median.hh.income)+
#   scale(pop.density)+scale(pct.blk)+scale(pct.ai)+scale(year)+
#   (1|state)+(1|FIPS)+(1|n_obs)
# 
# b.all.black<-lapply(dat.imp$imputations, function(x){stan_glmer(formula=within, 
#                       data=x,
#                       offset=log(child), family=poisson, 
#                       verbose=1, cores=cores,
#                       prior=normal(0, 1),  prior_intercept = normal(0, 3), prior_covariance = decov(1,1,1,1),
#                       chains=4, iter=1000)})
# 
# save.image(file="models-imp.RData")