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

### calculate variance of perfectly measured police reporting data, treat missing proportion 
### as drawn from same distribution with count bounded at [0, nmissing]. 
### variance is heteroskedastic as a linear function of pop size for the count var (log-linear specification?)
### amelia imputation models are normal - will need to log transform things, scale variance by pop, 
### set mean scaled to pop, then think about an integer transformation that makes sense (trunc? ceiling?)

# ### go back to NCANDS to retrieve count of all cases. need to calculate proportion of all cases that are police
# ### for estimate of variance of \sum(police) / \sum(all) as proxy for 
# gold<-dat.in%>%filter(offense=="all", race=="all", gender=="all", missing.rpt==0)
# 
# ### to estimate: true count x^star, observed x_obs, and missing u, x^star=x_obs+u
# ### u = exp^\nu * n_missing
# ### \nu \sim N(\bar{v}, \sigma^2_v)
# ### \bar{v} = log(\sum{rpts_g}\sum{tot_g}) - average proportion of police reporting in all data, lognormal
# ### \sigma^2_v = var(log(rpts_g/all_g)) - lognormal
# 
# ### this is wrong, unless I want to specify a ratio outcome variable for imputation. 
# ### the priors need to take the unit of the imputation (\bar{v}*n_missing, how to calc variance? scaling seems wrong)
# ### formula for linear scale of random variable = e(x_star)=a*e(x), var(x_star)=a^2 * var(x)
# 
# ### with this setup, I've removed the heteroskeasticity. could sample nu from count rather than ratio, but this is prob ok
# ### set up ratio variable, impute that, then apply it to the missing count
# 
# ### WONT WORK FOR RACE, NEED UNION OF MISSING BY RPT AND RACE TO GET UPPER
# 
# ### setup prior mean and variance from gold standard data
# v_bar<-(sum(gold$cases)/sum(gold$total.rpts))
# v_sig<-var((gold$cases/gold$total.rpts))
# 
# ### this is the sampler for the prior - loop over all observations
# overimp.index<-with(dat.in, which(race=="all"&gender=="all"&offense=="all"&missing.rpt>0))
# overimp.county.year<-cbind(dat.in[overimp.index, "FIPS"], dat.in[overimp.index, "year"])
# not.oi<-which(!(1:nrow(dat.in)%in%overimp.index))


### create ratio of pol.missing/rpt.missing, 0 for complete data
### will use this to add measurement error to case count
### with u*rpt.missing - bounds it at [0,1]
## make placeholder ratio variable for imputation
# dat.in$u<-dat.in$missing.rpt/dat.in$total.rpts
# dat.in[not.oi,"u"]<-0
# ### takes the form of matrix: rowN (0=all), column, mean, sd
# prior.mat<-cbind(overimp.index, 
#   rep(which(colnames(dat.in)=="u"), length(overimp.index)),
#     rep(v_bar, length(overimp.index)),
#       rep(sqrt(v_sig), length(overimp.index)))

#### for each child poverty measure
prior.mat<-cbind(1:nrow(dat), rep(which(names(dat)=="child.pov"), nrow(dat)),
  dat$child.pov, dat$child.pov.se)

prior.mat<-rbind(prior.mat,
  cbind(1:nrow(dat), rep(which(names(dat)=="blk.chpov_pe"), nrow(dat)),
  dat$blk.chpov_pe, dat$blk.chpov_se))

prior.mat<-rbind(prior.mat,
  cbind(1:nrow(dat), rep(which(names(dat)=="ai.chpov_pe"), nrow(dat)),
  dat$ai.chpov_pe, dat$ai.chpov_se))

overimp<-rbind(cbind(1:nrow(dat), rep(which(colnames(dat)=="child.pov"))),
  cbind(1:nrow(dat), rep(which(colnames(dat)=="blk.chpov_pe"))),
  cbind(1:nrow(dat), rep(which(colnames(dat)=="ai.chpov_pe"))))

logs<-c("median.hh.income")
## sqrt for all counts
counts<-c(which(names(dat)=="polrpt"), which(names(dat)=="NArpt.wht"))
sqrts<-names(dat)[counts[1]:counts[2]]
counts<-c(which(names(dat)=="child"), which(names(dat)=="pop.density"))
sqrts<-c(sqrts, names(dat[counts[1]:counts[2]]))

### this works - can't really figure out why the viol measures are problematic though

dat.imp<-amelia(dat, ts="year", cs="FIPS", 
                idvars=c("county", "state", "stname", "n_obs", "adult",
                  "qol.male","qol.female", "qol.ai", "qol.blk", "qol.wht", "qol.all"), 
splinetime=2, 
  priors=prior.mat, overimp = overimp,
  sqrts = sqrts, logs=logs, p2s=2, m=5, empri=0.01*nrow(dat))

#### FOR LATER - THIS IS ADEQUATE FOR MISSING DATA AND POVERTY ERROR
#### THINK ABOUT HOW TO SET UP MEASUREMENT ERROR PROBLEM FOR MISSING RPT AND RACE
#### NEED TO TALK MORE WITH ARCHIVISTS TO THINK ABOUT WHETHER POL RPTS ARE LIKELY TO BE MISSING
#### RACE IMPUTATION WOULD BE SIMPLER
