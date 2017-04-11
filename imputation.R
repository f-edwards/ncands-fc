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
setwd("R:/Project/NCANDS/ncands-csv/")
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

dat.in$race<-factor(dat.in$race, levels=c("all", "ai", "blk", "wht"))
dat.in$offense<-factor(dat.in$offense)
dat.in$FIPS<-factor(dat.in$FIPS)

### calculate variance of perfectly measured police reporting data, treat missing proportion 
### as drawn from same distribution with count bounded at [0, nmissing]. 
### variance is heteroskedastic as a linear function of pop size for the count var (log-linear specification?)
### amelia imputation models are normal - will need to log transform things, scale variance by pop, 
### set mean scaled to pop, then think about an integer transformation that makes sense (trunc? ceiling?)

### go back to NCANDS to retrieve count of all cases. need to calculate proportion of all cases that are police
### for estimate of variance of \sum(police) / \sum(all) as proxy for 
gold<-dat.in%>%filter(offense=="all", race=="all", gender=="all", missing.rpt==0)

### to estimate: true count x^star, observed x_obs, and missing u, x^star=x_obs+u
### u = exp^\nu * n_missing
### \nu \sim N(\bar{v}, \sigma^2_v)
### \bar{v} = log(\sum{rpts_g}\sum{tot_g}) - average proportion of police reporting in all data, lognormal
### \sigma^2_v = var(log(rpts_g/all_g)) - lognormal

### this is wrong, unless I want to specify a ratio outcome variable for imputation. 
### the priors need to take the unit of the imputation (\bar{v}*n_missing, how to calc variance? scaling seems wrong)
### formula for linear scale of random variable = e(x_star)=a*e(x), var(x_star)=a^2 * var(x)

### with this setup, I've removed the heteroskeasticity. could sample nu from count rather than ratio, but this is prob ok
### set up ratio variable, impute that, then apply it to the missing count

### WONT WORK FOR RACE, NEED UNION OF MISSING BY RPT AND RACE TO GET UPPER

### setup prior mean and variance from gold standard data
v_bar<-(sum(gold$cases)/sum(gold$total.rpts))
v_sig<-var((gold$cases/gold$total.rpts))

### this is the sampler for the prior - loop over all observations
overimp.index<-with(dat.in, which(race=="all"&gender=="all"&offense=="all"&missing.rpt>0))
overimp.county.year<-cbind(dat.in[overimp.index, "FIPS"], dat.in[overimp.index, "year"])
not.oi<-which(!(1:nrow(dat.in)%in%overimp.index))


### create ratio of pol.missing/rpt.missing, 0 for complete data
### will use this to add measurement error to case count
### with u*rpt.missing - bounds it at [0,1]
## make placeholder ratio variable for imputation
dat.in$u<-dat.in$missing.rpt/dat.in$total.rpts
dat.in[not.oi,"u"]<-0
### takes the form of matrix: rowN (0=all), column, mean, sd
prior.mat<-cbind(overimp.index, 
  rep(which(colnames(dat.in)=="u"), length(overimp.index)),
    rep(v_bar, length(overimp.index)),
      rep(sqrt(v_sig), length(overimp.index)))

prior.mat<-rbind(prior.mat,
  cbind(1:nrow(dat.in), rep(which(colnames(dat.in)=="child.pov"), nrow(dat.in)),
    dat.in$child.pov, dat.in$child.pov.se))

overimp<-rbind(cbind(overimp.index, rep(which(colnames(dat.in)=="u"))),
  cbind(1:nrow(dat.in), rep(which(colnames(dat.in)=="child.pov"))))

dat.imp<-amelia(dat.in, ts="year", cs="FIPS", 
                idvars=c("county", "state", "race", "gender", "missing.rpt", "offense",
                  "pct.race.pop", "median.hh.income", "wht.infmort", "nonwht.infmort", "MURDER_mav", "stname"), polytime=2, 
  priors=prior.mat,
  logs=c("adult.pop", "child.pop", "pop.density", "officers.pc", "child.pov", "child.pov.se", "infmort", "pol.infl.pc"),
  sqrts=c("cases", "total.rpts"),
  overimp = overimp)

### THIS ISNT WORKING....

