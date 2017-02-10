rm(list=ls())

.libPaths( c( .libPaths(), "U:/R") )

library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
library(ggplot2)
library(foreign)
library(haven)
set.seed(1)
setwd("R:/Project/NCANDS/ncands-fc/")
source("R:/Project/NCANDS/ncands-fc/read-main.r")

### ID PROBLEM COUNTIES, where upperbound on CJ reports is more than 5 percent larger than reported count
error.index<-which(with(dat, (pol.rpts.upper/pol.rpts)>1.05))
missing.conservative<-dat[error.index,]

### Drop counties with more than 5 percent of cases missing
error.index1<-which(with(dat, missing.rptsrc/tot.reports>0.05))
missing.liberal<-dat[error.index1,]

### drop on conservative missing index, where missing could shift pol.rpts by more than .05
dat<-dat[-error.index,]







####################################################################
# Models
####################################################################

### unconditional growth model, poisson, convergence problems
#m.0<-glmer(pol.rpts~1+(year|FIPS), data=dat, family="poisson", offset=log(child.pop))
### unconditional growth negbin, convergence problems
#m.0nb<-glmer(pol.rpts~1+(year|FIPS), data=dat, family="poisson", offset=log(child.pop))
### convergence problems even in no FE model
### intercept only model
### with overdispersion
# dat$n_obs<-1:nrow(dat)
# m.0od<-glmer(pol.rpts~1+(1|FIPS)+(1|n_obs), data=dat, family="poisson")

