rm(list=ls())
gc()

.libPaths( c( .libPaths(), "U:/R") )

library(Amelia)
library(MASS)
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
set.seed(1)
setwd("R:/Project/NCANDS/ncands-fc/")
source("R:/Project/NCANDS/ncands-fc/read-main.r")

### ID PROBLEM COUNTIES, where upperbound on CJ reports is more than 5 percent larger than reported count
# error.index<-which(with(dat, (pol.rpts.upper/pol.rpts)>1.05))
# missing.conservative<-dat[error.index,]

### Drop counties with more than 10 percent of cases missing
error.index1<-which(with(dat, prop.missing.rptsrc>0.1))
missing.liberal<-dat[error.index1,]

### for model diagnostics, complete cases only
dat<-dat[complete.cases(dat),]
dat<-dat%>%filter(pol.blk.pc<1)
## to subset for fitting
#dat1<-dat[sample(dat$n_obs, trunc(nrow(dat)/5)),]

### drop on conservative missing index, where missing could shift pol.rpts by more than .05
# dat<-dat[-error.index1,]

# freq<-as.data.frame(table(dat$FIPS))
# samp<-sample(freq[which(freq[,2]==6),1], 20)
# 
# samp.dat<-dat%>%filter(FIPS%in%samp)
# 
# ggplot(samp.dat, aes(x=year))+geom_line(aes(y=pol.tot.pc))+facet_wrap(~county)

# 
# dat.imp<-amelia(dat, ts="year", cs="FIPS", idvars=c("RptSrc", "fips.st", "fips.cnty", "county"), 
#                 noms="state",
#                 polytime=3)

####################################################################
# Models
####################################################################

### Model taxonomy with pol.rpts and arrest.all.tot.pc
library(arm)

poisson0<-glm(pol.rpts~arrest.all.tot.pc, data=dat, family=poisson(), offset=log(child.pop))

### FOLLOWING GELMAN 45

lm0<-lm(pol.tot.pc~arrest.all.tot.pc, data=dat)
plot(dat$arrest.all.tot.pc, dat$pol.tot.pc, pch=".")
x<-(dat$arrest.all.tot.pc)
curve(cbind(1, x)%*%coef(lm0), add=TRUE)
lm0.sim<-sim(lm0)
for(i in 1:10){
  curve(cbind(1,x)%*%coef(lm0.sim)[i,], add=TRUE, col="gray")
}
curve(cbind(1, x)%*%coef(lm0), add=TRUE)

gamma0<-glm(pol.tot.pc~arrest.all.tot.pc, data=dat, family=Gamma(link=log))
plot(dat$arrest.all.tot.pc, dat$pol.tot.pc, pch=".")
x<-(dat$arrest.all.tot.pc)
curve(cbind(1, x)%*%coef(gamma0), add=TRUE)
gamma0.sim<-sim(gamma0)
for(i in 1:10){
  curve(exp(cbind(1,x)%*%coef(gamma0.sim)[i,]), add=TRUE, col="gray")
}
curve(exp(cbind(1, x)%*%coef(gamma0)), add=TRUE)

poisson0<-glm(pol.rpts~arrest.all.tot.pc, data=dat, family=poisson(), offset=log(child.pop))
plot(dat$arrest.all.tot.pc, dat$pol.tot.pc, pch=".")
x<-(dat$arrest.all.tot.pc)
curve(cbind(1, x)%*%coef(poisson0), add=TRUE)
poisson0.sim<-sim(poisson0)
for(i in 1:10){
  curve(exp(cbind(1,x)%*%coef(poisson0.sim)[i,]), add=TRUE, col="gray")
}
curve(exp(cbind(1, x)%*%coef(poisson0)), add=TRUE)

lm1<-lm(pol.tot.pc~arrest.all.tot.pc+child.pov.pct, data=dat)
beta.hat<-coef(lm1)
beta.sim<-coef(sim(lm1))

plot(dat$arrest.all.tot.pc, dat$pol.tot.pc, pch=".")
for(i in 1:10){
  curve(cbind(1,dat$arrest.all.tot.pc, mean(dat$child.pov.pct))%*%coef(beta.sim)[i,], add=TRUE, col="gray")
}
curve(cbind(1,mean(dat$child.pov.pct), x)%*%coef(beta.sim)[i,], add=TRUE, col="black")

gamma1<-glm(pol.tot.pc~arrest.all.tot.pc, data=dat, family=Gamma(link=log))
plot(dat$arrest.all.tot.pc, dat$pol.tot.pc, pch=".")
x<-(dat$arrest.all.tot.pc)
curve(cbind(1, x)%*%coef(gamma1), add=TRUE)
gamma1.sim<-sim(gamma1)
for(i in 1:11){
  curve(exp(cbind(1,x)%*%coef(gamma1.sim)[i,]), add=TRUE, col="gray")
}
curve(exp(cbind(1, x)%*%coef(gamma1)), add=TRUE)

poisson1<-glm(pol.rpts~arrest.all.tot.pc, data=dat, family=poisson(), offset=log(child.pop))
plot(dat$arrest.all.tot.pc, dat$pol.tot.pc, pch=".")
x<-(dat$arrest.all.tot.pc)
curve(cbind(1, x)%*%coef(poisson1), add=TRUE)
poisson1.sim<-sim(poisson1)
for(i in 1:11){
  curve(exp(cbind(1,x)%*%coef(poisson1.sim)[i,]), add=TRUE, col="gray")
}
curve(exp(cbind(1, x)%*%coef(poisson1)), add=TRUE)


#### RUN EACH MODEL FOR DIFF CATS OF OFFENSES FOR ALL GROUPS

## unconditional growth model, poisson, convergence problems
## unconditional growth negbin, convergence problems
## convergence problems even in no FE model
## intercept only model
## with overdispersion
## State random effects matter alot
                                                                                                                                                                                                                                                                                                   

dat$n_obs<-1:nrow(dat)

#### MODEL SPECIFICATIONS - PARTIAL POOLING
total.all<-pol.rpts~
  scale(infmort)*scale(arrest.all.tot.pc)+
  scale(infmort)*scale(police.pc)+
  scale(child.pov.pct)*scale(police.pc)+
  scale(child.pov.pct)*scale(arrest.all.tot.pc)+
  scale(pop.density)+
  scale(infmort.mean)+scale(child.pov.pct.mean)+
  scale(arrest.all.tot.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

total.viol<-pol.rpts~
  scale(infmort)*scale(arrest.viol.tot.pc)+
  scale(infmort)*scale(police.pc)+
  scale(child.pov.pct)*scale(police.pc)+
  scale(child.pov.pct)*scale(arrest.viol.tot.pc)+
  scale(pop.density)+
  scale(infmort.mean)+scale(child.pov.pct.mean)+
  scale(arrest.viol.tot.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

total.drug<-pol.rpts~
  scale(infmort)*scale(arrest.drug.tot.pc)+
  scale(infmort)*scale(police.pc)+
  scale(child.pov.pct)*scale(police.pc)+
  scale(child.pov.pct)*scale(arrest.drug.tot.pc)+
  scale(pop.density)+
  scale(infmort.mean)+scale(child.pov.pct.mean)+
  scale(arrest.drug.tot.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

total.qol<-pol.rpts~
  scale(infmort)*scale(arrest.qol.tot.pc)+
  scale(infmort)*scale(police.pc)+
  scale(child.pov.pct)*scale(police.pc)+
  scale(child.pov.pct)*scale(arrest.qol.tot.pc)+
  scale(pop.density)+
  scale(infmort.mean)+scale(child.pov.pct.mean)+
  scale(arrest.qol.tot.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)


m.total.all.w<-glmer(total.all, data=dat, family="poisson", offset=log(child.pop),
                   control=glmerControl(optimizer = "bobyqa"))
m.total.all<-glmer(total.all, data=dat, family="poisson", offset=log(child.pop),
                     control=glmerControl(optimizer = "bobyqa"))

m.total.viol<-glmer(total.viol, data=dat, family="poisson", offset=log(child.pop),
                    control=glmerControl(optimizer = "bobyqa"))
m.total.drug<-glmer(total.drug, data=dat, family="poisson", offset=log(child.pop),
                    control=glmerControl(optimizer = "bobyqa"))
m.total.qol<-glmer(total.qol, data=dat, family="poisson", offset=log(child.pop),
                   control=glmerControl(optimizer = "bobyqa"))

### MODEL DIAGNOSTICS
### CHECK BOLKER http://ms.mcmaster.ca/~bolker/R/misc/foxchapter/bolker_chap.html
plot(fitted(m.total.all),dat$pol.rpts)
abline(0,1)
plot(residuals(m.total.all), m.total.all@frame$pol.rpts)
### LOOK INTO BINOMIAL, BETABINOMIAL, GAMMA, ADOLPH SLIDES,
#Bell and Jones Explaining Fixed Effects: Random Effects Modeling of Time-Series Cross-Sectional and Panel Data
#Harrison A comparison of observation-level random effect and Beta-Binomial models for modelling overdispersion in Binomial data in ecology & evolution


### BLACK ARREST MODELS

blk.all<-pol.blk~
  scale(nonwht.infmort)*scale(arrest.all.blk.pc)+
  scale(nonwht.infmort)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(arrest.all.blk.pc)+
  scale(pop.density)+
  scale(nonwht.infmort.mean)+scale(blk.chpov_pe.mean)+
  scale(arrest.all.blk.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  scale(pct.blk)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

blk.viol<-pol.blk~
  scale(nonwht.infmort)*scale(arrest.viol.blk.pc)+
  scale(nonwht.infmort)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(arrest.viol.blk.pc)+
  scale(pop.density)+
  scale(nonwht.infmort.mean)+scale(blk.chpov_pe.mean)+
  scale(arrest.viol.blk.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  scale(pct.blk)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

blk.drug<-pol.blk~
  scale(nonwht.infmort)*scale(arrest.drug.blk.pc)+
  scale(nonwht.infmort)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(arrest.drug.blk.pc)+
  scale(pop.density)+
  scale(nonwht.infmort.mean)+scale(blk.chpov_pe.mean)+
  scale(arrest.drug.blk.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  scale(pct.blk)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

blk.qol<-pol.blk~
  scale(nonwht.infmort)*scale(arrest.qol.blk.pc)+
  scale(nonwht.infmort)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(arrest.qol.blk.pc)+
  scale(pop.density)+
  scale(nonwht.infmort.mean)+scale(blk.chpov_pe.mean)+
  scale(arrest.qol.blk.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  scale(pct.blk)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

m.blk.all<-glmer(blk.all, data=dat, family="poisson", offset=log(blk.child.pop),
                   control=glmerControl(optimizer = "bobyqa"))
m.blk.viol<-glmer(blk.viol, data=dat, family="poisson", offset=log(blk.child.pop),
                    control=glmerControl(optimizer = "bobyqa"))
m.blk.drug<-glmer(blk.drug, data=dat, family="poisson", offset=log(blk.child.pop),
                    control=glmerControl(optimizer = "bobyqa"))
m.blk.qol<-glmer(blk.qol, data=dat, family="poisson", offset=log(blk.child.pop),
                   control=glmerControl(optimizer = "bobyqa"))

dat<-dat%>%filter(pol.blk/blk.child.pop<1)
dat$pol.blk<-as.integer(dat$pol.blk)

blk.all.binom<-cbind(pol.blk,blk.child.pop-pol.blk)~
  scale(nonwht.infmort)*scale(arrest.all.blk.pc)+
  scale(nonwht.infmort)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(police.pc)+
  scale(blk.chpov_pe)*scale(arrest.all.blk.pc)+
  scale(pop.density)+
  scale(nonwht.infmort.mean)+scale(blk.chpov_pe.mean)+
  scale(arrest.all.blk.pc.mean)+scale(police.pc.mean)+
  scale(pop.density.mean)+
  scale(pct.blk)+
  (1|state)+(1|FIPS)+(1|year)+(1|n_obs)

m.blk.all.binom<-glmer(blk.all.binom, data=dat, family=binomial)

### weird problem with high residuals on small fitted. not sure how to adjust for that. 
### go through gelman model taxonomy formally, keep all in here, run diagnostic plots, do more datavis, store it