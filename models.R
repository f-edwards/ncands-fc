rm(list=ls())
gc()

.libPaths( c( .libPaths(), "U:/R") )

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

### drop on conservative missing index, where missing could shift pol.rpts by more than .05
dat<-dat[-error.index1,]

# freq<-as.data.frame(table(dat$FIPS))
# samp<-sample(freq[which(freq[,2]==6),1], 20)
# 
# samp.dat<-dat%>%filter(FIPS%in%samp)
# 
# ggplot(samp.dat, aes(x=year))+geom_line(aes(y=pol.tot.pc))+facet_wrap(~county)




####################################################################
# Models
####################################################################

#### RUN EACH MODEL FOR DIFF CATS OF OFFENSES FOR ALL GROUPS

## unconditional growth model, poisson, convergence problems
## unconditional growth negbin, convergence problems
## convergence problems even in no FE model
## intercept only model
## with overdispersion
dat<-dat%>%group_by(FIPS)%>%
  mutate(infmort.mean=mean(infmort, na.rm=TRUE),
         child.pov.pct.mean=mean(child.pov.pct, na.rm=TRUE),
         arrest.all.tot.pc.mean=mean(arrest.all.tot.pc, na.rm=TRUE),
         arrest.viol.tot.pc.mean=mean(arrest.viol.tot.pc, na.rm=TRUE),
         arrest.drug.tot.pc.mean=mean(arrest.drug.tot.pc, na.rm=TRUE),
         arrest.qol.tot.pc.mean=mean(arrest.qol.tot.pc, na.rm=TRUE),
         police.pc.mean=mean(police.pc, na.rm=TRUE),
         pop.density.mean=mean(pop.density, na.rm=TRUE))%>%ungroup()

dat$n_obs<-1:nrow(dat)

#### MODEL SPECIFICATIONS - PARTIAL POOLING
total.all<-pol.rpts~scale(infmort)+
  scale(child.pov.pct)+
  scale(arrest.all.tot.pc)+
  scale(police.pc)+
  scale(pop.density)+
  (1|FIPS)+(1|year)+(1|n_obs)

total.all.FE<-update(total.all, ~.+scale(infmort.mean)+scale(child.pov.pct.mean)+
                       scale(arrest.all.tot.pc.mean)+scale(police.pc.mean)+
                       scale(pop.density.mean))

total.viol<-pol.rpts~scale(infmort)+
  scale(child.pov.pct)+
  scale(arrest.viol.tot.pc)+
  scale(police.pc)+
  scale(pop.density)+
  (1|FIPS)+(1|year)+(1|n_obs)

total.viol.FE<-update(total.viol, ~.+scale(infmort.mean)+scale(child.pov.pct.mean)+
                       scale(arrest.viol.tot.pc.mean)+scale(police.pc.mean)+
                       scale(pop.density.mean))

total.drug<-pol.rpts~scale(infmort)+
  scale(child.pov.pct)+
  scale(arrest.drug.tot.pc)+
  scale(police.pc)+
  scale(pop.density)+
  (1|FIPS)+(1|year)+(1|n_obs)

total.drug.FE<-update(total.drug, ~.+scale(infmort.mean)+scale(child.pov.pct.mean)+
                        scale(arrest.drug.tot.pc.mean)+scale(police.pc.mean)+
                        scale(pop.density.mean))

total.qol<-pol.rpts~scale(infmort)+
  scale(child.pov.pct)+
  scale(arrest.qol.tot.pc)+
  scale(police.pc)+
  scale(pop.density)+
  (1|FIPS)+(1|year)+(1|n_obs)

total.qol.FE<-update(total.qol, ~.+scale(infmort.mean)+scale(child.pov.pct.mean)+
                        scale(arrest.qol.tot.pc.mean)+scale(police.pc.mean)+
                        scale(pop.density.mean))


m.total.all<-glmer(total.all, data=dat, family="poisson", offset=log(child.pop),
                   control=glmerControl(optimizer = "bobyqa"))
m.total.viol<-glmer(total.viol, data=dat, family="poisson", offset=log(child.pop),
                    control=glmerControl(optimizer = "bobyqa"))
m.total.drug<-glmer(total.drug, data=dat, family="poisson", offset=log(child.pop),
                    control=glmerControl(optimizer = "bobyqa"))
m.total.qol<-glmer(total.qol, data=dat, family="poisson", offset=log(child.pop),
                   control=glmerControl(optimizer = "bobyqa"))


m.total.all.FE<-glmer(total.all.FE, data=dat, family="poisson", offset=log(child.pop),
                      control=glmerControl(optimizer = "bobyqa"))
m.total.viol.FE<-glmer(total.viol.FE, data=dat, family="poisson", offset=log(child.pop),
                       control=glmerControl(optimizer = "bobyqa"))
m.total.drug.FE<-glmer(total.drug.FE, data=dat, family="poisson", offset=log(child.pop),
                       control=glmerControl(optimizer = "bobyqa"))
m.total.qol.FE<-glmer(total.qol.FE, data=dat, family="poisson", offset=log(child.pop),
                      control=glmerControl(optimizer = "bobyqa"))

# drug.stan<-stan_glmer(total.drug, data=dat, family="poisson", offset=log(child.pop))
# drug.stan.nb<-stan_glmer.nb(pol.rpts~scale(infmort)+
#                               scale(child.pov.pct)+
#                               scale(arrest.drug.tot.pc)+
#                               scale(police.pc)+
#                               scale(pop.density)+
#                               (1|FIPS)+(1|year), data=dat,offset=log(child.pop))

###DIAGNOSTICS!!! RESID/FITTED, QQPLOTS, SUPER WONKY ERRORS, POORLY FITTING SMALL COUNT PLACES
###WONKY RESIDUALS FOR LOW COUNT PLACES, EVEN AFTER DROPPING TOT.POP<100000