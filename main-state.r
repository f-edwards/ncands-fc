rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(nlme)
library(lme4)
library(texreg)
library(ggplot2)
set.seed(1)

setwd("H:/")

### PULLED COL WIDTH, COL NAMES FROM VariableLayout.xlsx, transposed
### READ AND CLEAN DATA
mal<-read.csv("H:/data/state-malt.csv")
names(mal)[2]<-"stname"

state<-read.csv("H:/ncands-fc/statedat.csv")
### STATE LEVEL MEASURES
state$police.pc<-state$police.ft.emp/state$pop
state$welfare.pc<-state$welfare.ft.emp/state$pop
state$edu.pc<-state$edu.ft.emp/state$child
state$hosp.pc<-state$hosp.ft.emp/state$pop
state$pctblk<-state$blkpop/state$pop
state$chpovrt<-state$childpov/state$child
state$unemprt<-state$unemp/(state$unemp+state$emp)
state$childnot2par<-1-(state$kids2par/state$child)
state$gsppercap<-state$GSP*1000000/state$pop

s.dat<-left_join(mal, state, by=c("stname", "year"))
s.dat$obs<-1:nrow(s.dat)

s.dat<-s.dat%>%
  filter(year<2012)%>%
  filter(year>2001)%>%
  filter(stname!="XX")%>%
  filter(stname!="PR")%>%
  filter(stname!="DC")

s.dat$rpts.pc<-s.dat$tot.rpt/s.dat$child

ggplot(data=s.dat,
       aes(x=year, y=rpts.pc))+
  geom_point()+
  geom_line(stat="smooth", method="lm", se=FALSE)+
  facet_wrap(~stname)+
  theme_bw()

### UNCONDITIONAL GROWTH
m0<-lmer(rpts.pc~scale(year)+
	(1+scale(year)|stname),
	data=s.dat)


### UNCONDITIONAL GROWTH, OVERDISPERSED
m0.od<-glmer(tot.rpt~scale(year)+
	(scale(year)|stname)+(1|obs),
	offset=log(child),
	data=s.dat,
	family=poisson)

### PLUS DEMOGRAPHIC CONTROLS
m1.c<-glmer(tot.rpt~scale(pctblk)+
    scale(chpovrt)+scale(unemprt)+
    scale(childnot2par)+scale(year)+
    (scale(year)|stname)+
    (1|obs),
  	offset=log(child),
  	data=s.dat,
  	family=poisson)

m1<-lmer(rpts.pc~scale(pctblk)+
           scale(chpovrt)+scale(unemprt)+
           scale(childnot2par)+scale(year)+
           (scale(year)|stname), 
         data=s.dat)

### POLICE REPORTS - RATES OR COUNTS? 
m.pol.0<-glmer(tot.rpt~pctblk+chpovrt+unemprt+childnot2par+gsppercap+
	year+(year|state),
	offset=log(child),
	data=s.dat,
	family=poisson)