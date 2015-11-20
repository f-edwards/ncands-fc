rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(nlme)
library(lme4)
library(texreg)
set.seed(1)

setwd("H:/")

### PULLED COL WIDTH, COL NAMES FROM VariableLayout.xlsx, transposed
### READ AND CLEAN DATA
mal<-read.csv("H:/data/state-malt.csv")

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

s.dat<-left_join(mal, state, by=c("state", "year"))
s.dat$obs<-1:nrow(s.dat)

### UNCONDITIONAL GROWTH
m0<-glmer(tot.rpt~year+
	(year|state),
	offset=log(child),
	data=s.dat,
	family=poisson)

### UNCONDITIONAL GROWTH, OVERDISPERSED
m0.od<-glmer(tot.rpt~year+
	(year|state)+(1|obs_n),
	offset=log(child),
	data=s.dat,
	family=poisson)

### PLUS DEMOGRAPHIC CONTROLS
m1<-glmer(tot.rpt~pctblk+chpovrt+unemprt+childnot2par+gsppercap+
	year+(year|state),
	offset=log(child),
	data=s.dat,
	family=poisson)

### POLICE REPORTS - RATES OR COUNTS? 
m.pol.0<-glmer(tot.rpt~pctblk+chpovrt+unemprt+childnot2par+gsppercap+
	year+(year|state),
	offset=log(child),
	data=s.dat,
	family=poisson)