###first get median scenario
###then get countefactuals
###then diff the two with a 95 percent interval
###or just plot out the expected increase from median over range
###do this as the new plot, expected diff from some median case, zeroes out the REs

.libPaths( c( .libPaths(), "U:/R") )

library(arm)
library(ggplot2)
library(dplyr)
library(rstanarm)
library(shinystan)
library(lme4)
library(gridExtra)
library(grid)
library(bayesplot)
load("R:/Project/NCANDS/ncands-fc/models-within-norural.RData")
set.seed(1)

setwd("R:/Project/NCANDS/ncands-fc/tables")

fill.in<-function(x){
  x[is.na(x)]<-0
  ## set king county arbitrarily
  x$FIPS<-"53033"
  return(x)
}

###scratch
x<-data.frame(x=1, y=1, z=1)

for(i in 1:ncol(x)){
  y<-cbind(x[1,i], x[1,i])
}

### write function to cbind(median var, diff var) for each column ala rstanarm vignette
### think this through - take sd for each by race, gender, do a 1 sd increase



wht.dat<-within.dat%>%filter(race=="wht")

### median scenario
wht.all.median<-wht.dat%>%filter(offense=="all")%>%summarise_if(is.numeric, function(x){median(x, na.rm=TRUE)})
wht.all.median<-fill.in(wht.all.median)

### using quantile(within.dat$diff.arrest.rt, c(0.05, 0.5, 0.95)), 0.006 is sd, about a 95th percentile shock - extreme
wht.scen<-rbind(wht.all.median, wht.all.median%>%mutate(diff.arrest.rt=diff.arrest.rt+0.006))

wht.sim.diff.arrest<-posterior_predict(b.all.white, wht.scen)

wht.diff.arrest<-apply(wht.sim.diff.arrest, 1, diff)
w.a<-quantile(wht.diff, c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))
w.d<-quantile(apply(posterior_predict(b.white.drug, wht.scen), 1, diff),
  c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975))
