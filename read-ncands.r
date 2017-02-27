rm(list=ls())
gc()
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

setwd("R:/Project/NCANDS/ncands-csv/")
ncands.county<-function(dat){
  dat$race<-with(dat, ifelse(ChRacBl==1, "black",
                             ifelse(ChRacAI==1, "amind",
                                    ifelse(ChRacNH==1, "hawpi",
                                           ifelse(ChRacAs==1, "asian",
                                                  ifelse(ChRacWh==1, "white", "missing"))))))
  
  dat$race[is.na(dat$race)]<-"missing"
  
  total<-dat%>%group_by(StaTerr, RptFIPS, SubYr)%>%
    count(RptSrc)
  race<-dat%>%group_by(StaTerr, RptFIPS, SubYr, race)%>%
    count(RptSrc)%>%spread(key=race, value=n, fill=0)
  cases<-dat%>%group_by(StaTerr, RptFIPS, SubYr)%>%
    summarise(cases=n(), 
              missing.NA=(sum(RptSrc==99, na.rm=TRUE)+sum(is.na(RptSrc))))
  cdat<-full_join(full_join(total, race), cases)    
  
  cdat<-cdat%>%mutate(proportion=missing.NA/cases)  
  
  cdat<-cdat%>%mutate(upper=n+missing.NA)
  
  cdat<-cdat%>%filter(RptSrc==4)
  names(cdat)<-c("state", "FIPS", "year", "RptSrc", "pol.rpts",
                 "pol.ai", "pol.aa", "pol.blk", "pol.hpi", "pol.na", "pol.wht",
                 "tot.reports", "missing.rptsrc", "prop.missing.rptsrc", "pol.rpts.upper")
  return(as.data.frame(cdat))
}


dat12<-read.dta("Child2012v1.dta", convert.factors = FALSE)
county.out<-ncands.county(dat12)
rm(dat12)
gc()
dat11<-read.dta("Child2011v2.dta", convert.factors = FALSE)
county.out<-bind_rows(county.out, ncands.county(dat11))
rm(dat11)
gc()
dat10<-read.dta("Child2010v1a.dta", convert.factors = FALSE)
county.out<-bind_rows(county.out, ncands.county(dat10))
rm(dat10)
gc()
dat09<-read.dta("Child2009v2a.dta", convert.factors = FALSE)
county.out<-bind_rows(county.out, ncands.county(dat09))
rm(dat09)
gc()
dat08<-read.dta("Child2008v3a.dta", convert.factors = FALSE)
county.out<-bind_rows(county.out, ncands.county(dat08))
rm(dat08)
gc()
dat07<-read.dta("Child2007v2a.dta", convert.factors = FALSE)
county.out<-bind_rows(county.out, ncands.county(dat07))
rm(dat07)
gc()
dat06<-read.dta("Child2006_v1a.dta", convert.factors = FALSE)
county.out<-bind_rows(county.out, ncands.county(dat06))
rm(dat06)
gc()
dat05<-read.dta("Child2005v1a.dta", convert.factors = FALSE)
county.out<-bind_rows(county.out, ncands.county(dat05))
rm(dat05)
gc()
dat04<-read.dta("Child2004_v1a.dta", convert.factors = FALSE)
### load state.fips from library(maps) - match fips on state abbrev
library(maps)
data(state.fips)
state.fips$st.FIPS<-ifelse(nchar(as.character(state.fips$fips))==1, 
                           paste("0", as.character(state.fips$fips), sep=""),
                           as.character(state.fips$fips))
names(state.fips)[which(names(state.fips)=="abb")]<-"StaTerr"
dat04<-left_join(dat04, state.fips, by="StaTerr")
dat04$cnty.fips<-as.character(dat04$RptCnty)
dat04$cnty.fips[dat04$cnty.fips=="-1"]<-"000"
dat04$cnty.fips<-substr(dat04$cnty.fips, nchar(dat04$cnty.fips)-2, nchar(dat04$cnty.fips))
dat04$RptFIPS<-with(dat04, paste(st.FIPS, cnty.fips, sep=""))
county.out<-bind_rows(county.out, ncands.county(dat04))
rm(dat04)
gc()

### EDA FOR COUNTY MISSINGNESS
### create counts by rptsrc, count proportions in each cat

### FILTER DUP KIDS ON SAME REPORT FOR RPTSRC COUNTS

write.csv(county.out, file="ncands-rpt-county-04-12.csv", row.names=FALSE)
q(save="no")