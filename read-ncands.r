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
set.seed(1)

setwd("R:/Project/NCANDS/ncands-csv/")

dat12<-read.dta("Child2012v1.dta", convert.factors=FALSE)
dat11<-read.dta("Child2011v2.dta", convert.factors = FALSE)
dat10<-read.dta("Child2010v1a.dta",  convert.factors = FALSE)
dat09<-read.dta("Child2009v2a.dta",  convert.factors = FALSE)
dat08<-read.dta("Child2008v3a.dta",  convert.factors = FALSE)
dat07<-read.dta("Child2007v2a.dta",  convert.factors = FALSE)

dat<-bind_rows(dat12, dat11, dat10, dat09, dat08, dat07)
rm(dat12, dat11, dat10, dat09, dat08, dat07)
### EDA FOR COUNTY MISSINGNESS
### create counts by rptsrc, count proportions in each cat

### FILTER DUP KIDS ON SAME REPORT FOR RPTSRC COUNTS
dat<-dat%>%filter(!(duplicated(RptID)))

cdat<-full_join(
  dat%>%group_by(StaTerr, RptFIPS, SubYr)%>%
    count(RptSrc),
  dat%>%group_by(StaTerr, RptFIPS, SubYr)%>%
    summarise(cases=n(), 
              missing.NA=(sum(RptSrc==99, na.rm=TRUE)+sum(is.na(RptSrc)))))

cdat<-cdat%>%mutate(proportion=n/cases)  

cdat<-cdat%>%mutate(upper=n+missing.NA)

write.csv(cdat, file="ncands-rpt-county-07-12.csv", row.names=FALSE)