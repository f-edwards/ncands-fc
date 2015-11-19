rm(list=ls())
require(TTR)
require(data.table)
require(dplyr)
library(tidyr)
require(Amelia)
library(nlme)
library(lme4)
library(ggplot2)

set.seed(1)
setwd("H:/Data")

### read county level malt rpt data
mal<-read.csv("county.csv", stringsAsFactors = FALSE)
mal<-mal[,-1]
names(mal)[1:2]<-c("FIPS", "state")

### read cdc child pop data - manually removed county 'total' rows and notes column
pop<-read.table("CDC-Wonder-County-Kids.tsv", sep="",
                stringsAsFactors = FALSE, head=TRUE)
names(pop)<-c("cname", "FIPS", "year", "year.dup", "child.pop")
### pull out state abrev from "NAME County, ST format in var
z<-do.call("rbind",(strsplit(pop$cname, ", ")))
pop$cname<-z[,1]
pop$state<-z[,2]

rpt<-left_join(mal, pop, by=c("FIPS", "year"))

emp<-read.csv("county-emp.csv", head=TRUE, stringsAsFactors = FALSE)

rpt<-left_join(rpt, emp, by=c("FIPS", "year"))

acs<-read.csv("H:/census/acs-5yr-co.csv", head=TRUE)
names(acs)[c(9, 11, 4)]<-c("FIPS.st", "FIPS.co", "year")
acs<-acs%>%
  select(FIPS.st, FIPS.co, lessHS, pov2, unemp.rt, pov.rt, pctblk, pctlat, pctwht)
rpt<-left_join(rpt, acs, by=c("FIPS.st", "FIPS.co"))

rpt<-rpt%>%
  filter(year>2006)%>%
  filter(FIPS>0)%>%
  filter(!(is.na(cname)))%>%
  select(-c(year.dup, state.y))

### ID COUNTIES WITH MISSING EMP DATA, PRESENT RPT DATA
z<-which(is.na(rpt$totemp))
missing<-data.frame("st"=rpt[z,"state.x"], "county"=rpt[z,"cname"])

### ID COUNTIES WITH MISSING RPTSRC DATA
z.1<-which(is.na(rpt$rpt.inf))
missing.rpt<-data.frame("st"=rpt[z.1,"state.x"], "county"=rpt[z.1,"cname"], "year"=rpt[z.1,"year"])

### ID COUNTIES WITH MISSING POP DATA
z.2<-which(is.na(rpt$totpop))
missing.acs<-data.frame("st"=rpt[z.2,"state.x"], "county"=rpt[z.2,"cname"], "year"=rpt[z.2,"year"])

### Drop counties missing all report data (small pop)
rpt<-rpt[-(which(is.na(rpt$rpt.inf))),]
rpt<-rpt[-(which(is.na(rpt$totemp))),]
rpt$rpt.pc<-rpt$tot.rpt/rpt$child.pop
rpt$year.c<-rpt$year-2007


### Unconditional growth model for reports per capita
m0<-lme(fixed=rpt.pc~year,
           random=~year|FIPS, 
           data=rpt)

m1<-lme(fixed=rpt.pc~I(totemp/child.pop)+pov2+pctblk+year.c,
        random=~1+year.c|FIPS, 
        data=rpt, na.action="na.omit")


