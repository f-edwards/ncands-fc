rm(list=ls())
require(TTR)
require(data.table)
require(dplyr)
library(plyr)
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

rpt<-left_join(pop, mal, by=c("state", "FIPS", "year" ))
### Drop counties missing all report data (small pop)
rpt<-rpt[-(which(is.na(rpt$tot.rpt))),]
rpt$rpt.pc<-rpt$tot.rpt/rpt$child.pop

### Unconditional growth model for reports per capita
uc.gr<-lme(fixed=rpt.pc~year,
           random=~1+year|cname, 
           data=rpt)

