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

setwd("R:/Project/NCANDS/ncands-csv/")

cdat<-read.csv("ncands-rpt-county-07-12.csv", colClasses=c("character", "character", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric", "numeric",
                                                           "numeric"))

names(cdat)[which(names(cdat)=="RptFIPS")]<-"FIPS"
names(cdat)[which(names(cdat)=="SubYr")]<-"year"
cdat[which(cdat$FIPS=="12025"), "FIPS"]<-"12086" ##FIX MIAMI-DADE RECODE

pol.dat<-cdat%>%filter(RptSrc==4)

pov<-read.csv("saipe.csv")
pov$fips.st<-ifelse(nchar(as.character(pov$fips.st))<2, 
                    paste("0", as.character(pov$fips.st), sep=""), as.character(pov$fips.st))
pov$fips.cnty<-ifelse(nchar(as.character(pov$fips.cnty))==1,
                      paste("00", as.character(pov$fips.cnty), sep=""),
                      ifelse(nchar(as.character(pov$fips.cnty))==2,
                             paste("0", as.character(pov$fips.cnty), sep=""),
                             as.character(pov$fips.cnty)))
pov$FIPS<-paste(pov$fips.st, pov$fips.cnty, sep="")
pov$year<-as.character(pov$year)

infmort<-read.csv("AHRF-infmort.csv", colClasses = c(rep("character", 3)),
                   col.names=c("FIPS", "state", "county", 
                               "infmort2012","infmort2011","infmort2010","infmort2009","infmort2008","infmort2007",
                               "infmort2006","infmort2005","infmort2004","infmort2003","infmort2002",
                               "infmort2001","infmort2000",
                               "wht.infmort2012","wht.infmort2011","wht.infmort2010","wht.infmort2009","wht.infmort2008","wht.infmort2007",
                               "wht.infmort2006","wht.infmort2005","wht.infmort2004","wht.infmort2003","wht.infmort2002",
                               "wht.infmort2001","wht.infmort2000",
                               "nonwht.infmort2012","nonwht.infmort2011","nonwht.infmort2010","nonwht.infmort2009","nonwht.infmort2008","nonwht.infmort2007",
                               "nonwht.infmort2006","nonwht.infmort2005","nonwht.infmort2004","nonwht.infmort2003","nonwht.infmort2002",
                               "nonwht.infmort2001","nonwht.infmort2000"))

infmorttot<-infmort[,1:16]
infmorttot<-gather(infmorttot, year, infmort, -c(FIPS, state, county))
infmorttot$year<-substr(infmorttot$year, nchar(infmorttot$year)-3,nchar(infmorttot$year))
infmortwht<-infmort[,c(1:3, 17:29)]
infmortwht<-gather(infmortwht, year, wht.infmort, -c(FIPS, state, county))
infmortwht$year<-substr(infmortwht$year, nchar(infmortwht$year)-3,nchar(infmortwht$year))
infmortnonwht<-infmort[,c(1:3, 30:ncol(infmort))]
infmortnonwht<-gather(infmortnonwht, year, nonwht.infmort, -c(FIPS, state, county))
infmortnonwht$year<-substr(infmortnonwht$year, nchar(infmortnonwht$year)-3,nchar(infmortnonwht$year))
inf.mort<-full_join(full_join(infmorttot, infmortwht), infmortnonwht)




### ID PROBLEM COUNTIES, where upperbound on CJ reports is more than 5 percent larger than reported count
error.index<-which(with(pol.dat, (upper/n)>1.05))
missing.conservative<-pol.dat[error.index,]

### Drop counties with more than 5 percent of cases missing
error.index1<-which(with(pol.dat, missing.NA/cases>0.05))
missing.liberal<-pol.dat[error.index1,]

samp.dat<-pol.dat[-error.index,]


cdat<-left_join(left_join(left_join(cdat, ucr.county.tot), pov), inf.mort)









