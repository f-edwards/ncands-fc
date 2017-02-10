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
inf.mort$year<-as.numeric(inf.mort$year)
inf.mort$infmort<-as.numeric(inf.mort$infmort)
inf.mort$wht.infmort<-as.numeric(inf.mort$wht.infmort)
inf.mort$nonwht.infmort<-as.numeric(inf.mort$nonwht.infmort)
write.csv(inf.mort, "infmort-clean.csv", row.names=FALSE)