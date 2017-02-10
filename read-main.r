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

ucr<-read.csv("ucr-county-offense.csv", colClasses=c(rep("character",2), rep(NA, 4)))
names(ucr)[which(names(ucr)=="YEAR")]<-"year"
ucr$year<-as.numeric(ucr$year)

pop<-read.csv("seer-pop.csv", colClasses=c("character", NA))

cdat<-read.csv("ncands-rpt-county-04-12.csv", colClasses=c("character", "character", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric", "numeric",
                                                           "numeric"))

names(cdat)[which(names(cdat)=="RptFIPS")]<-"FIPS"
names(cdat)[which(names(cdat)=="SubYr")]<-"year"
cdat[which(cdat$FIPS=="12025"), "FIPS"]<-"12086" ##FIX MIAMI-DADE RECODE


pov<-read.csv("saipe.csv")
pov$fips.st<-ifelse(nchar(as.character(pov$fips.st))<2, 
                    paste("0", as.character(pov$fips.st), sep=""), as.character(pov$fips.st))
pov$fips.cnty<-ifelse(nchar(as.character(pov$fips.cnty))==1,
                      paste("00", as.character(pov$fips.cnty), sep=""),
                      ifelse(nchar(as.character(pov$fips.cnty))==2,
                             paste("0", as.character(pov$fips.cnty), sep=""),
                             as.character(pov$fips.cnty)))
pov$FIPS<-paste(pov$fips.st, pov$fips.cnty, sep="")

inf.mort<-read.csv("infmort-clean.csv", colClasses = c(rep("character",3),
                                                       rep("numeric", 4)))

dat<-left_join(left_join(left_join(left_join(cdat, inf.mort), pop), ucr), pov)

#filter out de-identified counties for now, maybe create state aggregates later for separate L1 frame
dat<-dat%>%filter(!(substr(FIPS, 3, 5)%in%c("000", "999")))

rm(ucr)
rm(pop)
rm(pov)
rm(cdat)
rm(inf.mort)
gc()