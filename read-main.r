.libPaths( c( .libPaths(), "U:/R") )
rm(list=ls())
gc()

library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
library(ggplot2)
library(foreign)
library(haven)
library(parallel)
set.seed(1)

setwd("R:/Project/NCANDS/ncands-csv/")

###################################
## UCR ARREST, OFFICERS, HOMICIDES
###################################

ucr<-read.csv("ucr-wide.csv", colClasses=c("character", rep("numeric", 27)))

ucr<-ucr%>%filter(!(is.na(FIPS)))
ucr<-ucr%>%rename(year=YEAR)

###################################
## SEER POPULATION ESTIMATES
###################################

pop<-read.csv("seer-pop.csv", colClasses=c("numeric", "character",
                                           rep("numeric", 10)))

###################################
## NCANDS
###################################

cdat<-read.csv("ncands-wide-04-12.csv", colClasses=c("character",rep("numeric", 16)))

###################################
## SAIPE
###################################

pov<-read.csv("saipe.csv", colClasses=c(rep("numeric", 7), "character","character",
                                        rep("numeric", 7), "character"))
pov<-pov%>%dplyr::select(child.pov, child.pov.se, FIPS, year, median.hh.income)

###################################
## AHRF INFMORT
###################################

inf.mort<-read.csv("infmort-clean.csv", colClasses = c(rep("character",3),
                                                       rep("numeric", 4)))

### consider using infmort at state level? probably best given huge missing in county
### filter only state data
# inf.mort<-inf.mort%>%filter(substr(FIPS, 3, 5)=="000")%>%dplyr::select(-c(FIPS, county))

###################################
## NHGIS POV BY RACE
###################################

###nhgis poverty data, includes standard errors for estimates
nhgis<-read.csv("nhgis-chpov.csv", colClasses=c(rep("numeric", 8), "character", "numeric"))
#### data are 5 yr, currently using 1st yr as year, add 1 for better match
nhgis$year<-nhgis$year+1

###################################
## ACS POP DENSITY
###################################

density<-read.csv("density.csv",
                  colClasses = c(rep("character", 7),
                                 rep(NA, 7)))
names(density)[5]<-"FIPS"
names(density)[12]<-"land.area"
density<-density%>%dplyr::select(FIPS, land.area)
density<-left_join(pop,density)
density$tot.pop<-density$adult+density$child
density$pop.density<-density$tot.pop/density$land.area
density<-density%>%dplyr::select(-land.area, -tot.pop)

###################################
# Clean and join
###################################
cdat[which(cdat$FIPS=="12025"), "FIPS"]<-"12086" ##FIX MIAMI-DADE RECODE
ucr[which(ucr$FIPS=="12025"), "FIPS"]<-"12086" ##FIX MIAMI-DADE RECODE

### create full data, include NAs for all de-identified counties so it's easy later
m<-left_join(cdat, density)
m1<-left_join(m, pov)
m2<-left_join(m1, nhgis)
m3<-left_join(m2, inf.mort)
m4<-left_join(m3, ucr)

m4$stname<-m4$state
m4$state<-substr(m4$FIPS, 1, 2)

pol.budget<-read.csv("police-budget.csv", stringsAsFactors = FALSE, colClasses = c("character", "numeric", "numeric"))
dat<-left_join(m4, pol.budget)

#### join all FIPS last, join in descending order of complexity

write.csv(dat, "ncands-fc-merge.csv", row.names=FALSE)