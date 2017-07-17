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

ucr<-read.csv("ucr-county-arrest-offense.csv")

ucr<-ucr%>%filter(!(is.na(FIPS)))
ucr<-ucr%>%rename(year=YEAR)

###################################
## SEER POPULATION ESTIMATES
###################################

pop<-read.csv("seer-pop.csv")

# ###################################
# ## NCANDS
# ###################################
# 
# cdat<-read.csv("ncands-wide-04-12.csv", colClasses=c("character",rep("numeric", 16)))

###################################
## SAIPE
###################################

pov<-read.csv("saipe.csv")

pov<-pov%>%dplyr::select(child.pov, child.pov.se, FIPS, year, median.hh.income)

# ###################################
# ## AHRF INFMORT
# ###################################
# 
# inf.mort<-read.csv("infmort-clean.csv", colClasses = c(rep("character",3),
#                                                        rep("numeric", 4)))

### consider using infmort at state level? probably best given huge missing in county
### filter only state data
# inf.mort<-inf.mort%>%filter(substr(FIPS, 3, 5)=="000")%>%dplyr::select(-c(FIPS, county))

###################################
## NHGIS POV BY RACE
###################################

###nhgis poverty data, includes standard errors for estimates
nhgis<-read.csv("nhgis-chpov.csv")
#### data are 5 yr, currently using 1st yr as year, add 1 for better match
nhgis$year<-nhgis$year+1

###################################
## ACS POP DENSITY
###################################

density<-read.csv("density.csv")
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
ucr[which(ucr$FIPS==12025), "FIPS"]<-12086 ##FIX MIAMI-DADE RECODE

### create full data, include NAs for all de-identified counties so it's easy later
m1<-full_join(density, pov)%>%
  full_join(nhgis)%>%
  full_join(ucr)

m1$state<-substr(m1$FIPS, 1, 2)

pol.budget<-read.csv("police-budget.csv", stringsAsFactors = FALSE, colClasses = c("character", "numeric", "numeric"))
dat<-left_join(m1, pol.budget)

#### join all FIPS last, join in descending order of complexity

write.csv(dat, "export.csv", row.names=FALSE)