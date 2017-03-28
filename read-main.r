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

### RESHAPE DATA TO LONG WITH RACE ID COLUMN - WILL HAVE REPEATED COUNTY ROWS, but way fewer columns

ucr<-read.csv("ucr-gender-plus.csv", colClasses=c("character", "numeric", "character",
                                                     "character", "character", "numeric",
                                                  "numeric", "numeric"))

names(ucr)[which(names(ucr)=="YEAR")]<-"year"
ucr[which(ucr$arrest<0),"arrest"]<-NA
ucr<-ucr%>%filter(!(is.na(FIPS)))
### murder data missing for 2014
ucr<-ucr%>%filter(year!=2014)

#### ADD GENDER ARREST RATES AND MURDER MOVING AVERAGES

pop<-read.csv("seer-pop.csv", colClasses=c("numeric", rep("character", 2),
                                           rep("numeric", 3))) 

pop$gender<-ifelse(pop$sex==0, "all", ifelse(
  pop$sex==1, "male", ifelse(
    pop$sex==2, "female", NA
  )))

pop<-pop%>%dplyr::select(-sex)

cdat<-read.csv("ncands-rpt-county-04-12.csv", 
               colClasses=c("character","numeric", "character",
                            "character", "numeric",
                            "numeric"))
### DROP STATE TOTALS AND DEIDENTIFIED

cdat<-cdat%>%filter(RptSrc=="police")
cdat<-cdat%>%dplyr::select(-RptSrc)
### weird - some duplicates
index<-which(duplicated(cdat))
cdat<-cdat[-index,]

cdat<-cdat%>%filter(substr(FIPS, 3, 5)!="000")%>%filter(substr(FIPS, 3, 5)!="999")%>%
  filter(substr(FIPS, 1, 2)!="NA")
cdat<-cdat%>%filter(substr(FIPS, 1, 2)!="72")
###some missing FIPS codes in county file I made - coming between puerto rico and death de-identified
cdat<-cdat%>%filter(FIPS!="")
missing.race<-data.table::dcast(setDT(cdat), FIPS+year~race, value.var="cases", fun.aggregate=sum)
missing.race<-missing.race%>%dplyr::select(c(FIPS, year, missing))%>%
  rename(missing.race=missing)
cdat<-left_join(cdat, missing.race)%>%filter(race%in%c("ai", "all", "blk", "wht"))


### think about incorporating reported intervals into measurement error models
pov<-read.csv("saipe.csv", colClasses=c(rep("numeric", 7), "character","character",
                                        rep("numeric", 7), "character"))
pov$race<-"all"
pov<-pov%>%dplyr::select(-c(stname, state))
pov<-pov%>%dplyr::select(child.pov, child.pov.se, FIPS, race, year, median.hh.income)



### infmort censored for small pops, could search for max(blk.pop, ai.pop) for county years and assign that way, but not perfect - fuck it
### script to ID max(nonwhtpop) by county for race assignment, too much censored missing
# pop.test<-pop%>%dplyr::select(year, FIPS, race, child.pop)%>%
#   filter(!(race%in%c("all", "wht", "lat", "aa")))%>%distinct()
# 
# inf.mort.key<-data.frame("FIPS"=NA, "year"=NA, "race.key"=NA)
# for(i in 1:length(unique(pop.test$FIPS))){
#   temp<-pop.test%>%filter(FIPS==unique(pop.test$FIPS)[i])
#   for(j in 1:length(unique(temp$year))){
#     temp2<-temp%>%filter(year==unique(temp$year)[j])
#     max.race<-temp2[order(temp2$child.pop, decreasing=TRUE), ][1, "race"]
#     inf.mort.key<-rbind(inf.mort.key, c(unique(pop.test$FIPS)[i], unique(temp$year)[j], max.race))
#   }
#   print(i)
# }
# 
# inf.mort.key<-inf.mort.key[-1,]
# inf.mort.key$year<-as.numeric(inf.mort.key$year)

inf.mort<-read.csv("infmort-clean.csv", colClasses = c(rep("character",3),
                                                       rep("numeric", 4)))
#### GOING TO KEEP WIDE BECAUSE OF MISMATCH ON RACE, USE DUMMIES TO ZERO OUT NONWHT/WHT in models
#inf.mort<-gather(inf.mort, race, mort,-c(FIPS, state, county, year))
# inf.mort.test<-left_join(inf.mort, inf.mort.key)
# inf.mort.test$race<-ifelse(inf.mort.test$race=="infmort", "all",
#                       ifelse(inf.mort.test$race=="wht.infmort", "wht",
#                              ifelse(inf.mort.test$race=="nonwht.infmort", inf.mort.test$race.key, inf.mort.test$race)))

cname<-inf.mort%>%dplyr::select(FIPS, state, county)
cname<-unique(cname)
#inf.mort<-inf.mort.test%>%dplyr::select(-c(race.key))

### consider using infmort at state level? probably best given huge missing in county
### filter only state data
inf.mort<-inf.mort%>%filter(substr(FIPS, 3, 5)=="000")%>%dplyr::select(-c(FIPS, county))

                            
###nhgis poverty data, includes standard errors for estimates
nhgis<-read.csv("nhgis-chpov.csv", colClasses=c("character", "numeric",
                                                "character", "numeric",
                                                "numeric"))
names(nhgis)[4:5]<-c("child.pov", "child.pov.se")
#### data are 5 yr, currently using 1st yr as year, add 1 for better match
nhgis$year<-nhgis$year+1

density<-read.csv("density.csv",
                  colClasses = c(rep("character", 7),
                                 rep(NA, 7)))
names(density)[5]<-"FIPS"
names(density)[12]<-"land.area"
density<-density%>%dplyr::select(FIPS, land.area)
density<-left_join(pop%>%filter(gender=="all", race=="all")%>%
                     mutate(tot.pop=adult.pop+child.pop), density)
density$pop.density<-density$tot.pop/density$land.area
density<-density%>%dplyr::select(year, FIPS, pop.density)

cdat[which(cdat$FIPS=="12025"), "FIPS"]<-"12086" ##FIX MIAMI-DADE RECODE

ucr[which(ucr$FIPS=="12025"), "FIPS"]<-"12086" ##FIX MIAMI-DADE RECODE
ucr.officers<-ucr%>%filter(race=="all", offense=="all", gender=="all")%>%dplyr::select(FIPS, year, officers, MURDER_mav)
pop.officers<-pop%>%filter(race=="all", gender=="all")%>%dplyr::select(year, FIPS, adult.pop)
ucr.officers<-left_join(ucr.officers, pop.officers)%>%mutate(officers.pc=officers/adult.pop, murder.pc=MURDER_mav/adult.pop)%>%
  dplyr::select(-adult.pop, -officers, -MURDER_mav)
ucrtest<-left_join(ucr, ucr.officers, by=c("FIPS", "year"))

pop.dens<-left_join(pop, density)
### bring in budget data
### make composition vars
pop.dens%>%filter(gender=="all")%>%
  dplyr::select(FIPS, year, adult.pop, race)%>%spread(., race, adult.pop)

pop.pct<-left_join(pop.dens,pop.dens%>%filter(gender=="all")%>%
                     dplyr::select(FIPS, year, adult.pop, race)%>%spread(., race, adult.pop))%>%
  filter(gender=="all")%>%mutate(pct.race.pop=adult.pop/all)%>%
  dplyr::select(year, FIPS, race, pct.race.pop)%>%filter(race!="all")
  
  
#mutate(pct.blk=blk/all, pct.ai=ai/all, pct.wht=wht/all)%>%dplyr::select(FIPS, year, pct.blk, pct.ai, pct.wht)


pop.dens<-left_join(pop.dens, pop.pct)



pov.full<-full_join(pov, nhgis)

m<-left_join(cdat, pop.dens)
m0<-left_join(m, ucrtest)
m1<-left_join(m0, pov.full)
m2<-left_join(m1, cname)
dat<-left_join(m2, inf.mort)
dat$stname<-dat$state
dat$state<-substr(dat$FIPS, 1, 2)

pol.budget<-read.csv("police-budget.csv", stringsAsFactors = FALSE, colClasses = c("character", "numeric", "numeric"))
dat<-left_join(dat, pol.budget)

#### join all FIPS last, join in descending order of complexity

dat<-dat%>%filter(substr(FIPS, 3, 5)!="000")%>%filter(substr(FIPS, 3, 5)!="999")
dat<-dat%>%filter(substr(FIPS, 1, 2)!="72")

z<-which(is.na(dat$state))

dat<-dat%>%filter(race%in%c("ai", "all", "blk", "wht"))

write.csv(dat, "ncands-fc-merge.csv", row.names=FALSE)