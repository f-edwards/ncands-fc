rm(list=ls())
gc()
.libPaths( c( .libPaths(), "U:/R") )

library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(foreign)
library(haven)
set.seed(1)

setwd("R:/Project/NCANDS/ncands-csv/")
ncands.county<-function(dat){
  dat$race<-with(dat, ifelse(ChRacBl==1, "blk",
                             ifelse(ChRacAI==1, "ai",
                                    ifelse(ChRacNH==1, "hawpi",
                                           ifelse(ChRacAs==1, "aa",
                                                  ifelse(ChRacWh==1, "wht", "missing"))))))
  
  dat$race[is.na(dat$race)]<-"missing"
  
  total<-dat%>%group_by(RptFIPS, SubYr, race)%>%
    count(RptSrc)%>%rename(cases=n)
  
  totalNA<-total%>%group_by(RptFIPS, SubYr, race)%>%filter((RptSrc==99)|(is.na(RptSrc))|(RptSrc==0))%>%
    summarise(missing.rpt=sum(cases))
  
  total.race<-dat%>%group_by(RptFIPS, SubYr)%>%
    count(RptSrc)%>%rename(cases=n)%>%mutate(race="all")
  totalNA.race<-total%>%group_by(RptFIPS, SubYr, RptSrc)%>%filter((RptSrc==99)|(is.na(RptSrc))|(RptSrc==0))%>%
    summarise(missing.rpt=sum(cases))%>%mutate(race="all")%>%select(-RptSrc)
  total.race<-left_join(total.race, totalNA.race)
  ###create rptsrc==98 for all reports
  total.rptsrc<-dat%>%group_by(RptFIPS, SubYr)%>%
    summarise(cases=n())%>%mutate(RptSrc=98)%>%mutate(race="all")
  totalNA.rptsrc<-total%>%group_by(RptFIPS, SubYr)%>%filter((RptSrc==99)|(is.na(RptSrc))|(RptSrc==0))%>%
    summarise(missing.rpt=sum(cases))%>%mutate(RptSrc=98)%>%mutate(race="all")
  total.rptsrc<-left_join(total.rptsrc, totalNA.rptsrc)
  all.cat<-bind_rows(total.rptsrc, total.race)
  
  out<-left_join(total, totalNA)
  out<-bind_rows(out, all.cat)
  
  out$missing.rpt[is.na(out$missing.rpt)]<-0
  ### filter out missing RptSrc, now tallied in missing.rpt
  out<-out%>%filter(!((RptSrc==99)|(is.na(RptSrc))|(RptSrc==0)))
  ### rather than add total cases column, can group_by(RptFIPS, SubYr)%>%summarise(cases=sum(cases))
  ### in model data
  out<-out%>%rename(FIPS=RptFIPS, year=SubYr)
  return(out)
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



### FILTER de-identified state (fatalities) and Puerto Rico
county.test<-county.out%>%ungroup()%>%
  mutate(RptSrc=as.character(RptSrc))%>%as.data.frame()
county.test$RptSrc<-factor(county.test$RptSrc, 
                           levels=as.character(c(1:13, 88, 98)))
levels(county.test$RptSrc)<-c("socserv","med","menthlth","police","edu","daycare","fostercare",
                              "victim","parent","relative","neighbor","perp","anon","other",
                              "total")

### recode RptSrc into named cats for ease of interpretation

write.csv(county.test, file="ncands-rpt-county-04-12.csv", row.names=FALSE)
q(save="no")