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
                                                  ifelse((ChRacWh==1&CEthn!=1), "wht", "missing"))))))
  
  dat$race[is.na(dat$race)]<-"missing"
  
  ###################### MAKE COUNTS FOR WIDE (county-year)
  ### RPTSRC==4 is police
  #count all police reports by race, set up wide, filter unused groups
  
    police.race<-dat%>%filter(race%in%c("blk", "ai", "wht", "missing"))%>%
      group_by(RptFIPS, SubYr, race)%>%filter(RptSrc==4)%>%
      count()%>%rename(cases=n)
    police.race<-spread(police.race, race, cases, fill=0, sep=".")
    names(police.race)[3:ncol(police.race)]<-paste("polrpt.", 
      substr(names(police.race)[3:ncol(police.race)], 6, nchar(names(police.race)[3:ncol(police.race)])), sep="")
    
  #count all reports by race, set up wide
  
    total.race<-dat%>%filter(race%in%c("blk", "ai", "wht", "missing"))%>%
      group_by(RptFIPS, SubYr, race)%>%count()%>%rename(cases=n)
    total.race<-spread(total.race, race, cases, fill=0, sep=".")
    names(total.race)[3:ncol(total.race)]<-paste("totalrpt.", 
      substr(names(total.race)[3:ncol(total.race)], 6, nchar(names(total.race)[3:ncol(total.race)])), sep="")
    
  #count all missing reports by race
  
    totalNArace<-dat%>%filter(race%in%c("blk", "ai", "wht", "missing"))%>%
      group_by(RptFIPS, SubYr, race)%>%filter((RptSrc==99)|(is.na(RptSrc))|(RptSrc==0))%>%
      count()%>%rename(cases=n)
    totalNArace<-spread(totalNArace, race, cases, fill=0, sep=".")
    names(totalNArace)[3:ncol(totalNArace)]<-paste("NArpt.", 
      substr(names(totalNArace)[3:ncol(totalNArace)], 6, nchar(names(totalNArace)[3:ncol(totalNArace)])), sep="")

  #count all police reports
    
    police<-dat%>%group_by(RptFIPS, SubYr)%>%filter(RptSrc==4)%>%
      count()%>%rename(polrpt=n)
    
  #count all reports
    
    total<-dat%>%group_by(RptFIPS, SubYr)%>%count()%>%rename(totalrpt=n)
  
  #count all missing reports
  
    totalNA<-dat%>%group_by(RptFIPS, SubYr)%>%filter((RptSrc==99)|(is.na(RptSrc))|(RptSrc==0))%>%
    count()%>%rename(NArpt=n)
  
  out<-full_join(full_join(full_join(full_join(full_join(
    police, total),totalNA),police.race),total.race),totalNArace)
  
  ### missings are true zeroes - not in data. can use totlrpt.missing, polrpt.missing to upper bound
  out[is.na(out)]<-0  
  out<-out%>%rename(FIPS=RptFIPS, year=SubYr)

  ###FILTER OUT DEIDENTIFIED, PUERTO RICO
  out<-out%>%filter(substr(FIPS, 3, 5)!="000")%>%filter(substr(FIPS, 3, 5)!="999")%>%
  filter(substr(FIPS, 1, 2)!="NA")
  out<-out%>%filter(substr(FIPS, 1, 2)!="72")
  ###some missing FIPS codes in county file I made - coming between puerto rico and death de-identified
  out<-out%>%filter(FIPS!="")
  
  ### return wide format FIPS-year ncands pol and total rpt counts by race

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



# ### FILTER de-identified state (fatalities) and Puerto Rico
# county.test<-county.out%>%ungroup()%>%
#   mutate(RptSrc=as.character(RptSrc))%>%as.data.frame()
# # 
# ### recode RptSrc into named cats for ease of interpretation
# ### Fill zeroes for combinations without observations in data
# county.test<-county.test%>%complete(FIPS, year, race, RptSrc, fill=list(cases=0))
# ### then filter out counties that are de-identified
# z<-county.test%>%filter(RptSrc=="total"&race=="all")%>%filter(cases==0)%>%select(FIPS, year)
# z$drop<-TRUE
# ctest<-left_join(county.test, z)
# ctest<-ctest%>%filter(is.na(drop))



### write out
write.csv(county.out, file="ncands-wide-04-12.csv", row.names=FALSE)
q(save="no")