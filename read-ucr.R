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

###UCR READ
### load 2012 UCR, loads object da35018.001
dir<-"R:/Project/NCANDS/ucr/"
files<-c("ICPSR_03729/ICPSR_03729/DS0001/03729-0001-Data.dta",
         "ICPSR_03997/ICPSR_03997/DS0001/03997-0001-Data.dta",
         "ICPSR_04068/ICPSR_04068/DS0001/04068-0001-Data.dta",
         "ICPSR_04461/ICPSR_04461/DS0001/04461-0001-Data.dta",
         "ICPSR_04716/ICPSR_04716/DS0001/04716-0001-Data.dta",
         "ICPSR_22405/ICPSR_22405/DS0001/22405-0001-Data.dta",
         "ICPSR_25106/ICPSR_25106/DS0001/25106-0001-Data.dta",
         "ICPSR_27643/ICPSR_27643/DS0001/27643-0001-Data.dta",
         "ICPSR_27651/ICPSR_27651/DS0001/27651-0001-Data.dta",
         "ICPSR_30762/ICPSR_30762/DS0001/30762-0001-Data.dta",
         "ICPSR_33522/ICPSR_33522/DS0001/33522-0001-Data.dta",
         "ICPSR_34581/ICPSR_34581/DS0001/34581-0001-Data.dta",
         "ICPSR_35018/ICPSR_35018/DS0001/35018-0001-Data.dta",
         "ICPSR_36116/ICPSR_36116/DS0001/36116-0001-Data.dta",
         "ICPSR_36400/ICPSR_36400/DS0001/36400-0001-Data.dta")
files<-paste(dir, files, sep="")
ucr<-list()
for(i in 1:length(files)){
  ucr[[i]]<-read_dta(files[[i]])
  print(i)
}

ucr.dat<-bind_rows(list(ucr[[1]], ucr[[2]], ucr[[3]], ucr[[4]], ucr[[5]], ucr[[6]],
                        ucr[[7]], ucr[[8]], ucr[[9]], ucr[[10]], ucr[[11]], ucr[[12]],
                        ucr[[13]], ucr[[14]]))

names(ucr.dat)[which(names(ucr.dat)=="ORI")]<-"ORI7"

ucr.dat[ucr.dat==999998]<-NA
ucr.dat[ucr.dat=="999998]"]<-NA

### load UCR crosswalk to convert into FIPS, ORI in UCR, ORI7 in crosswalk should match
crosswalk<-read.delim("35158-0001-Data.tsv", sep="\t", colClasses=c(rep(NA, 3), rep("character", 3), rep(NA, 40)),
                      stringsAsFactors = FALSE)

ucr.fips<-left_join(ucr.dat, crosswalk, by="ORI7")

####Create county-level counts of arrest by offense type, want total arrest, violent arrest, fam viol arrest, 
####QOL arrests, drug arrests, subset by race / gender later
### get indices
age.index<-c(which(names(ucr.fips)=="M0_9"), which(names(ucr.fips)=="F65"))
race.index<-c(which(names(ucr.fips)=="JW"), which(names(ucr.fips)=="AN"))

ucr.fips$total.age<-apply(ucr.fips[,age.index[1]:age.index[2]], 1, sum)
#ucr.fips$total.race<-apply(ucr.fips[,race.index[1]:race.index[2]], 1, sum)
### Age is more inclusive, many more arrests counted, appears race underrreported
### table(ucr.fips$total.age>ucr.fips$total.race)

ucr.county.offense<-ucr.fips%>%group_by(FIPS, COUNTYNAME, OFFENSE, YEAR)%>%
  summarise(tot.arrest=sum(total.age, na.rm=TRUE), pop=sum(POP, na.rm=TRUE))
names(ucr.county.offense)[which(names(ucr.county.offense)=="OFFENSE")]<-"code"

ucr.county.tot<-ucr.county.offense%>%group_by(FIPS, COUNTYNAME, YEAR)%>%
  summarise(arrests=sum(tot.arrest), pop=max(pop))

ucr.offense<-read.csv("ucr-offense-key.csv", stringsAsFactors = FALSE)
ucr.offense$code<-substr(ucr.offense[,1],1, 3)
ucr.offense$offense<-substr(ucr.offense[,1], 4, nchar(ucr.offense[,1]))
ucr.offense$code[which(ucr.offense$code=="18 ")]<-"18"
ucr.offense<-ucr.offense[,-1]

ucr.county.offense<-left_join(ucr.county.offense, ucr.offense)

write.csv(ucr.county.offense, file="ucr-county-offense.csv", row.names=FALSE)