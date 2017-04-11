rm(list=ls())

#### DATA FROM NIH SEER
#### https://seer.cancer.gov/popdata/download.html

.libPaths( c( .libPaths(), "U:/R") )

library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
library(ggplot2)
library(foreign)
set.seed(1)

setwd("R:/Project/NCANDS/ncands-csv/")

# With reduced age categories
# seer<-read_fwf(file="us.1990_2015.19ages.txt", 
#         fwf_widths(c(4,2,2,3,2,1,1,1,2,8)),
#         col_types="icccciiiii")
# names(seer)<-c("year", "state", "state.fips",
#                      "cnty.fips", "registry", 
#                      "race", "latino", "sex",
#                      "age", "pop")

# with single year age categories
seer<-read_fwf(file="us.1990_2015.singleages.txt", 
               fwf_widths(c(4,2,2,3,2,1,1,1,2,8)),
               col_types="icccciiiii")
names(seer)<-c("year", "state", "state.fips",
               "cnty.fips", "registry", 
               "race", "latino", "sex",
               "age", "pop")

seer$FIPS<-paste(seer$state.fips, seer$cnty.fips, sep="")

seer<-seer%>%mutate(child=age<18, adult=age>17)
seer$race<-ifelse(seer$race==2, "blk",
                  ifelse(seer$race==3, "ai",
                  ifelse(seer$latino==1, "lat",
                  ifelse(seer$race==1, "wht",
                  ifelse(seer$race==4, "aa", seer$race)))))
seer<-seer%>%dplyr::select(-c(state, state.fips, cnty.fips, registry, latino, age))
seer<-seer%>%filter(year>1999)


seer.tot<-seer%>%group_by(year, FIPS, child)%>%summarise(pop=sum(pop))%>%mutate(race="all", sex=0)
seer.tot<-spread(seer.tot, child, pop)%>%rename(adult=`FALSE`, child=`TRUE`)%>%dplyr::select(-race, -sex)


seer.race<-seer%>%filter(race%in%c("ai", "blk", "wht"))%>%
  group_by(year, FIPS, child, race)%>%summarise(pop=sum(pop))

seer.race.adult<-seer.race%>%filter(child==FALSE)%>%spread(race, pop, sep=".")%>%
  ungroup%>%dplyr::select(-child)
names(seer.race.adult)[grep("race.", names(seer.race.adult))]<-
  paste("adult.", substr(names(seer.race.adult)[grep("race.", names(seer.race.adult))], 6, 
  nchar(names(seer.race.adult)[grep("race.", names(seer.race.adult))])), sep="")

seer.race.child<-seer.race%>%filter(child==TRUE)%>%spread(race, pop, sep=".")%>%
  ungroup%>%dplyr::select(-child)
names(seer.race.child)[grep("race.", names(seer.race.child))]<-
  paste("child.", substr(names(seer.race.child)[grep("race.", names(seer.race.child))], 6, 
  nchar(names(seer.race.child)[grep("race.", names(seer.race.child))])), sep="")


seer.gender<-seer%>%group_by(year, FIPS, child, sex)%>%summarise(pop=sum(pop))%>%
  mutate(race="all")
seer.gender<-seer.gender%>%ungroup%>%filter(child==FALSE)%>%dplyr::select(-child, -race)%>%
  spread(sex, pop, sep="")%>%rename(men=sex1, women=sex2)

seer.out<-full_join(full_join(full_join(
  seer.tot, seer.gender), seer.race.adult), seer.race.child)


write.csv(seer.out, file="seer-pop.csv", row.names=FALSE)