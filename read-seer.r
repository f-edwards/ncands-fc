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


seer.tot<-seer%>%group_by(year, FIPS, child)%>%summarise(pop=sum(pop))%>%mutate(race="all", sex=0)
seer.race<-seer%>%group_by(year, FIPS, child, race)%>%summarise(pop=sum(pop))%>%mutate(sex=0)
seer.gender<-seer%>%group_by(year, FIPS, child, sex)%>%summarise(pop=sum(pop))%>%
  mutate(race="all")
seer.out<-full_join(full_join(seer.tot, seer.race), seer.gender)
seer.child<-seer.out%>%
  mutate(child.pop=child*pop)%>%filter(child==TRUE)%>%dplyr::select(-c(pop, child))%>%
  filter(sex==0)%>%select(-sex)
seer.tot<-seer.out%>%filter(child==FALSE)%>%group_by(year, FIPS,race, sex)%>%summarise(adult.pop=sum(pop))

seer.all<-left_join(seer.tot, seer.child)

write.csv(seer.all, file="seer-pop.csv", row.names=FALSE)