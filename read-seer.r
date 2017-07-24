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
seer<-seer%>%filter(year>2001)
seer<-seer%>%
  group_by(year, FIPS, child, race)%>%
  summarise(pop=sum(pop))%>%
  ungroup()

seer<-seer%>%
  bind_rows(
    seer%>%
    group_by(year, FIPS, child)%>%
    summarise(pop=sum(pop))%>%
    mutate(race="tot"))

seer<-seer%>%
  spread(child, pop)%>%
  rename(adult=`FALSE`, child=`TRUE`)%>%
  complete(race, nesting(year, FIPS), fill=list(adult=0, child=0))

write.csv(seer, file="seer-pop.csv", row.names=FALSE)