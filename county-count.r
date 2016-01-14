rm(list=ls())
library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
set.seed(1)

setwd("H:/data")
source("H:/ncands-fc/ncandsread.r")

files<-c("Child2012.csv", "Child2011.csv",	"Child2010.csv",	"Child2009.csv", 	"Child2008.csv","Child2007.csv", 	"Child2006.csv", "Child2005.csv", 	"Child2004.csv","Child2003.csv", 	"Child2002.csv", "Child2001.csv",	"Child2000.csv")

year<-c(2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003, 2002, 2001, 2000)

cnty.out<-list()
for(i in (1:length(files))){
  dat<-fread(files[i])
  names(dat)<-tolower(names(dat))
  if("isvictim"%in%names(dat)){
    names(dat)[which(names(dat)=="isvictim")]<-"rptvictim"
  }
  if(!("rptvictim"%in%names(dat))){
    dat$rptvictim<-with(dat,(rptdisp==1|rptdisp==2|rptdisp==3))
  }
  if("rptcnty"%in%names(dat)){
    names(dat)[which(names(dat)=="rptcnty")]<-"rptfips"
  }
  dat$rptsrc[which(is.na(dat$rptsrc))]<-"N.A"
  dat$rptsrc<-factor(dat$rptsrc)

#### New strategy - use tidyr/dplyr to turn vars (rptsrc, others) into factor, spread into columns
#### Can eyeball data quality by county from there, leave transformation out to analysis script
  cnty.rpt<-dat%>%
    group_by(rptfips)%>%
    count(rptsrc, rptfips)%>%
    spread(rptsrc,n)
  cnty.rpt[is.na(cnty.rpt)]<-0
  names(cnty.rpt)[2:ncol(cnty.rpt)]<-paste("src", names(cnty.rpt)[2:ncol(cnty.rpt)], sep="")
  temp<-left_join(cnty.rpt,
            dat%>%
              group_by(rptfips, staterr)%>%
              summarise(tot.rpt=n(),
                        unique.reports=n_distinct(rptid),
                        victims=sum(rptvictim==1))%>%
                        mutate(non.victim=tot.rpt-victims)%>%
                        mutate(year=year[i]), by="rptfips")

  temp<-as.data.frame(temp)
  cnty.out[[i]]<-temp
  rm(dat)
}


for(i in 1:length(year)){
  file<-paste("cnty", year[i], ".csv", sep="")
  write.csv(cnty.out[[i]], file, row.names=FALSE)
}

q(save="no")
