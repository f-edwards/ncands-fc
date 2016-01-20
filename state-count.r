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

### measurement and matching problems for year < 2006 when variable definition changes - 
### will need to match on state + rptcnty for 2000-2005 if I want to use those.  
### have to preserve fips as character to keep leading zeroes, for earlier years, no rptfips variable, 
### rptcnty as three digit county code (w/0 2 digit state, incl -1)

year<-c(2012:2000)

cnty.out<-NULL
for(i in (1:length(files))){
  dat<-fread(files[i], colClasses="character", na.strings="")
  names(dat)<-tolower(names(dat))
  if("isvictim"%in%names(dat)){
    names(dat)[which(names(dat)=="isvictim")]<-"rptvictim"
  }
  if(!("rptvictim"%in%names(dat))){
    dat$rptvictim<-with(dat,(rptdisp=="1"|rptdisp=="2"|rptdisp=="3"))
  }
  dat$rptsrc[which(is.na(dat$rptsrc))]<-"N.A"
  dat$rptsrc<-factor(dat$rptsrc)
  
  #### New strategy - use tidyr/dplyr to turn vars (rptsrc, others) into factor, spread into columns
  #### Can eyeball data quality by county from there, leave transformation out to analysis script
  st.rpt<-dat%>%
    group_by(staterr)%>%
    count(rptsrc)%>%
    spread(rptsrc,n)
  st.rpt[is.na(st.rpt)]<-0
  names(st.rpt)[2:ncol(st.rpt)]<-paste("src", names(st.rpt)[2:ncol(st.rpt)], sep="")
  temp<-left_join(st.rpt,
                  dat%>%
                    group_by(staterr)%>%
                    summarise(tot.rpt=n(),
                              unique.reports=n_distinct(rptid),
                              victims=sum(rptvictim=="1"))%>%
                    mutate(non.victim=tot.rpt-victims)%>%
                    mutate(year=year[i]), by="staterr")
  
  temp<-as.data.frame(temp)
  cnty.out<-bind_rows(cnty.out, temp)
  rm(dat)
}

write.csv(cnty.out,"strpt00-12.csv", row.names=FALSE)

# dat<-NULL

# for(i in 1:length(year)){
#   file<-paste("cnty", year[i], ".csv", sep="")
#   write.csv(cnty.out[[i]], file, row.names=FALSE)
#   dat<-bind_rows(dat, read.csv(file))
#   
# }



#q(save="no")
