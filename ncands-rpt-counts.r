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



year<-c(2012:2000)

cnty.out<-NULL
st.out<-NULL

for(i in (1:length(year))){
  dat<-fread(files[i], colClasses="character", na.strings="")
  names(dat)<-tolower(names(dat))
  if("isvictim"%in%names(dat)){
    names(dat)[which(names(dat)=="isvictim")]<-"rptvictim"
  }
  if(!("rptvictim"%in%names(dat))){
    dat$rptvictim<-with(dat,(rptdisp=="1"|rptdisp=="2"|rptdisp=="3"))
  }
  dat$rptsrc[which(dat$rptsrc=="0")]<-NA
  dat$rptsrc[which(is.na(dat$rptsrc))]<-"N.A"

### State counts
st.rpt<-dat%>%
  group_by(staterr)%>%
  count(rptsrc, staterr)%>%
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
st.out<-bind_rows(st.out, temp)
  
### measurement and matching problems for year < 2006 when variable definition changes - 
### will need to match on state + rptcnty for 2000-2005 if I want to use those.  
### have to preserve fips as character to keep leading zeroes, for earlier years, no rptfips variable, 
### rptcnty as three digit county code (w/0 2 digit state, incl -1)
  
  if(year[i]>2005){ ### compute county counts for years 2006 and greater
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
                          victims=sum(rptvictim=="1"))%>%
                          mutate(non.victim=tot.rpt-victims)%>%
                          mutate(year=year[i]), by="rptfips")
  
    temp<-as.data.frame(temp)
    cnty.out<-bind_rows(cnty.out, temp)
  }
}

write.csv(cnty.out,"cntyrpt06-12.csv", row.names=FALSE)
write.csv(st.out, "strpt00-12.csv", row.names=FALSE)


