rm(list=ls())
require(TTR)
require(dplyr)
library(tidyr)
require(Amelia)
library(nlme)
library(lme4)
library(ggplot2)
library(arm)

set.seed(1)
setwd("H:/Data")

### read county level malt rpt data
mal<-read.csv("county.csv", stringsAsFactors = FALSE)
mal<-mal[,-1]
names(mal)[1:2]<-c("FIPS", "state")


### read cdc child pop data - manually removed county 'total' rows and notes column
pop<-read.table("CDC-Wonder-County-Kids.tsv", sep="",
                stringsAsFactors = FALSE, head=TRUE)
names(pop)<-c("cname", "FIPS", "year", "year.dup", "child.pop")
### pull out state abrev from "NAME County, ST format in var
z<-do.call("rbind",(strsplit(pop$cname, ", ")))
pop$cname<-z[,1]
pop$state<-z[,2]

rpt<-left_join(mal, pop, by=c("FIPS", "year", "state"))
### DROP ALL PR, STATE SUMMARIES - ALL DATA MISSING AFTER CDC JOIN ARE NON U.S. COUNTIES
### ONLY HAVE DATA FOR 2007-2011
### RESTRICTING TO BIG POP COUNTIES (child.pop > median)
rpt<-rpt%>%
  filter(!(is.na(child.pop)))%>%
  filter(year>2006)%>%
  filter(year<2012)%>%
  filter(child.pop>median(child.pop))

emp<-read.csv("county-emp.csv", head=TRUE, stringsAsFactors = FALSE)
names(emp)[1]<-"stname"
z<-which(emp$totemp==0)
z.1<-which(emp$lawenf==0)
z.2<-which(emp$edu==0)

emp[c(z, z.1, z.2), 7:11]<-NA
### TREAT ZERO EMPLOYMENT AS NA

rpt<-left_join(rpt, emp, by=c("FIPS", "year"))

### Using 3-yr averages over time, can also use 5-yr file from NHGIS for point in time
### MANY COUNTIES NOT PRESENT IN ACS ROLLING ESTIMATES FROM PUMS - SAMPLE EXCLUDES A LOT OF DATA POINTS
acs<-read.csv("H:/census/acs-county-2007-12.csv", head=TRUE)
names(acs)[c(1, 2, 3)]<-c("FIPS.st", "FIPS.co", "year")
rpt<-left_join(rpt, acs, by=c("FIPS.st", "FIPS.co", "year"))

rpt<-rpt%>%
  mutate(rpts.pc=tot.rpt/child.pop,
         ch.pov.rt=chpov/child.pop)

### ID COUNTIES WITH MISSING EMP DATA, PRESENT RPT DATA
z<-which(is.na(rpt$totemp))
missing<-data.frame("st"=rpt[z,"state"], "county"=rpt[z,"cname"], "year"=rpt[z,"year"])

### ID COUNTIES WITH MISSING RPTSRC DATA
z.1<-which(is.na(rpt$rpt.inf))
missing.rpt<-data.frame("st"=rpt[z.1,"state"], "county"=rpt[z.1,"cname"], "year"=rpt[z.1,"year"])

### ID COUNTIES WITH MISSING POP DATA
z.2<-which(is.na(rpt$ch.pov.rt))
missing.acs<-data.frame("st"=rpt[z.2,"state"], "county"=rpt[z.2,"cname"], "year"=rpt[z.2,"year"])

### MAP MISSINGNESS
missmap(rpt)

z<-sample(unique(rpt$FIPS), 50, replace=FALSE)

samp.plot<-rpt%>%
  filter(FIPS%in%z)

ggplot(data=samp.plot,
       aes(x=year, y=scale(ch.pov.rt)))+
  geom_point()+
  facet_wrap(~county)+
  theme_bw()


ggplot(data=samp.plot,
       aes(x=year, y=scale(rpts.pc)))+
  geom_point()+
  facet_wrap(~county)+
  theme_bw()

ggplot(data=samp.plot,
       aes(x=year, y=scale(edu/child.pop)))+
  geom_point()+
  facet_wrap(~county)+
  theme_bw()

### Impute
m<-round((sum(is.na(rpt$ch.pov.rt))/nrow(rpt))*100)
bounds<-cbind(1:ncol(rpt), rep(0, ncol(rpt)), rep(Inf, ncol(rpt)))
rpt$year.c<-rpt$year-2007
rpt.imp<-amelia(rpt, ts="year", cs="FIPS", idvars=c("state", "year.dup", "cname", "stname", "county",
                                                "FIPS.st", "FIPS.co", "year.c"),
                m=28, polytime=1, empri=0.01*nrow(rpt), bounds=bounds)

### Unconditional growth model for reports per capita
m0<-lme(fixed=rpts.pc~year.c,
        random=~year.c|FIPS, 
        data=rpt, na.action="na.omit")

#### GOT A TON OF MODELS TO WRITE - put them here, loop over, merge results

gamma_imp <- matrix(0, nrow=rpt.imp$m, ncol=length(fixef(m0)))
gamma_SE_imp <- matrix(0, nrow=rpt.imp$m, ncol=length(fixef(m0)))
colnames(gamma_imp) <- colnames(gamma_SE_imp) <- names(fixef(m0))


for(i in (1:m)){
  dat<-rpt.imp$imputations[[i]]
  m0.i<-lme(fixed=rpts.pc~year.c,
           random=~year.c|FIPS, 
           data=dat)

#   m1<-lme(fixed=scale(rpts.pc)~year.c+scale(I(totemp/child.pop)),
#              random=~year.c|FIPS,
#              data=dat)
  
  gamma_imp[i,]<-fixef(m0.i)
  gamma_SE_imp[i,]<-summary(m0.i)$tTable[,"Std.Error"]
}

