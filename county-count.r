rm(list=ls())
library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
set.seed(1)

source("H:/ncands-fc/ncandsfunctions.r")

setwd("H:/data")
source("H:/ncands-fc/ncandsread.r")

key<-read.csv("H:/NCANDS-clean/ncandskey2012.csv", head=TRUE)
files<-c("Child2012v1.dat"
	, "Child2010v1a.dat",
	"Child2009v2a.dat", "Child2008v3a.dat",
	"Child2007v2a.dat", "Child2006_v1a.dat",
	"Child2005v1a.dat", "Child2004_v1a.dat")

year<-c(2012, 2010, 2009, 2008, 2007, 2006, 2005, 2004)

cnty.out<-list()
for(i in (1:length(files))){
dat<-ncands.fwf(dat=files[i], "H:/ncands-fc/ncandskey2012.csv")
dat<-dat%>%mutate(RptSrc=as.character(RptSrc)) 
cnty.rpt<- dat %>%
	group_by(RptFIPS, StaTerr)%>%
	summarise(tot.rpt=n(),
	          unique.reports=n_distinct(RptID),
	          victims=sum(RptVictim==1),
	          rpt.inf=sum(RptSrc==12|RptSrc==8|RptSrc==13|RptSrc==11|
	            RptSrc==88|RptSrc==10|RptSrc==9),
	          rpt.daycr=sum(RptSrc==6),
	          rpt.edu=sum(RptSrc==5),
	          rpt.cj=sum(RptSrc==4),
	          rpt.med=sum(RptSrc==2),
	          rpt.mh=sum(RptSrc==3),
	          rpt.socserv=sum(RptSrc==1),
	          rpt.foster=sum(RptSrc==7))%>%
  mutate(year=year[i])
cnty.out[[i]]<-cnty.rpt
rm(dat)
}

county.out<-do.call("rbind", cnty.out)
write.csv(county.out, "county.csv")
q(save="no")
