rm(list=ls())
library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
set.seed(1)

source("H:/ncands-fc/ncandsfunctions.r")

setwd("H:/data")
source("H:/ncands-fc/ncandsread.r")

files<-c("Child2012.csv")
#, "Child2011.csv",	"Child2010.csv",	"Child2009.csv", 	"Child2008.csv","Child2007.csv", 	"Child2006.csv", "Child2005.csv", 	"Child2004.csv","Child2003.csv", 	"Child2002.csv", "Child2001.csv",	"Child2000.csv")

### 2006, 2004 are fully missing on RPT vars, 2011 needs to be included

year<-c(2012)
  #, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003, 2002, 2001, 2000)

cnty.out<-list()
#for(i in (1:length(files))){
  i<-1
  dat<-fread(files[i])
  names(dat)<-tolower(names(dat))
  dat$rptsrc<-addNA(factor(dat$rptsrc))

#### New strategy - use tidyr/dplyr to turn vars (rptsrc, others) into factor, spread into columns
#### Can eyeball data quality by county from there, leave transformation out to analysis script
  cnty.rpt<-dat%>%
    group_by(rptfips)%>%
    count(rptsrc, rptfips)%>%
    spread(rptsrc,n)


# if(year[i]==2012|year[i]==2011|year[i]==2007|year[i]==2005){
# dat<-dat%>%mutate(RptSrc=as.character(RptSrc)) 
# cnty.rpt<- dat %>%
# 	group_by(RptFIPS, StaTerr)%>%
# 	summarise(tot.rpt=n(),
# 	          unique.reports=n_distinct(RptID),
# 	          victims=sum(RptVictim==1),
# 	          rpt.inf=sum(RptSrc==12|RptSrc==8|RptSrc==13|RptSrc==11|
# 	            RptSrc==88|RptSrc==10|RptSrc==9),
# 	          rpt.daycr=sum(RptSrc==6),
# 	          rpt.edu=sum(RptSrc==5),
# 	          rpt.cj=sum(RptSrc==4),
# 	          rpt.med=sum(RptSrc==2),
# 	          rpt.mh=sum(RptSrc==3),
# 	          rpt.socserv=sum(RptSrc==1),
# 	          rpt.foster=sum(RptSrc==7))%>%
#   mutate(year=year[i])
# }

# if(year[i]==2010|year[i]==2009|year[i]==2008|
#    year[i]==2006){
#   dat<-dat%>%mutate(RptSrc=as.character(RptSrc)) 
#   cnty.rpt<- dat %>%
#     group_by(RptFIPS, StaTerr)%>%
#     summarise(tot.rpt=n(),
#               unique.reports=n_distinct(RptID),
#               victims=sum(IsVictim==1),
#               rpt.inf=sum(RptSrc==12|RptSrc==8|RptSrc==13|RptSrc==11|
#                             RptSrc==88|RptSrc==10|RptSrc==9),
#               rpt.daycr=sum(RptSrc==6),
#               rpt.edu=sum(RptSrc==5),
#               rpt.cj=sum(RptSrc==4),
#               rpt.med=sum(RptSrc==2),
#               rpt.mh=sum(RptSrc==3),
#               rpt.socserv=sum(RptSrc==1),
#               rpt.foster=sum(RptSrc==7))%>%
#     mutate(year=year[i])
# }

# if(year[i]==2004){
#   dat<-dat%>%mutate(RptSrc=as.character(RptSrc)) 
#   cnty.rpt<- dat %>%
#     group_by(RptCnty, StaTerr)%>%
#     summarise(tot.rpt=n(),
#               unique.reports=n_distinct(RptID),
#               victims=sum(RptVictim==1),
#               rpt.inf=sum(RptSrc==12|RptSrc==8|RptSrc==13|RptSrc==11|
#                             RptSrc==88|RptSrc==10|RptSrc==9),
#               rpt.daycr=sum(RptSrc==6),
#               rpt.edu=sum(RptSrc==5),
#               rpt.cj=sum(RptSrc==4),
#               rpt.med=sum(RptSrc==2),
#               rpt.mh=sum(RptSrc==3),
#               rpt.socserv=sum(RptSrc==1),
#               rpt.foster=sum(RptSrc==7))%>%
#     mutate(year=year[i])
# }

# if(year[i]==2003){
#   dat<-dat%>%mutate(rptsrc=as.character(rptsrc)) 
#   cnty.rpt<- dat %>%
#     group_by(rptcnty, staterr)%>%
#     summarise(tot.rpt=n(),
#               unique.reports=n_distinct(rptid),
#               victims=sum(IsVictim==1),
#               rpt.inf=sum(rptsrc==12|rptsrc==8|rptsrc==13|rptsrc==11|
#                             rptsrc==88|rptsrc==10|rptsrc==9),
#               rpt.daycr=sum(rptsrc==6),
#               rpt.edu=sum(rptsrc==5),
#               rpt.cj=sum(rptsrc==4),
#               rpt.med=sum(rptsrc==2),
#               rpt.mh=sum(rptsrc==3),
#               rpt.socserv=sum(rptsrc==1),
#               rpt.foster=sum(rptsrc==7))%>%
#     mutate(year=year[i])
  
# }

# if(year[i]==2002|year[i]==2001|year[i]==2000){
#   names(dat)<-tolower(names(dat))
#   dat<-dat%>%mutate(rptsrc=as.character(rptsrc)) 
#   cnty.rpt<- dat %>%
#     group_by(rptcnty, staterr)%>%
#     summarise(tot.rpt=n(),
#               unique.reports=n_distinct(rptid),
#               victims=sum(rptdisp==1|rptdisp==2|rptdisp==3),
#               rpt.inf=sum(rptsrc==12|rptsrc==8|rptsrc==13|rptsrc==11|
#                             rptsrc==88|rptsrc==10|rptsrc==9),
#               rpt.daycr=sum(rptsrc==6),
#               rpt.edu=sum(rptsrc==5),
#               rpt.cj=sum(rptsrc==4),
#               rpt.med=sum(rptsrc==2),
#               rpt.mh=sum(rptsrc==3),
#               rpt.socserv=sum(rptsrc==1),
#               rpt.foster=sum(rptsrc==7))%>%
#     mutate(year=year[i])
# }
# cnty.rpt<-as.data.frame(cnty.rpt)
# names(cnty.rpt)[1:2]<-c("RptFIPS", "StaTerr")
# cnty.out[[i]]<-cnty.rpt
# rm(dat)
# }


county.out<-do.call("rbind", cnty.out)
write.csv(county.out, "county.csv")
q(save="no")
