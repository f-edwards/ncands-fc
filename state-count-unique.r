rm(list=ls())
library(readr)
library(dplyr)
library(lme4)
library(texreg)
set.seed(1)

source("H:/ncands-fc/ncandsfunctions.r")

setwd("H:/data")
source("H:/ncands-fc/ncandsread.r")

key<-read.csv("H:/NCANDS-clean/ncandskey2012.csv", head=TRUE)
files<-c("Child2012v1.dat"
	, "Child2010v1a.dat",
	"Child2009v2a.dat", "Child2008v3a.dat",
	"Child2007v2a.dat", "Child2006_v1a.dat",
	"Child2005v1a.dat", "Child2004_v1a.dat"
	)
year<-c(2012, 2010, 2009, 2008, 2007, 2006, 2005, 2004)
state.out.unique.rpt<-list() 

for(i in (1:length(files))){
dat<-ncands.fwf(dat=files[i], "H:/ncands-fc/ncandskey2012.csv")
dat<-ncandsclean(dat)

### ONLY CAPTURES COUNT OF UNIQUE REPORT IDS
dat<-dat[!(duplicated(dat$RptID)),]
state.unique.rpt<- dat %>%
	group_by(st)%>%
	summarise(reports=n_distinct(RptID),
		rpt.police=sum('%in%'(rptsrc, "cj")),
		rpt.edu=sum('%in%'(rptsrc, "education")),
		rpt.med=sum('%in%'(rptsrc, "medical")),
		rpt.welf=sum('%in%'(rptsrc, "socserv"))
		)

state.unique.rpt$year<-year[i]
state.out.unique.rpt[[i]]<-state.unique.rpt

rm(dat)

}

# dat<-read.csv("Child2011.csv")
# state.unique.rpt<- dat %>%
# 	group_by(st)%>%
# 	summarise(report_child=n(),
# 		reports=n_distinct(RptID),
# 		rpt.police=sum('%in%'(rptsrc, "cj")),
# 		rpt.edu=sum('%in%'(rptsrc, "education")),
# 		rpt.med=sum('%in%'(rptsrc, "medical")),
# 		rpt.welf=sum('%in%'(rptsrc, "socserv")),
# 		)
# year<-2011
# state.count$year<-year
# state.out[[9]]<-state.count
# rm(dat)

st.out<-do.call("rbind", state.out.unique.rpt)
st.out[st.out$st=="PA", 2:6] <- NA ## PA as missing, DOESN'T COUNT UNIQUE REPORTS, ONLY COUNTS ALL KIDS
write.csv(st.out, "rpt-count-unique.csv")

### screening out rates may vary - might want to control for proportion screened out at state level