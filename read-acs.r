rm(list=ls())
library(data.table)
library(dplyr)

setwd("H:/census")

colClasses=sapply(fread("usa_00032.csv", nrows=100), class)
colClasses[5:6]<-"character"
dat<-fread("usa_00032.csv", sep=",", 
	drop=c(2,3,4,8,9),
	colClasses=colClasses)

# if(nchar(dat$COUNTYFIPS)==)
### SMALL N OF

dat2<-dat%>%
	#mutate(FIPS=paste(STATEFIP, COUNTYFIPS, sep=""))%>%
	group_by(STATEFIP, COUNTYFIPS, YEAR)%>%
	summarise(totpop=sum(PERWT),
		chpop=sum((AGE<18)*PERWT),
		chpov=sum((AGE<18)*(POVERTY<150)*(PERWT)),
		blkpop=sum((RACE==2)*PERWT),
		amindpop=sum((RACE==3)*PERWT),
		unemp=sum((EMPSTAT==2)*PERWT),
		labforce=sum((EMPSTAT==1)*PERWT + (EMPSTAT==2)*PERWT),
		kids2par=sum((POPLOC!=0)*(MOMLOC!=0)*(AGE<18)*PERWT)
		)

write.csv(dat2, "acs-county-2007-12.csv", col.names=TRUE, row.names=FALSE)

