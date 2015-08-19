rm(list=ls())
library(readr)
library(dplyr)
library(lme4)

source("H:/ncands-fc/ncandsfunctions.r")

setwd("H:/")
source("H:/NCANDS-clean/ncandsread.r")

### PULLED COL WIDTH, COL NAMES FROM VariableLayout.xlsx, transposed
### READ AND CLEAN DATA
key<-read.csv("H:/NCANDS-clean/ncandskey2012.csv", head=TRUE)

dat<-ncands.fwf(dat="H:/data/Sample2012.dat", "H:/NCANDS-clean/ncandskey2012.csv")
dat<-ncandsclean(dat)
dat<-as.data.frame(dat)
state<-read.csv("H:/ncands-fc/statedat.csv", head=TRUE)
names(state)[(which(names(state)=="stname"))]<-"st"
### STATE LEVEL MEASURES
state$police.pc<-state$police.ft.emp/state$pop
state$welfare.pc<-state$welfare.ft.emp/state$pop
state$pctblk<-state$blkpop/state$pop
state$chpovrt<-state$childpov/state$child


state2011<-state[state$year==2011,] ### As latest rolling estimate with this data, will improve later to 5yr ACS for period

# #### LOOK AT FORMAL REPORTS COUNT BY STATE - have problem of missing race data
# ### using dplyr
# states<- d %>%
# 	group_by(st)%>%
# 	summarise(report_child=n(),
# 		reports=n_distinct(RptID),
# 		report.race=sum(!(is.na(chrace))),
# 		child.blk=sum('%in%'(chrace, "black")),
# 		pct.blk.rpt=child.blk/report.race,
# 		fc=sum('%in%'(serv.foster, TRUE))
# 		)

# states<-left_join(states, state2011, by="st")

s.dat<-left_join(dat, state2011, by="st")


# stabs<-as.data.frame(matrix(nrow=length(unique(dat$StaTerr)), ncol=ncol(dat)))
# m.index<-NULL
# for(i in(1:ncol(dat))){
# 	stabs[,i]<-as.matrix(table(dat$StaTerr, is.na(dat[,i])))[,1]/
# 		as.matrix(table(dat$StaTerr))[,1]
# }
# names(stabs)<-names(dat)
# stabs<-cbind(unique(dat$StaTerr), stabs)



### LATER - BOUNDED COUNT MODELS
### TRY TO THIS POINT WITH ALL YEARS OF SAMPLE DATA
##################glmer() offset example###################################
# ent.scale<-glmer(entered~scale(unemprt)+scale(childnot2par)+
# 	scale(food.insec)+scale(chpovrt)+scale(LessHS)+
# 	scale(gsppercap)+
# 	scale(ideo)+scale(crime.pc)+scale(pctblk)+
# 	scale(tanf.pc)+scale(incarrt)+
# 	(1|state)+(1|year)+(1|obs_n), offset=log(child),  
# 	data=fc.count,
# 	family=poisson)







# m1<-rptsrc!="informal"~chage+chmale+chrace+chlatino+
# 	ideo+
# 	#+crime.pc+childnot2par+chpovrt+incarrt+afdcrec
# 	(1|stname)


# d$rptsrc[is.na(d$rptsrc)]<-"informal"

# rpt.count<-as.data.frame(table(d$stname))
# rpt.form.count<-aggregate(d$rptsrc!="informal",
# 	by=list(d$st), FUN=sum)
# rptpct<-rpt.form.count[,2]/rpt.count[,2]
# rpt<-cbind(rpt.count, rpt.form.count, rptpct)
# names(rpt)<-c("st", "totrpt", "st", "form.rpt", "pct")

# m1.results<-glmer(m1, data=mergetest, family="binomial")


m2<-victim~(chrace=="black")*(par.married==FALSE)+
	+alleg.neg+alleg.phys+alleg.medneg+alleg.sex+alleg.psych+
	ideo+pctblk+povrt
	

m2.results<-glm(m2, data=s.dat, family="binomial")

m3<-(chrace=="black")~
	+alleg.neg+alleg.phys+alleg.medneg+alleg.sex+alleg.psych+
	rptsrc+
	ideo+pctblk+povrt

m3.results<-glm(m3, data=s.dat, family="binomial")

rpt.results<-NULL
rpt<-levels(s.dat$rptsrc)
for(i in (1:length(levels(s.dat$rptsrc)))){
	m<-(rptsrc==rpt[[i]])~alleg.neg+alleg.phys+
	alleg.medneg+alleg.sex+alleg.psych+
	(chrace=="black")+
	scale(inst6010_nom)+scale(pctblk)+#povrt+
	scale(police.pc)+scale(welfare.pc)+(1|st)
	rpt.results[[i]]<-glmer(m, data=s.dat, family="binomial")
}

#m3.results<-glmer(m3, data=mergetest, family="binomial")




### CLEANING AND MISSING EVAL

# stabs<-list(NULL)
# m.index<-NULL
# for(i in(1:ncol(dat))){
# 	stabs[[i]]<-table(dat$StaTerr, is.na(dat[,i]))
# }
# names(stabs)<-names(dat)
