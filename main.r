rm(list=ls())
library(readr)
library(dplyr)
library(lme4)
library(texreg)

source("H:/ncands-fc/ncandsfunctions.r")

setwd("H:/")
source("H:/NCANDS-clean/ncandsread.r")

### PULLED COL WIDTH, COL NAMES FROM VariableLayout.xlsx, transposed
### READ AND CLEAN DATA
key<-read.csv("H:/NCANDS-clean/ncandskey2012.csv", head=TRUE)

dat<-ncands.fwf(dat="H:/data/Child2012v1.dat", "H:/NCANDS-clean/ncandskey2012.csv")
dat<-ncandsclean(dat)
dat<-dat[,c(3, 4, 6, 11, 21:25)]
dat<-as.data.frame(dat)
dat$chrace<-factor(dat$chrace, levels(dat$chrace)[c(4,1,2,3)])
state<-read.csv("H:/ncands-fc/statedat.csv", head=TRUE)
names(state)[(which(names(state)=="stname"))]<-"st"
### STATE LEVEL MEASURES
state$police.pc<-state$police.ft.emp/state$pop
state$welfare.pc<-state$welfare.ft.emp/state$pop
state$edu.pc<-state$edu.ft.emp/state$child
state$hosp.pc<-state$hosp.ft.emp/state$pop
state$pctblk<-state$blkpop/state$pop
state$chpovrt<-state$childpov/state$child
state$unemprt<-state$unemp/(state$unemp+state$emp)
state$childnot2par<-1-(state$kids2par/state$child)
state$gsppercap<-state$GSP*1000000/state$pop




state2011<-state[state$year==2010,] ### As latest rolling estimate with this data, will improve later to 5yr ACS for period
keeps<-c("st", "police.pc", "welfare.pc", "edu.pc", "hosp.pc",
	"pctblk", "chpovrt", "unemprt", "childnot2par", "gsppercap", "food.insec",
	"inst6010_nom")
state2011<-state2011[,which(names(state2011)%in%keeps)]

#### LOOK AT FORMAL REPORTS COUNT BY STATE - have problem of missing race data
### using dplyr
# state.count<- dat %>%
# 	group_by(st)%>%
# 	summarise(report_child=n(),
# 		reports=n_distinct(RptID),
# 		report.race=sum(!(is.na(chrace))),
# 		child.blk=sum('%in%'(chrace, "black")),
# 		pct.blk.rpt=child.blk/report.race,
# 		rpt.police=sum('%in%'(rptsrc, "cj")),
# 		rpt.edu=sum('%in%'(rptsrc, "education")),
# 		rpt.med=sum('%in%'(rptsrc, "medical")),
# 		rpt.welf=sum('%in%'(rptsrc, "socserv")),
# 		fc=sum('%in%'(serv.foster, TRUE))
# 		)

# state.count<-left_join(state.count, state2011, by="st")

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


# m2<-victim~(chrace=="black")*(par.married==FALSE)+
# 	+alleg.neg+alleg.phys+alleg.medneg+alleg.sex+alleg.psych+
# 	ideo+pctblk+povrt
	

# m2.results<-glm(m2, data=s.dat, family="binomial")

# m3<-(chrace=="black")~
# 	+alleg.neg+alleg.phys+alleg.medneg+alleg.sex+alleg.psych+
# 	rptsrc+
# 	ideo+pctblk+povrt

# m3.results<-glm(m3, data=s.dat, family="binomial")

rpt.results<-list()
m<-list()
	###POLICE
	m[[1]]<-(rptsrc=="cj")~alleg.neg+alleg.phys+
	alleg.medneg+alleg.sex+alleg.psych+
	chrace+
	scale(inst6010_nom)+scale(pctblk)+scale(chpovrt)+
	scale(childnot2par)+scale(unemprt)+
	scale(food.insec)+scale(gsppercap)+
	scale(police.pc)+scale(edu.pc)+scale(hosp.pc)+scale(welfare.pc)+
	(1|st)
	###EDUCATION
	m[[2]]<-(rptsrc=="education")~alleg.neg+alleg.phys+
	alleg.medneg+alleg.sex+alleg.psych+
	chrace+
	scale(inst6010_nom)+scale(pctblk)+scale(chpovrt)+
	scale(childnot2par)+scale(unemprt)+
	scale(food.insec)+scale(gsppercap)+
	scale(police.pc)+scale(edu.pc)+scale(hosp.pc)+scale(welfare.pc)+
	(1|st)
	###INFORMAL 
	m[[3]]<-(rptsrc=="informal")~alleg.neg+alleg.phys+
	alleg.medneg+alleg.sex+alleg.psych+
	chrace+
	scale(inst6010_nom)+scale(pctblk)+scale(chpovrt)+
	scale(childnot2par)+scale(unemprt)+
	scale(food.insec)+scale(gsppercap)+
	scale(police.pc)+scale(edu.pc)+scale(hosp.pc)+scale(welfare.pc)+
	(1|st)
	###MEDICAL
	m[[4]]<-(rptsrc=="medical")~alleg.neg+alleg.phys+
	alleg.medneg+alleg.sex+alleg.psych+
	chrace+
	scale(inst6010_nom)+scale(pctblk)+scale(chpovrt)+
	scale(childnot2par)+scale(unemprt)+
	scale(food.insec)+scale(gsppercap)+
	scale(police.pc)+scale(edu.pc)+scale(hosp.pc)+scale(welfare.pc)+
	(1|st)
	###WELFARE STAFF
	m[[5]]<-(rptsrc=="socserv")~alleg.neg+alleg.phys+
	alleg.medneg+alleg.sex+alleg.psych+
	chrace+
	scale(inst6010_nom)+scale(pctblk)+scale(chpovrt)+
	scale(childnot2par)+scale(unemprt)+
	scale(food.insec)+scale(gsppercap)+
	scale(police.pc)+scale(edu.pc)+scale(hosp.pc)+scale(welfare.pc)+
	(1|st)

for(i in (1:5)){
	rpt.results[[i]]<-glmer(m[[i]], data=s.dat, family="binomial")
}

plotreg(l=list(rpt.results[[1]]), file="rpt-police.pdf",
	custom.model.names=c(""),
	custom.coef.names=c("Intercept",
		"alleg.neg",
		"alleg.phys",
		"alleg.medneg",
		"alleg.sex",
		"alleg.psych",
		"Child race - Black",
		"Child race - Native American",
		"Political Ideology",
		"Percent Black population",
		"Child poverty rate",
		"Single parent family rate",
		"Unemployment rate",
		"Food insecurity rate",
		"GSP per capita",
		"Police per capita",
		"Welfare workers per capita",
		"Education staff per capita",
		"Public hospital staff per capita"),
	omit.coef="(alleg)")