rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(nlme)
library(lme4)
library(ggplot2)
library(Amelia)
library(arm)
library(pander)
library(texreg)
set.seed(1)

setwd("H:/")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

### PULLED COL WIDTH, COL NAMES FROM VariableLayout.xlsx, transposed
### READ AND CLEAN DATA
state.mal<-read.csv("H:/data/strpt00-12.csv", stringsAsFactors = FALSE)
state.dat<-read.csv("H:/data/statedat.csv", stringsAsFactors=FALSE)
names(state.dat)[which(names(state.dat)=="stname")]<-"staterr"
state.mal<-left_join(state.mal, state.dat, by=c("staterr", "year"))
### remove XX (death cases) and PR - can add death cases back in with state file (probably should!)
state.mal<-state.mal[-which(state.mal$staterr%in%c("XX", "PR")),]

cnty.mal<-read.csv("H:/data/cntyrpt06-12.csv", stringsAsFactors = FALSE)
### Strip out missing and de-identified counties
cnty.mal<-cnty.mal[-which(cnty.mal$rptfips==0),]
cnty.mal<-cnty.mal[which(!(substrRight(as.character(cnty.mal$rptfips), 3)%in%(c("999", "000")))),]

pop<-read.table("H:/data/CDC-Wonder-County-Kids.tsv", sep="",
                stringsAsFactors = FALSE, head=TRUE)
names(pop)<-c("cname", "FIPS", "year", "year.dup", "child.pop")
### pull out state abrev from "NAME County, ST format in var
z<-do.call("rbind",(strsplit(pop$cname, ", ")))
pop$cname<-z[,1]
pop$state<-z[,2]

names(cnty.mal)[which(names(cnty.mal)=="staterr")]<-"state"
names(cnty.mal)[which(names(cnty.mal)=="rptfips")]<-"FIPS"
cnty.mal<-cnty.mal[-which(cnty.mal$state=="PR"),]
### MIAMI DADE CHANGED FROM 12025 to 12086
cnty.mal[which(cnty.mal$FIPS==12025), "FIPS"]<-12086

rpt.cnty<-left_join(cnty.mal, pop, by=c("FIPS", "year", "state"))


### add overdispersion
# rpt.cnty$obs_n<-(1:nrow(rpt.cnty))
# m.0<-glmer(tot.rpt~I(year-2006)+(I(year-2006)|FIPS)+(I(year-2006)|state)+(1|obs_n), data=rpt.cnty, family=poisson, offset=log(child.pop))

### write function to read in NHGIS pop data
read.nhgis<-function(x){
  temp<-read.csv(x)
  ### Have to pasted together state and county codes
  ### need county codes to be XXX
  temp$county<-formatC(temp$COUNTYA, width = 3, format = "d", flag = "0")
  ### Convert to 01001 SSCCC FIPS FORMAT
  temp$FIPS<-paste(as.character(temp$STATEA), temp$county, sep="")
  ### Construct needed vars
  ### % of family households getting SSI, pub assist, SNAP in past 12 mos
  temp$fam.assist<-(temp$UTAE003/temp$UTAE001)
  temp$ch.pov<-with(temp,(UV9E004+UV9E005+UV9E006+UV9E007+UV9E008+UV9E009)+
                      (UV9E018+UV9E019+UV9E020+UV9E021+UV9E022+UV9E023))
  ### Wonky stuff going on when applying margin of error - results in negative pop counts... ignoring for now
  ### i.e. hist((temp$UTAM003+temp$UTAE003)/temp$UTAE003)
  out<-data.frame("FIPS"=as.numeric(temp$FIPS), "fam.assist"=temp$fam.assist, "ch.pov"=temp$ch.pov)
  return(out)
}
read.nhgis2<-function(x){
  temp<-read.csv(x)
  ### Have to pasted together state and county codes
  ### need county codes to be XXX
  temp$county<-formatC(temp$COUNTYA, width = 3, format = "d", flag = "0")
  ### Convert to 01001 SSCCC FIPS FORMAT
  temp$FIPS<-paste(as.character(temp$STATEA), temp$county, sep="")
  ### Construct needed vars
  ### % of family households getting SSI, pub assist, SNAP in past 12 mos
  temp$c.pop<-temp$UEYE001
  temp$c.wht.pop<-temp$UEYE003
  temp$c.blk.pop<-temp$UEYE004+temp$UEYE014
  temp$c.amind.pop<-temp$UEYE005+temp$UEYE015
  temp$c.hisp.pop<-temp$UEYE012
  
  out<-data.frame("FIPS"=as.numeric(temp$FIPS), "c.pop"=temp$c.pop, "c.wht.pop"=temp$c.wht.pop, "c.blk.pop"=temp$c.blk.pop, "c.amind.pop"=temp$c.amind.pop,
                  "c.hisp.pop"=temp$c.hisp.pop)
  return(out)
}
cnty.pov1<-read.nhgis("h:/data/nhgis0016_ds202_20135_2013_county.csv")
cnty.race<-read.nhgis2("h:/data/nhgis0017_ds201_20135_2013_county.csv")
cnty.pov2<-left_join(cnty.pov1, cnty.race, by="FIPS")

## read ed data
cnty.ed<-read.csv("H:/data/NCES_Report_215.csv", skip=10, stringsAsFactors = FALSE)
names(cnty.ed)<-c("state", "county.name", "FIPS", "tot.schools", "charter.schools",
                  "state.fips", "tot.students", "free.lunch", "red.lunch",
                  "free.red.lunch", "ell", "indiv.ed.prog.stud", "tot.staff",
                  "s.t.ratio", "fte.teachers.scl", "fte.teachers.dist", "instruct.aides",
                  "instruct.coord", "elem.counsel", "elem.teachers", "k.teachers",
                  "lea.admin", "lea.admin.support", "libr", "lib.support","other.support",
                  "pre.k.teachers", "school.admin", "school.admin.support",
                  "sec.counsel", "sec.teachers", "student.support.dist", "counselors",
                  "ungraded.teachers", "other.counselors")
cnty.pov<-left_join(cnty.pov2, cnty.ed, by=c("FIPS"))

cnty.emp<-read.fwf("H:/data/12coar2.dat", 
                   widths = c(2, 1, 3, 11, 3, 10, 12, 10, 12, 10, 10, 10, 12))
cnty.emp<-cnty.emp[,-c(2,4)]

names(cnty.emp)<-c("s.code", "c.code", "item", "fte", "ftpay", "pte", "ptpay",
                   "pthrs", "ftee", "tot.emp", "tot.pay")

### STUPID STATE CODES IN CoG aren't FIPS, gotta use the key file to match, sub FIPS
emp.key<-read.fwf("H:/data/12coar1.dat",
                  widths=c(2, 1, 3, 8, 35,29,1,30,2,3,5,4,2,9,2,61,2))
emp.key<-emp.key[,-c(2,4)]
names(emp.key)<-c("s.code", "c.code", ".", ".", ".", ".", "fips.st", "fips.c")

emp.key$fips.c<-formatC(emp.key$fips.c, width = 3, format = "d", flag = "0")
emp.key$FIPS<-paste(emp.key$fips.st, emp.key$fips.c, sep="")
emp.key<-emp.key[, c("s.code", "c.code", "FIPS")]
emp.join<-left_join(cnty.emp, emp.key, by=c("s.code", "c.code"))
emp.pay<-emp.join%>%
  dplyr::select(FIPS, item, tot.pay)%>%
  group_by(FIPS)%>%
  spread(item,tot.pay)

c.pay<-emp.join%>%
  dplyr::select(FIPS, item, tot.pay)%>%
  group_by(FIPS)%>%
  spread(item, tot.pay)%>%
  dplyr::select(FIPS, all.pay=`0`, teach.pay=`12`, 
                health.pay=`32`, hosp.pay=`40`, police.pay=`62`, welf.pay=`79`)

c.emp<-emp.join%>%
  dplyr::select(FIPS, item, tot.emp)%>%
  group_by(FIPS)%>%
  spread(item, tot.emp)%>%
  dplyr::select(FIPS, all.emp=`0`, teach.emp=`12`, 
                health.emp=`32`, hosp.emp=`40`, police.emp=`62`, welf.emp=`79`)

c.emp.dat<-left_join(c.pay, c.emp, by="FIPS")                   
c.emp.dat$FIPS<-as.numeric(c.emp.dat$FIPS)
### use single year for now - most data in 2011
cnty.11<-left_join(rpt.cnty[rpt.cnty$year==2011,], cnty.pov, by=c("FIPS", "state"))
cnty.11$ch.pov.rt<-cnty.11$ch.pov/cnty.11$child.pop
# mc.0<-glmer(tot.rpt~ch.pov.rt+(1|state)+(1|obs_n), data=cnty.11, family=poisson, offset=log(child.pop))
### cor(fam.assist, ch.pov.rt)>0.9, can't have both in model
### bring in state covars


state.11<-state.dat[state.dat$year==2011,]
cnty.11$staterr<-cnty.11$state
cnty.dat<-left_join(cnty.11, state.11, by=c("staterr"))
cnty.dat<-left_join(cnty.dat, c.emp.dat, by="FIPS")

cnty.dat$obs_n<-seq(1:nrow(cnty.dat))

## validate quality of census of gov data
cor(cnty.dat$teach.emp, cnty.dat$tot.staff)

mt1<-tot.rpt~scale(ch.pov.rt)+scale(inc.crime)+scale(ideo)+scale(pctblk)+
  scale(I(c.blk.pop/c.pop))+scale(I(c.amind.pop/c.pop))+scale(I(c.hisp.pop/c.pop))+
  scale(s.t.ratio)+scale(I(libr/tot.students))+scale(I(counselors/tot.students))+
  scale(I(health.emp+hosp.emp)/c.pop)+scale(I(police.emp/c.pop))+scale(I(welf.emp/c.pop))+
  (1|staterr)+(1|obs_n)

mp1<-prof.rpt~scale(ch.pov.rt)+scale(inc.crime)+scale(ideo)+scale(pctblk)+
  scale(I(c.blk.pop/c.pop))+scale(I(c.amind.pop/c.pop))+scale(I(c.hisp.pop/c.pop))+
  scale(s.t.ratio)+scale(I(libr/tot.students))+scale(I(counselors/tot.students))+
  scale(I(health.emp+hosp.emp)/c.pop)+scale(I(police.emp/c.pop))+scale(I(welf.emp/c.pop))+
  (1|staterr)+(1|obs_n)

### src5=education personnel
me1<-src5~scale(ch.pov.rt)+scale(inc.crime)+scale(ideo)+scale(pctblk)+
  scale(I(c.blk.pop/c.pop))+scale(I(c.amind.pop/c.pop))+scale(I(c.hisp.pop/c.pop))+
  scale(s.t.ratio)+scale(I(libr/tot.students))+scale(I(counselors/tot.students))+
  scale(I(health.emp+hosp.emp)/c.pop)+scale(I(police.emp/c.pop))+scale(I(welf.emp/c.pop))+
  (1|staterr)+(1|obs_n)


e.1<-glmer(me1, data=cnty.dat, family=poisson, offset=log(child.pop))
p.1<-glmer(mp1, data=cnty.dat, family=poisson, offset=log(child.pop))
t.1<-glmer(mt1, data=cnty.dat, family=poisson, offset=log(child.pop))

texreg(list(t.1, p.1, e.1), 
                  custom.model.names=c("Total", "Prof", "Edu"),
                  custom.coef.names=c("Intercept", "Child Pov", "Incar / Crime",
                                      "Leg. Ideology", "State pct Black", 
                                      "County pct Black", "County pct Am. Ind.",
                                      "County pct Latino", "Student / Teacher Ratio",
                                      "Librarians per student", "Counselors per student",
                                      "Public med employees per cap", "Police per cap", "Welfare staff per cap"),
       file="ProspModels.tex")

state.mal<-state.mal[state.mal$year<2012,]
state.mal$obs_n<-(1:nrow(state.mal))
state.mal$year.c<-state.mal$year-2000


st1<-tot.rpt~scale(I(childpov/child))+scale(inc.crime)+scale(ideo)+scale(pctblk)+scale(crime)+
  +year.c+(year.c|staterr)+(1|obs_n)

mp1<-prof.rpt~scale(ch.pov.rt)+scale(inc.crime)+scale(ideo)+scale(pctblk)+
  scale(I(c.blk.pop/c.pop))+scale(I(c.amind.pop/c.pop))+scale(I(c.hisp.pop/c.pop))+
  scale(s.t.ratio)+scale(I(libr/tot.students))+scale(I(counselors/tot.students))+
  scale(I(health.emp+hosp.emp)/c.pop)+scale(I(police.emp/c.pop))+scale(I(welf.emp/c.pop))+
  (1|staterr)+(1|obs_n)

### src5=education personnel
me1<-src5~scale(ch.pov.rt)+scale(inc.crime)+scale(ideo)+scale(pctblk)+
  scale(I(c.blk.pop/c.pop))+scale(I(c.amind.pop/c.pop))+scale(I(c.hisp.pop/c.pop))+
  scale(s.t.ratio)+scale(I(libr/tot.students))+scale(I(counselors/tot.students))+
  scale(I(health.emp+hosp.emp)/c.pop)+scale(I(police.emp/c.pop))+scale(I(welf.emp/c.pop))+
  (1|staterr)+(1|obs_n)


e.1<-glmer(me1, data=cnty.dat, family=poisson, offset=log(child.pop))
p.1<-glmer(mp1, data=cnty.dat, family=poisson, offset=log(child.pop))
st.1<-glmer(st1, data=state.mal, family=poisson, offset=log(child))

texreg(list(t.1, p.1, e.1), 
       custom.model.names=c("Total", "Prof", "Edu"),
       custom.coef.names=c("Intercept", "Child Pov", "Incar / Crime",
                           "Leg. Ideology", "State pct Black", 
                           "County pct Black", "County pct Am. Ind.",
                           "County pct Latino", "Student / Teacher Ratio",
                           "Librarians per student", "Counselors per student",
                           "Public med employees per cap", "Police per cap", "Welfare staff per cap"),
       file="ProspModels.tex")

