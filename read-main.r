.libPaths( c( .libPaths(), "U:/R") )

library(readr)
library(dplyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
library(ggplot2)
library(foreign)
library(haven)
set.seed(1)

setwd("R:/Project/NCANDS/ncands-csv/")

ucr<-read.csv("ucr-county-offense.csv", colClasses=c("character", NA))
names(ucr)[which(names(ucr)=="YEAR")]<-"year"
for(i in 2:ncol(ucr)){ucr[,i]<-as.numeric(ucr[,i])}


### assume no zero arrest counties, treat zero arrest counties as missing

pop<-read.csv("seer-pop.csv", colClasses=c("character", rep("numeric", 19)))

cdat<-read.csv("ncands-rpt-county-04-12.csv", colClasses=c("character", "character", 
                                                           rep("numeric", 13)))

names(cdat)[which(names(cdat)=="RptFIPS")]<-"FIPS"
names(cdat)[which(names(cdat)=="SubYr")]<-"year"
cdat[which(cdat$FIPS=="12025"), "FIPS"]<-"12086" ##FIX MIAMI-DADE RECODE


pov<-read.csv("saipe.csv")
pov$fips.st<-ifelse(nchar(as.character(pov$fips.st))<2, 
                    paste("0", as.character(pov$fips.st), sep=""), as.character(pov$fips.st))
pov$fips.cnty<-ifelse(nchar(as.character(pov$fips.cnty))==1,
                      paste("00", as.character(pov$fips.cnty), sep=""),
                      ifelse(nchar(as.character(pov$fips.cnty))==2,
                             paste("0", as.character(pov$fips.cnty), sep=""),
                             as.character(pov$fips.cnty)))
pov$FIPS<-paste(pov$fips.st, pov$fips.cnty, sep="")

inf.mort<-read.csv("infmort-clean.csv", colClasses = c(rep("character",3),
                                                       rep("numeric", 4)))

# emp<-read.csv("county-emp.csv", colClasses =
#                 c(rep("character", 5), rep(NA, 6)))
# emp<-emp%>%dplyr::select(FIPS, year, lawenf, totemp)

density<-read.csv("density.csv",
                  colClasses = c(rep("character", 7),
                                 rep(NA, 7)))
names(density)[5]<-"FIPS"
names(density)[12]<-"land.area"
density<-density%>%dplyr::select(FIPS, land.area)

dat<-left_join(
  left_join(
  left_join(
  left_join(left_join(
    cdat, inf.mort), pop), ucr), pov),
  density)

sdat<-dat%>%group_by(state, year)%>%summarise_if(is.numeric, funs(sum(., na.rm=TRUE)))%>%
  dplyr::select(-contains("infmort"), -prop.missing.rptsrc, - RptSrc)
sdat[sdat==0]<-NA

#filter out de-identified counties for now, maybe create state aggregates later for separate L1 frame
dat<-dat%>%filter(!(substr(FIPS, 3, 5)%in%c("000", "999")))

#### MAKE RATES AND CLEAN VARS
###
dat<-dat%>%mutate(pol.tot.pc=pol.rpts/child.pop,
                  pol.ai.pc=pol.ai/ai.child.pop,
                  pol.aa.pc=pol.aa/aa.child.pop,
                  pol.blk.pc=pol.blk/blk.child.pop,
                  pol.wht.pc=pol.wht/wht.child.pop,
                  arrest.all.tot.pc=all.tot/adult.pop,
                  # arrest.all.wht.pc=all.wht/wht.adult.pop,
                  # arrest.all.blk.pc=all.blk/blk.adult.pop,
                  # arrest.all.ai.pc=all.ai/ai.adult.pop,
                  # arrest.all.aa.pc=all.aa/aa.adult.pop,
                  arrest.viol.tot.pc=viol.tot/adult.pop,
                  # arrest.viol.wht.pc=viol.wht/wht.adult.pop,
                  # arrest.viol.blk.pc=viol.blk/blk.adult.pop,
                  # arrest.viol.ai.pc=viol.ai/ai.adult.pop,
                  # arrest.viol.aa.pc=viol.aa/aa.adult.pop,
                  arrest.drug.tot.pc=drug.tot/adult.pop,
                  # arrest.drug.wht.pc=drug.wht/wht.adult.pop,
                  # arrest.drug.blk.pc=drug.blk/blk.adult.pop,
                  # arrest.drug.ai.pc=drug.ai/ai.adult.pop,
                  # arrest.drug.aa.pc=drug.aa/aa.adult.pop,
                  arrest.qol.tot.pc=qol.tot/adult.pop,
                  # arrest.qol.wht.pc=qol.wht/wht.adult.pop,
                  # arrest.qol.blk.pc=qol.blk/blk.adult.pop,
                  # arrest.qol.ai.pc=qol.ai/ai.adult.pop,
                  # arrest.qol.aa.pc=qol.aa/aa.adult.pop,
                  pop.density=tot.pop/land.area,
                  police.pc=officers/tot.pop)

sdat<-sdat%>%mutate(pol.tot.pc=pol.rpts/child.pop,
                    # pol.ai.pc=pol.ai/ai.child.pop,
                    # pol.aa.pc=pol.aa/aa.child.pop,
                    # pol.blk.pc=pol.blk/blk.child.pop,
                    # pol.wht.pc=pol.wht/wht.child.pop,
                    arrest.all.tot.pc=all.tot/adult.pop,
                    # arrest.all.wht.pc=all.wht/wht.adult.pop,
                    # arrest.all.blk.pc=all.blk/blk.adult.pop,
                    # arrest.all.ai.pc=all.ai/ai.adult.pop,
                    # arrest.all.aa.pc=all.aa/aa.adult.pop,
                    arrest.viol.tot.pc=viol.tot/adult.pop,
                    # arrest.viol.wht.pc=viol.wht/wht.adult.pop,
                    # arrest.viol.blk.pc=viol.blk/blk.adult.pop,
                    # arrest.viol.ai.pc=viol.ai/ai.adult.pop,
                    # arrest.viol.aa.pc=viol.aa/aa.adult.pop,
                    arrest.drug.tot.pc=drug.tot/adult.pop,
                    # arrest.drug.wht.pc=drug.wht/wht.adult.pop,
                    # arrest.drug.blk.pc=drug.blk/blk.adult.pop,
                    # arrest.drug.ai.pc=drug.ai/ai.adult.pop,
                    # arrest.drug.aa.pc=drug.aa/aa.adult.pop,
                    arrest.qol.tot.pc=qol.tot/adult.pop,
                    # arrest.qol.wht.pc=qol.wht/wht.adult.pop,
                    # arrest.qol.blk.pc=qol.blk/blk.adult.pop,
                    # arrest.qol.ai.pc=qol.ai/ai.adult.pop,
                    # arrest.qol.aa.pc=qol.aa/aa.adult.pop,
                    pop.density=tot.pop/land.area,
                    police.pc=officers/tot.pop)
dat<-dat%>%dplyr::select(-tag, -stname)
