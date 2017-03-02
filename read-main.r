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

### RESHAPE DATA TO LONG WITH RACE ID COLUMN - WILL HAVE REPEATED COUNTY ROWS, but way fewer columns

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

### think about incorporating reported intervals into measurement error models
pov<-read.csv("saipe.csv", colClasses=c(rep("numeric", 7), "character", rep("numeric", 7), "character"))


inf.mort<-read.csv("infmort-clean.csv", colClasses = c(rep("character",3),
                                                       rep("numeric", 4)))

###nhgis poverty data, includes standard errors for estimates
nhgis<-read.csv("nhgis-chpov.csv", colClasses=c(rep("numeric", 8), "character", "numeric"))
#### data are 5 yr, currently using 1st yr as year, add 1 for better match
nhgis$year<-nhgis$year+1

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
  left_join(
  left_join(
  left_join(
    cdat, inf.mort), pop), ucr), pov),
  density), nhgis)

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
                  arrest.all.wht.pc=all.wht/wht.adult.pop,
                  arrest.all.blk.pc=all.blk/blk.adult.pop,
                  arrest.all.ai.pc=all.ai/ai.adult.pop,
                  arrest.all.aa.pc=all.aa/aa.adult.pop,
                  arrest.viol.tot.pc=viol.tot/adult.pop,
                  arrest.viol.wht.pc=viol.wht/wht.adult.pop,
                  arrest.viol.blk.pc=viol.blk/blk.adult.pop,
                  arrest.viol.ai.pc=viol.ai/ai.adult.pop,
                  arrest.viol.aa.pc=viol.aa/aa.adult.pop,
                  arrest.drug.tot.pc=drug.tot/adult.pop,
                  arrest.drug.wht.pc=drug.wht/wht.adult.pop,
                  arrest.drug.blk.pc=drug.blk/blk.adult.pop,
                  arrest.drug.ai.pc=drug.ai/ai.adult.pop,
                  arrest.drug.aa.pc=drug.aa/aa.adult.pop,
                  arrest.qol.tot.pc=qol.tot/adult.pop,
                  arrest.qol.wht.pc=qol.wht/wht.adult.pop,
                  arrest.qol.blk.pc=qol.blk/blk.adult.pop,
                  arrest.qol.ai.pc=qol.ai/ai.adult.pop,
                  arrest.qol.aa.pc=qol.aa/aa.adult.pop,
                  pop.density=tot.pop/land.area,
                  police.pc=officers/tot.pop,
                  pct.blk=blk.pop/tot.pop,
                  pct.wht=wht.pop/tot.pop,
                  pct.ai=ai.pop/tot.pop,
                  pct.aa=aa.pop/tot.pop)

sdat<-sdat%>%mutate(pol.tot.pc=pol.rpts/child.pop,
                    pol.ai.pc=pol.ai/ai.child.pop,
                    pol.aa.pc=pol.aa/aa.child.pop,
                    pol.blk.pc=pol.blk/blk.child.pop,
                    pol.wht.pc=pol.wht/wht.child.pop,
                    arrest.all.tot.pc=all.tot/adult.pop,
                    arrest.all.wht.pc=all.wht/wht.adult.pop,
                    arrest.all.blk.pc=all.blk/blk.adult.pop,
                    arrest.all.ai.pc=all.ai/ai.adult.pop,
                    arrest.all.aa.pc=all.aa/aa.adult.pop,
                    arrest.viol.tot.pc=viol.tot/adult.pop,
                    arrest.viol.wht.pc=viol.wht/wht.adult.pop,
                    arrest.viol.blk.pc=viol.blk/blk.adult.pop,
                    arrest.viol.ai.pc=viol.ai/ai.adult.pop,
                    arrest.viol.aa.pc=viol.aa/aa.adult.pop,
                    arrest.drug.tot.pc=drug.tot/adult.pop,
                    arrest.drug.wht.pc=drug.wht/wht.adult.pop,
                    arrest.drug.blk.pc=drug.blk/blk.adult.pop,
                    arrest.drug.ai.pc=drug.ai/ai.adult.pop,
                    arrest.drug.aa.pc=drug.aa/aa.adult.pop,
                    arrest.qol.tot.pc=qol.tot/adult.pop,
                    arrest.qol.wht.pc=qol.wht/wht.adult.pop,
                    arrest.qol.blk.pc=qol.blk/blk.adult.pop,
                    arrest.qol.ai.pc=qol.ai/ai.adult.pop,
                    arrest.qol.aa.pc=qol.aa/aa.adult.pop,
                    pop.density=tot.pop/land.area,
                    police.pc=officers/tot.pop)

dat<-dat%>%group_by(FIPS)%>%
  mutate(infmort.mean=mean(infmort, na.rm=TRUE),
         nonwht.infmort.mean=mean(nonwht.infmort, na.rm=TRUE),
         wht.infmort.mean=mean(wht.infmort, na.rm=TRUE),
         child.pov.pct.mean=mean(child.pov.pct, na.rm=TRUE),
         blk.chpov_pe.mean=mean(blk.chpov_pe, na.rm=TRUE),
         ai.chpov_pe.mean=mean(ai.chpov_pe, na.rm=TRUE),
         aa.chpov_pe.mean=mean(aa.chpov_pe, na.rm=TRUE),
         wht.chpov_pe.mean=mean(wht.chpov_pe, na.rm=TRUE),
         arrest.all.tot.pc.mean=mean(arrest.all.tot.pc, na.rm=TRUE),
         arrest.viol.tot.pc.mean=mean(arrest.viol.tot.pc, na.rm=TRUE),
         arrest.drug.tot.pc.mean=mean(arrest.drug.tot.pc, na.rm=TRUE),
         arrest.qol.tot.pc.mean=mean(arrest.qol.tot.pc, na.rm=TRUE),
         arrest.all.blk.pc.mean=mean(arrest.all.blk.pc, na.rm=TRUE),
         arrest.viol.blk.pc.mean=mean(arrest.viol.blk.pc, na.rm=TRUE),
         arrest.drug.blk.pc.mean=mean(arrest.drug.blk.pc, na.rm=TRUE),
         arrest.qol.blk.pc.mean=mean(arrest.qol.blk.pc, na.rm=TRUE),
         arrest.all.wht.pc.mean=mean(arrest.all.wht.pc, na.rm=TRUE),
         arrest.viol.wht.pc.mean=mean(arrest.viol.wht.pc, na.rm=TRUE),
         arrest.drug.wht.pc.mean=mean(arrest.drug.wht.pc, na.rm=TRUE),
         arrest.qol.wht.pc.mean=mean(arrest.qol.wht.pc, na.rm=TRUE),
         arrest.all.ai.pc.mean=mean(arrest.all.ai.pc, na.rm=TRUE),
         arrest.viol.ai.pc.mean=mean(arrest.viol.ai.pc, na.rm=TRUE),
         arrest.drug.ai.pc.mean=mean(arrest.drug.ai.pc, na.rm=TRUE),
         arrest.qol.ai.pc.mean=mean(arrest.qol.ai.pc, na.rm=TRUE),
         arrest.all.aa.pc.mean=mean(arrest.all.aa.pc, na.rm=TRUE),
         arrest.viol.aa.pc.mean=mean(arrest.viol.aa.pc, na.rm=TRUE),
         arrest.drug.aa.pc.mean=mean(arrest.drug.aa.pc, na.rm=TRUE),
         arrest.qol.aa.pc.mean=mean(arrest.qol.aa.pc, na.rm=TRUE),
         police.pc.mean=mean(police.pc, na.rm=TRUE),
         pop.density.mean=mean(pop.density, na.rm=TRUE),
         pct.blk.mean=mean(pct.blk, na.rm=TRUE),
         pct.wht.mean=mean(pct.wht, na.rm=TRUE),
         pct.ai.mean=mean(pct.ai, na.rm=TRUE),
         pct.aa.mean=mean(pct.aa, na.rm=TRUE))

dat<-dat%>%dplyr::select(-RptSrc, -all.lat, -viol.lat, -drug.lat, -qol.lat)
