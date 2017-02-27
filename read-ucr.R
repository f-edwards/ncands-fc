rm(list=ls())

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

###UCR READ
### load 2012 UCR, loads object da35018.001
dir<-"R:/Project/NCANDS/ucr/"
files<-c("ICPSR_03729/ICPSR_03729/DS0001/03729-0001-Data.dta",
         "ICPSR_03997/ICPSR_03997/DS0001/03997-0001-Data.dta",
         "ICPSR_04068/ICPSR_04068/DS0001/04068-0001-Data.dta",
         "ICPSR_04461/ICPSR_04461/DS0001/04461-0001-Data.dta",
         "ICPSR_04716/ICPSR_04716/DS0001/04716-0001-Data.dta",
         "ICPSR_22405/ICPSR_22405/DS0001/22405-0001-Data.dta",
         "ICPSR_25106/ICPSR_25106/DS0001/25106-0001-Data.dta",
         "ICPSR_27643/ICPSR_27643/DS0001/27643-0001-Data.dta",
         "ICPSR_27651/ICPSR_27651/DS0001/27651-0001-Data.dta",
         "ICPSR_30762/ICPSR_30762/DS0001/30762-0001-Data.dta",
         "ICPSR_33522/ICPSR_33522/DS0001/33522-0001-Data.dta",
         "ICPSR_34581/ICPSR_34581/DS0001/34581-0001-Data.dta",
         "ICPSR_35018/ICPSR_35018/DS0001/35018-0001-Data.dta",
         "ICPSR_36116/ICPSR_36116/DS0001/36116-0001-Data.dta",
         "ICPSR_36400/ICPSR_36400/DS0001/36400-0001-Data.dta")
files<-paste(dir, files, sep="")
ucr<-list()
for(i in 1:length(files)){
  ucr[[i]]<-read_dta(files[[i]])
  print(i)
}

ucr.dat<-bind_rows(list(ucr[[1]], ucr[[2]], ucr[[3]], ucr[[4]], ucr[[5]], ucr[[6]],
                        ucr[[7]], ucr[[8]], ucr[[9]], ucr[[10]], ucr[[11]], ucr[[12]],
                        ucr[[13]], ucr[[14]]))

names(ucr.dat)[which(names(ucr.dat)=="ORI")]<-"ORI7"

ucr.dat[ucr.dat==999998]<-NA
ucr.dat<-ucr.dat%>%filter(REPORT==0)
#ucr.dat[ucr.dat=="999998]"]<-NA

### load UCR crosswalk to convert into FIPS, ORI in UCR, ORI7 in crosswalk should match
crosswalk<-read.delim("35158-0001-Data.tsv", sep="\t", colClasses=c(rep(NA, 3), rep("character", 3), rep(NA, 40)),
                      stringsAsFactors = FALSE)

ucr.fips<-left_join(ucr.dat, crosswalk, by="ORI7")



##PROBLEM WITH SUMMARY OFFENSE CODES, FILTER OUT SUBSET CODES
drug.codes<-as.character(180:189)
gambling.codes<-as.character(191:193)
ucr.fips<-ucr.fips%>%filter((!(OFFENSE%in%drug.codes))&
                   (!(OFFENSE%in%gambling.codes)))

ucr.fips<-ucr.fips%>%filter(CONTENTS==2)
rm(ucr.dat)

####Create county-level counts of arrest by offense type, want total arrest, violent arrest, fam viol arrest, 
### Age is more inclusive, many more arrests counted, appears race underrreported

age.index<-c(which(names(ucr.fips)=="M0_9"), which(names(ucr.fips)=="F65"))
#race.index<-c(which(names(ucr.fips)=="JW"), which(names(ucr.fips)=="AN"))

ucr.fips$total.age<-apply(ucr.fips[,age.index[1]:age.index[2]], 1, sum)

### figure out why some counties giving crazy total arrests (i.e more than 1mil in NYC)
# test<-ucr.fips%>%filter(FIPS=="04013")
# View(test[,c(17, 22:66, 124)])

ucr.county.offense<-ucr.fips%>%group_by(FIPS, COUNTYNAME, OFFENSE, YEAR)%>%
  summarise(tot.arrest=sum(total.age), 
            wht.arrest=(sum(JW)+sum(AW)),
            blk.arrest=(sum(JB)+sum(AB)),
            ai.arrest=(sum(JI)+sum(AI)),
            aa.arrest=sum(JA)+sum(AA),
            lat.arrest=sum(JH)+sum(AH))

# names(ucr.county.offense)[which(names(ucr.county.offense)=="OFFENSE")]<-"code"

# ucr.county.tot<-ucr.county.offense%>%group_by(FIPS, COUNTYNAME, YEAR)%>%
#   summarise(arrests=sum(tot.arrest), pop=max(pop))

# ucr.offense<-read.csv("ucr-offense-key.csv", stringsAsFactors = FALSE)
# ucr.offense$code<-substr(ucr.offense[,1],1, 3)
# ucr.offense$offense<-substr(ucr.offense[,1], 4, nchar(ucr.offense[,1]))
# ucr.offense$code[which(ucr.offense$code=="18 ")]<-"18"
# ucr.offense<-ucr.offense[,-1]

# ucr.county.offense<-left_join(ucr.county.offense, ucr.offense)
### create wide frame for UCR
### aggregate violent, property, drug, QoL, family, total
### violent = murder, manslaughter, rape, robbery, agg asst, other assaults
### violent codes = 011, 012, 020, 030, 040, 080
### prop = burg, larceny, mvt, arson, forgery, fraud, embezzlement, stolen prop
### prop codes = 050, 060, 070, 100, 110, 120, 130
### QoL = vandalism, liquor laws, drunkenness, disorderly conduct, vagrancy, suspicion, curfew and loitering
### QoL codes = 140, 220, 230, 240, 250, 270, 280
### Drug codes = 18 (total - check this!), 
### subsets: 180, 181, 182, 183, 184, 185, 186, 187, 188, 189
### to sort: weapons 150, prostitution 160, sex offenses 170, gambling (190:193), off. family children (200),
### DUI 210, runaways 290, NA 998


viol.codes<-c("011", "012", "020", "030", "040", "080")
prop.codes<-c("050", "060", "070", "100", "110", "120", "130")
drug.codes<-c("18")
qol.codes<-c("140","190", "220", "230", "240", "250", "270", "280")

viol<-ucr.county.offense%>%group_by(FIPS, YEAR)%>%filter(OFFENSE%in%viol.codes)%>%
  summarise(viol.tot = sum(tot.arrest),
            viol.wht = sum(wht.arrest),
            viol.blk = sum(blk.arrest),
            viol.ai = sum(ai.arrest),
            viol.aa = sum(aa.arrest),
            viol.lat = sum(lat.arrest))

drug<-ucr.county.offense%>%group_by(FIPS, YEAR)%>%filter(OFFENSE%in%drug.codes)%>%
  summarise(drug.tot = sum(tot.arrest),
            drug.wht = sum(wht.arrest),
            drug.blk = sum(blk.arrest),
            drug.ai = sum(ai.arrest),
            drug.aa = sum(aa.arrest),
            drug.lat = sum(lat.arrest))

qol<-ucr.county.offense%>%group_by(FIPS, YEAR)%>%filter(OFFENSE%in%qol.codes)%>%
  summarise(qol.tot = sum(tot.arrest),
            qol.wht = sum(wht.arrest),
            qol.blk = sum(blk.arrest),
            qol.ai = sum(ai.arrest),
            qol.aa = sum(aa.arrest),
            qol.lat = sum(lat.arrest))

tot<-ucr.county.offense%>%group_by(FIPS, YEAR)%>%
  summarise(all.tot = sum(tot.arrest),
            all.wht = sum(wht.arrest, na.rm=TRUE),
            all.blk = sum(blk.arrest, na.rm=TRUE),
            all.ai = sum(ai.arrest, na.rm=TRUE),
            all.aa = sum(aa.arrest, na.rm=TRUE),
            all.lat = sum(lat.arrest, na.rm=TRUE))

ucr.out<-full_join(full_join(full_join(tot, viol), drug) , qol)

setwd("R:/Project/NCANDS/ncands-csv/leoka/")

### read leoka data
files<-list.files()
leoka<-list()
for(i in 1:length(files)){
  leoka[[i]]<-read_dta(files[[i]])
}

leoka.dat<-bind_rows(list(leoka[[1]], leoka[[2]], leoka[[3]], leoka[[4]], leoka[[5]], leoka[[6]],
                          leoka[[7]], leoka[[8]], leoka[[9]], leoka[[10]], leoka[[11]], leoka[[12]],
                          leoka[[13]], leoka[[14]], leoka[[15]]))
names(leoka.dat)[c(4, 7)]<-c("ORI7", "YEAR")
leoka.dat<-leoka.dat%>%mutate(officers=V12+V15)%>%dplyr::select(ORI7, YEAR, officers)
leoka.dat<-left_join(leoka.dat, crosswalk)%>%dplyr::select(ORI7, YEAR, officers, FIPS)

leoka.county<-leoka.dat%>%group_by(FIPS, YEAR)%>%summarise(officers=sum(officers))

ucr.out<-left_join(ucr.out, leoka.county)
ucr.out<-ucr.out%>%filter(YEAR>2001)
setwd("R:/Project/NCANDS/ncands-csv/")
write.csv(ucr.out, file="ucr-county-offense-race.csv", row.names=FALSE)

setwd("R:/Project/NCANDS/ncands-csv/ucr-county")
files<-list.files()
county.dat<-list()
for(i in 1:length(files)){
  county<-read.fwf(files[[i]], widths=c(4, 1, 1, 4, 2, 3, 8, 3,1,8,6,6,5,
                                     5,4,4,5,5,5,5,5,3,5,4,5,3,4,5,5,4,
                                     4,5,5,5,4,4,5,5,5,5,4,5,4,3,3,4,4,
                                     5,5,5,5,4,6,4,5,5),
                   colClasses=c(rep("character", 6), NA))
  names(county)<-c("STUDYNO", "EDITION", "PART", 
                "IDNO", "FIPS_ST", "FIPS_CTY",
                "CPOPARST", "AG_ARRST", "JURFLAG",
                "COVIND", "GRNDTOT", "P1TOT", "P1VLNT",
                "P1PRPTY", "MURDER", "RAPE", "ROBBERY",
                "AGASSLT", "BURGLRY", "LARCENY", "MVTHEFT",
                "ARSON", "OTHASLT", "FRGYCNT", "FRAUD",
                "EMBEZL", "STLNPRP", "VANDLSM", "WEAPONS",
                "COMVICE", "SEXOFF", "DRUGTOT", "DRGSALE",
                "COCSALE", "MJSALE", "SYNSALE", "OTHSALE",
                "DRGPOSS", "COCPOSS", "MJPOSS", "SYNPOSS",
                "OTHPOSS", "GAMBLE", "BOOKMG", "NUMBERS",
                "OTGAMBL", "OFAGFAM", "DUI", "LIQUOR",
                "DRUNK", "DISORDR", "VAGRANT", "ALLOTHR",
                "SUSPICN", "CURFEW", "RUNAWAY")
  county$YEAR<-i+1999
  county.dat[[i]]<-county
  
  print(i)
}
### MISSING RULE FOR OFFENSES. FROM 2000 Codebook:
### if coverage indicator >0, arrest data is
### either accurate or agency imputed. if COVIND==0
### AND if all arrest categories ==0, then NA
### OFF CATS: 15 - 56
c.ucr<-county.dat[[1]]
for(i in 2:length(files)){
  c.ucr<-bind_rows(c.ucr, county.dat[[i]])
}
for(j in 10:ncol(c.ucr)){
  c.ucr[, j]<-as.numeric(c.ucr[,j])
}
for(i in 1:nrow(c.ucr)){
  if((c.ucr$COVIND[i]==0)&&(c.ucr[i,15:56]==0)){
    c.ucr[i,12:56]<-NA
  }
}

c.ucr$viol.tot<-c.ucr$P1VLNT
c.ucr$prop.tot<-c.ucr$P1PRPTY
c.ucr$drug.tot<-c.ucr$DRUGTOT
c.ucr<-c.ucr%>%mutate(qol.tot=VANDLSM+GAMBLE+LIQUOR+DRUNK+DISORDR+VAGRANT+SUSPICN+CURFEW)
c.ucr$all.tot<-c.ucr$GRNDTOT
c.ucr$FIPS_ST<-ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_ST)))<2, paste("0", as.character(as.numeric(c.ucr$FIPS_ST)), sep=""), c.ucr$FIPS_ST)
c.ucr$FIPS_CTY<-ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_CTY)))==2, paste("0",as.character(as.numeric(c.ucr$FIPS_CTY)), sep=""), ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_CTY)))==1,
                                                                                           paste("00",as.character(as.numeric(c.ucr$FIPS_CTY)), sep=""),
                                                                                           c.ucr$FIPS_CTY))

c.ucr$FIPS<-paste(c.ucr$FIPS_ST, c.ucr$FIPS_CTY, sep="")
c.ucr<-c.ucr%>%dplyr::select(FIPS, YEAR, viol.tot, prop.tot, drug.tot, all.tot, qol.tot)
ucr.county.out<-left_join(c.ucr, leoka.county)
setwd("R:/Project/NCANDS/ncands-csv/")
write.csv(ucr.county.out, file="ucr-county-offense.csv", row.names=FALSE)


### NYC IS STUPID.  NEED TO DO EDA ON QUALITY ACROSS COUNTIES.... PROBABLY CHECK ALL OF THEM. 36061 2013 and 2014 are too low
### WHAT IS ALGORITHM FOR CHECKING QUALITY?
### EMAIL ICPSR ABT NYC QUALITY FOR 13-14 - check total numbers against city stats
### NY EDA
### NYPD ORI7: "NY03030" 
# View(c.ucr%>%filter(FIPS=="36061"))

###EDA TO COMPARE AGENCY FILE COUNTY AGG TO COUNTY FILE, clear error on both files, I'll stick with the county file for now
###many zeroes reported non-zero in either file, but largely r=1
# test<-full_join(c.ucr, ucr.out)
# plot(all.tot~GRNDTOT, test)
# plot(drug.tot~DRUGTOT, test, pch=".")
# text(drug.tot~DRUGTOT, data=test,labels=test$FIPS) 
