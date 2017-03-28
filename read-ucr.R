rm(list=ls())
gc()

#.libPaths( c( .libPaths(), "U:/R") )

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
#setwd("D:/Sync/ucr/")

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

rm(ucr)
gc()
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

viol.codes<-c("011", "012", "020", "030", "040", "080")
prop.codes<-c("050", "060", "070", "100", "110", "120", "130")
drug.codes<-c("18")
qol.codes<-c("140","190", "220", "230", "240", "250", "270", "280")

ucr.fips$offense<-ifelse(ucr.fips$OFFENSE%in%viol.codes, "viol", 
                         ifelse(ucr.fips$OFFENSE%in%prop.codes, "prop",
                                ifelse(ucr.fips$OFFENSE%in%drug.codes, "drug",
                                       ifelse(ucr.fips$OFFENSE%in%qol.codes, "qol", "other"
                                       ))))

### rework into long format
ucr.fips<-ucr.fips%>%rename(wht=AW, blk=AB, ai=AI, aa=AA, lat=AH)

ucr.county.offense<-ucr.fips%>%group_by(FIPS, offense, YEAR)%>%
  summarise(all=sum(total.age), 
            wht=sum(wht),
            blk=sum(blk),
            ai=sum(ai),
            aa=sum(aa),
            lat=sum(lat))

ucr.tot<-ucr.fips%>%group_by(FIPS, YEAR)%>%
  summarise(all=sum(total.age), 
            wht=sum(wht),
            blk=sum(blk),
            ai=sum(ai),
            aa=sum(aa),
            lat=sum(lat))%>%
              mutate(offense="all")
ucr.county.offense<-full_join(ucr.county.offense, ucr.tot)

ucr.long<-gather(ucr.county.offense, race, arrest, -c(FIPS, YEAR, offense))

f.index<-c(which(names(ucr.fips)=="F18"), which(names(ucr.fips)=="F65"))
m.index<-c(which(names(ucr.fips)=="M18"), which(names(ucr.fips)=="M65"))
ucr.fips$female<-apply(ucr.fips[,f.index[1]:f.index[2]], 1, sum)
ucr.fips$male<-apply(ucr.fips[,m.index[1]:m.index[2]], 1, sum)

ucr.county.gender<-ucr.fips%>%group_by(FIPS, offense, YEAR)%>%
  summarise(female=sum(female), male=sum(male))

ucr.county.gender.all<-ucr.fips%>%group_by(FIPS, YEAR)%>%
  summarise(female=sum(female), male=sum(male))%>%mutate(offense="all")

ucr.county.gender<-full_join(ucr.county.gender, ucr.county.gender.all)

ucr.long.gender<-gather(ucr.county.gender, gender, arrest, -c(FIPS, YEAR, offense))%>%mutate(race="all")%>%
  rename(year=YEAR)


write.csv(ucr.long.gender, "ucr-county-offense-gender.csv", row.names=FALSE)

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



setwd("R:/Project/NCANDS/ncands-csv/leoka/")

### read leoka data
files<-list.files()
leoka<-list()
for(i in 1:length(files)){
  leoka[[i]]<-read_dta(files[[i]])
  print(i)
}

leoka.dat<-bind_rows(list(leoka[[1]], leoka[[2]], leoka[[3]], leoka[[4]], leoka[[5]], leoka[[6]],
                          leoka[[7]], leoka[[8]], leoka[[9]], leoka[[10]], leoka[[11]], leoka[[12]],
                          leoka[[13]], leoka[[14]], leoka[[15]]))
names(leoka.dat)[c(4, 7)]<-c("ORI7", "YEAR")
leoka.dat<-leoka.dat%>%mutate(officers=V12+V15)%>%dplyr::select(ORI7, YEAR, officers)
leoka.dat<-left_join(leoka.dat, crosswalk)%>%dplyr::select(ORI7, YEAR, officers, FIPS)

leoka.county<-leoka.dat%>%group_by(FIPS, YEAR)%>%summarise(officers=sum(officers))

# setwd("R:/Project/NCANDS/ncands-csv/")
# write.csv(ucr.out, file="ucr-county-offense-race.csv", row.names=FALSE)

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

c.ucr$viol<-c.ucr$P1VLNT
c.ucr$prop<-c.ucr$P1PRPTY
c.ucr$drug<-c.ucr$DRUGTOT
c.ucr<-c.ucr%>%mutate(qol=VANDLSM+GAMBLE+LIQUOR+DRUNK+DISORDR+VAGRANT+SUSPICN+CURFEW)
c.ucr$all<-c.ucr$GRNDTOT
c.ucr$FIPS_ST<-ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_ST)))<2, 
                      paste("0", as.character(as.numeric(c.ucr$FIPS_ST)), sep=""), c.ucr$FIPS_ST)
c.ucr$FIPS_CTY<-ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_CTY)))==2, 
                       paste("0",as.character(as.numeric(c.ucr$FIPS_CTY)), sep=""), 
                       ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_CTY)))==1,
                              paste("00",as.character(as.numeric(c.ucr$FIPS_CTY)), sep=""),
                                                                                    c.ucr$FIPS_CTY))

c.ucr$FIPS<-paste(c.ucr$FIPS_ST, c.ucr$FIPS_CTY, sep="")
c.ucr<-c.ucr%>%dplyr::select(FIPS, YEAR, viol, prop, drug, all, qol)

c.ucr.long<-gather(c.ucr, offense, arrest, -c(FIPS, YEAR))
c.ucr.long$race<-"all"
ucr.long<-ucr.long%>%filter(race!="all")
ucr.county.out<-full_join(c.ucr.long, ucr.long)

ucr.out<-left_join(ucr.county.out, leoka.county)

setwd("R:/Project/NCANDS/ncands-csv/")
write.csv(ucr.out, file="ucr-county-offense.csv", row.names=FALSE)

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

#### Offense data

setwd("R:/Project/NCANDS/ncands-csv/ucr-county-offenses/data")
files<-list.files()
### structure changes after 2008 - 27644, index 9
### caution on 2009 - 2014, INDEX and MODINDX are 
### VIOLENT and PROPERTY for those years, dropping vars
county.dat<-list()
cnames<-c("STUDYNO", "EDITION", "PART",
                 "IDNO", "FIPS_ST", "FIPS_CTY",
                 "CPOPARST", "CPOPCRIM",
                 "AG_ARRST", "AG_OFF",
                 "COVIND", "INDEX", "MODINDX",
                 "MURDER", "RAPE", "ROBBERY",
                 "AGASSLT", "BURGLRY", "LARCENY", "MVTHEFT",
                 "ARSON")
widths=c(4,1,1,4,2,3,8,8,3,3,8,6,6,4,4,5,5,6,6,6,4)


for(i in 1:length(files)){
  county<-read.fwf(files[[i]], widths=widths,
                   colClasses=c(rep("numeric", 21)))
  names(county)<-cnames
  county$YEAR<-i+1999
  county.dat[[i]]<-county
  print(i)
}



### MISSING RULE FOR OFFENSES. FROM 2000 Codebook:
### if coverage indicator >0, arrest data is
### either accurate or agency imputed. if COVIND==0
### AND if all arrest categories ==0, then NA
### OFF CATS: 15 - 56
c.offense.ucr<-county.dat[[1]]
for(i in 2:length(files)){
  c.offense.ucr<-bind_rows(c.offense.ucr, county.dat[[i]])
}
for(i in 1:nrow(c.offense.ucr)){
  if(c.offense.ucr$COVIND[i]==0){
    c.offense.ucr[i, 12:21]<-NA
  }
}

c.offense.ucr$FIPS_ST<-ifelse(nchar(as.character(as.numeric(c.offense.ucr$FIPS_ST)))<2, 
                      paste("0", as.character(as.numeric(c.offense.ucr$FIPS_ST)), sep=""), c.offense.ucr$FIPS_ST)
c.offense.ucr$FIPS_CTY<-ifelse(nchar(as.character(as.numeric(c.offense.ucr$FIPS_CTY)))==2, 
                       paste("0",as.character(as.numeric(c.offense.ucr$FIPS_CTY)), sep=""), 
                       ifelse(nchar(as.character(as.numeric(c.offense.ucr$FIPS_CTY)))==1,
                              paste("00",as.character(as.numeric(c.offense.ucr$FIPS_CTY)), sep=""),
                              c.offense.ucr$FIPS_CTY))

c.offense.ucr$FIPS<-paste(c.offense.ucr$FIPS_ST, c.offense.ucr$FIPS_CTY, sep="")

get.mav <- function(bp,n=5){
  require(zoo)
  if(is.na(bp[1])) bp[1] <- mean(bp,na.rm=TRUE)
  bp <- na.locf(bp,na.rm=FALSE)
  if(length(bp)<n) return(bp)
  c(bp[1:(n-1)],rollapply(bp,width=n,mean,align="right"))  
}

c.offense.ucr <- with(c.offense.ucr,c.offense.ucr[order(FIPS,YEAR),])

c.offense.ucr$MURDER_mav <- 
  unlist(aggregate(MURDER~FIPS,c.offense.ucr,get.mav,na.action=NULL,n=5)$MURDER)
c.offense.ucr<-c.offense.ucr%>%dplyr::select(FIPS, YEAR, MURDER_mav)%>%
  rename(year=YEAR)

write.csv(c.offense.ucr, "R:/Project/NCANDS/ncands-csv/ucr-murder-known.csv", row.names=FALSE)

ucr.out<-ucr.out%>%rename(year=YEAR)

ucr.reduced<-ucr.out%>%dplyr::select(FIPS, year, officers)%>%group_by(FIPS, year)
ucr.reduced<-ucr.reduced[!duplicated(ucr.reduced),]
gender.merge<-left_join(ucr.long.gender, ucr.reduced)

test<-full_join(ucr.out%>%mutate(gender="all"), gender.merge)

test2<-left_join(test, c.offense.ucr)

###make NAs for missing offense/FIPS,year,race,gender pairs

fips<-unique(test2$FIPS)
year<-unique(test2$year)
race<-unique(test2$race)
gender<-unique(test2$gender)
offense<-unique(test2$offense)

m1<-expand.grid(FIPS=fips, year=year, race=race, gender=gender, offense=offense)
m1<-m1%>%filter(!(race!="all"&gender%in%c("male", "female")))

t1<-left_join(m1, test2)

write.csv(t1, "R:/Project/NCANDS/ncands-csv/ucr-gender-plus.csv", row.names=FALSE)

##quality test
table(t1$offense, t1$race)
table(test2$offense, test2$gender)