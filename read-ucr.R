rm(list=ls())
gc()

.libPaths( c( .libPaths(), "U:/R") )

library(readr)
library(dplyr)
library(tidyr)
library(lme4)
library(texreg)
library(tidyr)
library(data.table)
library(ggplot2)
library(foreign)
library(haven)

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

### load UCR crosswalk to convert into FIPS, ORI in UCR, ORI7 in crosswalk should match
crosswalk<-read.delim("35158-0001-Data.tsv", sep="\t", colClasses=c(rep(NA, 3), rep("character", 3), rep(NA, 40)),
                      stringsAsFactors = FALSE)

ucr.fips<-left_join(ucr.dat, crosswalk, by="ORI7")

### drop agencies w/o county fips
ucr.fips<-ucr.fips[!(is.na(ucr.fips$FIPS)),]


##PROBLEM WITH SUMMARY OFFENSE CODES, FILTER OUT SUBSET CODES
drug.codes<-as.character(180:189)
gambling.codes<-as.character(191:193)
ucr.fips<-ucr.fips%>%filter((!(OFFENSE%in%drug.codes))&
                   (!(OFFENSE%in%gambling.codes)))

### mark NA those with only agency flag
ucr.fips[which(ucr.fips$CONTENTS==1), which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)]<-NA
### some negative reported values plus coded missingness
ucr.fips[ucr.fips==999998]<-NA
ucr.fips[ucr.fips<0]<-NA
### switch from drop to place NA - want to include NA in county sums if appropriate
ucr.fips[ucr.fips$REPORT==3,which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)]<-NA
### report missing as counties with no race data
ucr.fips[ucr.fips$ADJUST>3,which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)]<-NA

#### check for negative values in arrest counts - sub NA

####Create county-level counts of arrest by offense type, want total arrest, violent arrest, fam viol arrest, 
### Age is more inclusive, many more arrests counted, appears race underrreported

age.index<-c(which(names(ucr.fips)=="M0_9"), which(names(ucr.fips)=="F65"))
#race.index<-c(which(names(ucr.fips)=="JW"), which(names(ucr.fips)=="AN"))


ucr.fips$total.age<-apply(ucr.fips[,age.index[1]:age.index[2]], 1, sum)

### figure out why some counties giving crazy total arrests (i.e more than 1mil in NYC)
# test<-ucr.fips%>%filter(FIPS=="04013")
# View(test[,c(17, 22:66, 124)])


####FOR LATER - COULD MAKE AGENCY COUNTS FOR DATA IN BOTH UCR AND CROSSWALK TO CHECK FOR COMPLETENESS

viol.codes<-c("011", "012", "020", "030", "040", "080")
prop.codes<-c("050", "060", "070", "100", "110", "120", "130")
drug.codes<-c("18")
qol.codes<-c("140","190", "220", "230", "240", "250", "270", "280")

ucr.fips$offense<-ifelse(ucr.fips$OFFENSE%in%viol.codes, "viol", 
                         ifelse(ucr.fips$OFFENSE%in%prop.codes, "prop",
                                ifelse(ucr.fips$OFFENSE%in%drug.codes, "drug",
                                       ifelse(ucr.fips$OFFENSE%in%qol.codes, "qol", "other"
                                       ))))

### drop other and property
ucr.fips<-ucr.fips%>%filter(!(offense%in%c("prop", "other")))


### rework into wide format
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
              mutate(offense="arrest")

ucr.county.offense<-full_join(ucr.county.offense, ucr.tot)
ucr.county.offense<-ucr.county.offense%>%dplyr::select(-aa, -lat)


### gather to long, join race-offense column, spread for wide
ucr.long<-gather(ucr.county.offense, race, arrest, -c(FIPS, YEAR, offense))
ucr.long<-ucr.long%>%unite(race.off, c(offense, race), sep=".")
ucr.wide<-spread(ucr.long, race.off, arrest, fill=NA)

### by gender

f.index<-c(which(names(ucr.fips)=="F18"), which(names(ucr.fips)=="F65"))
m.index<-c(which(names(ucr.fips)=="M18"), which(names(ucr.fips)=="M65"))
ucr.fips$female<-apply(ucr.fips[,f.index[1]:f.index[2]], 1, sum)
ucr.fips$male<-apply(ucr.fips[,m.index[1]:m.index[2]], 1, sum)

ucr.county.gender<-ucr.fips%>%group_by(FIPS, offense, YEAR)%>%
  summarise(female=sum(female), male=sum(male))

ucr.county.gender.all<-ucr.fips%>%group_by(FIPS, YEAR)%>%
  summarise(female=sum(female), male=sum(male))%>%mutate(offense="arrest")

ucr.county.gender<-full_join(ucr.county.gender, ucr.county.gender.all)

ucr.long.gender<-gather(ucr.county.gender, gender, arrest, -c(FIPS, YEAR, offense))
ucr.long.gender<-ucr.long.gender%>%unite(gender.off, c(offense, gender), sep=".")
ucr.wide.gender<-spread(ucr.long.gender, gender.off, arrest, fill=NA)

ucr.agencyfile<-full_join(ucr.wide.gender, ucr.wide)

# 
# ### create wide frame for UCR
# ### aggregate violent, property, drug, QoL, family, total
# ### violent = murder, manslaughter, rape, robbery, agg asst, other assaults
# ### violent codes = 011, 012, 020, 030, 040, 080
# ### prop = burg, larceny, mvt, arson, forgery, fraud, embezzlement, stolen prop
# ### prop codes = 050, 060, 070, 100, 110, 120, 130
# ### QoL = vandalism, liquor laws, drunkenness, disorderly conduct, vagrancy, suspicion, curfew and loitering
# ### QoL codes = 140, 220, 230, 240, 250, 270, 280
# ### Drug codes = 18 (total - check this!), 
# ### subsets: 180, 181, 182, 183, 184, 185, 186, 187, 188, 189
# ### to sort: weapons 150, prostitution 160, sex offenses 170, gambling (190:193), off. family children (200),
# ### DUI 210, runaways 290, NA 998
# 

###############EMPLOYEE DATA
### using ICPSR FILES
#  [1] "03445-0001-Data.dta" "03749-0001-Data.dta" "03996-0001-Data.dta"
#  [4] "04269-0001-Data.dta" "04462-0001-Data.dta" "04719-0001-Data.dta"
#  [7] "22402-0001-Data.dta" "25104-0001-Data.dta" "27646-0001-Data.dta"
# [10] "30765-0001-Data.dta" "33525-0001-Data.dta" "34584-0001-Data.dta"
# [13] "35020-0001-Data.dta" "36119-0001-Data.dta" "36395-0001-Data.dta"

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

# Could proxy measurement error with total age / county file to get error variance as pct of total variance
# 
# # setwd("R:/Project/NCANDS/ncands-csv/")
# # write.csv(ucr.out, file="ucr-county-offense-race.csv", row.names=FALSE)
# 
# setwd("R:/Project/NCANDS/ncands-csv/ucr-county")
# files<-list.files()
# county.dat<-list()
# for(i in 1:length(files)){
#   county<-read.fwf(files[[i]], widths=c(4, 1, 1, 4, 2, 3, 8, 3,1,8,6,6,5,
#                                      5,4,4,5,5,5,5,5,3,5,4,5,3,4,5,5,4,
#                                      4,5,5,5,4,4,5,5,5,5,4,5,4,3,3,4,4,
#                                      5,5,5,5,4,6,4,5,5),
#                    colClasses=c(rep("character", 6), NA))
#   names(county)<-c("STUDYNO", "EDITION", "PART",
#                 "IDNO", "FIPS_ST", "FIPS_CTY",
#                 "CPOPARST", "AG_ARRST", "JURFLAG",
#                 "COVIND", "GRNDTOT", "P1TOT", "P1VLNT",
#                 "P1PRPTY", "MURDER", "RAPE", "ROBBERY",
#                 "AGASSLT", "BURGLRY", "LARCENY", "MVTHEFT",
#                 "ARSON", "OTHASLT", "FRGYCNT", "FRAUD",
#                 "EMBEZL", "STLNPRP", "VANDLSM", "WEAPONS",
#                 "COMVICE", "SEXOFF", "DRUGTOT", "DRGSALE",
#                 "COCSALE", "MJSALE", "SYNSALE", "OTHSALE",
#                 "DRGPOSS", "COCPOSS", "MJPOSS", "SYNPOSS",
#                 "OTHPOSS", "GAMBLE", "BOOKMG", "NUMBERS",
#                 "OTGAMBL", "OFAGFAM", "DUI", "LIQUOR",
#                 "DRUNK", "DISORDR", "VAGRANT", "ALLOTHR",
#                 "SUSPICN", "CURFEW", "RUNAWAY")
#   county$YEAR<-i+1999
#   county.dat[[i]]<-county
# 
#   print(i)
# }
# 
# ### this is the county file - going to axe it for now. weird to classify missing across multiple datasets
# ### MISSING RULE FOR OFFENSES. FROM 2000 Codebook:
# ### if coverage indicator >0, arrest data is
# ### either accurate or agency imputed. if COVIND==0
# ### AND if all arrest categories ==0, then NA
# ### OFF CATS: 15 - 56
# c.ucr<-county.dat[[1]]
# for(i in 2:length(files)){
#   c.ucr<-bind_rows(c.ucr, county.dat[[i]])
# }
# for(j in 10:ncol(c.ucr)){
#   c.ucr[, j]<-as.numeric(c.ucr[,j])
# }
# for(i in 1:nrow(c.ucr)){
#   if((c.ucr$COVIND[i]==0)&&(c.ucr[i,15:56]==0)){
#     c.ucr[i,12:56]<-NA
#   }
# }
# 
# c.ucr$viol<-c.ucr$P1VLNT
# c.ucr$prop<-c.ucr$P1PRPTY
# c.ucr$drug<-c.ucr$DRUGTOT
# c.ucr<-c.ucr%>%mutate(qol=VANDLSM+GAMBLE+LIQUOR+DRUNK+DISORDR+VAGRANT+SUSPICN+CURFEW)
# c.ucr$all<-c.ucr$GRNDTOT
# c.ucr$FIPS_ST<-ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_ST)))<2, 
#                       paste("0", as.character(as.numeric(c.ucr$FIPS_ST)), sep=""), c.ucr$FIPS_ST)
# c.ucr$FIPS_CTY<-ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_CTY)))==2, 
#                        paste("0",as.character(as.numeric(c.ucr$FIPS_CTY)), sep=""), 
#                        ifelse(nchar(as.character(as.numeric(c.ucr$FIPS_CTY)))==1,
#                               paste("00",as.character(as.numeric(c.ucr$FIPS_CTY)), sep=""),
#                                                                                     c.ucr$FIPS_CTY))
# 
# c.ucr$FIPS<-paste(c.ucr$FIPS_ST, c.ucr$FIPS_CTY, sep="")
# c.ucr<-c.ucr%>%dplyr::select(FIPS, YEAR, viol, prop, drug, all, qol)
# 
# c.ucr.long<-gather(c.ucr, offense, arrest, -c(FIPS, YEAR))
# c.ucr.long$race<-"all"
# ucr.long<-ucr.long%>%filter(race!="all")
# ucr.county.out<-full_join(c.ucr.long, ucr.long)
# 
# ucr.out<-left_join(ucr.county.out, leoka.county)
# 
# setwd("R:/Project/NCANDS/ncands-csv/")
# write.csv(ucr.out, file="ucr-county-offense.csv", row.names=FALSE)

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

#### Offense data at the county-level for homicide moving ave using ICPSR files
# [1] "03451-0004-Data.txt" "03721-0004-Data.txt" "04009-0004-Data.txt"
#  [4] "04360-0004-Data.txt" "04466-0004-Data.txt" "04717-0004-Data.txt"
#  [7] "23780-0004-Data.txt" "25114-0004-Data.txt" "27644-0004-Data.txt"
# [10] "30763-0004-Data.txt" "33523-0004-Data.txt" "34582-0004-Data.txt"
# [13] "35019-0004-Data.txt" "36117-0004-Data.txt"

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

#write.csv(c.offense.ucr, "R:/Project/NCANDS/ncands-csv/ucr-murder-known.csv", row.names=FALSE)


####################################################
#### MERGE leoka.county, c.offense.ucr, ucr.agenycfile

# ### this is to complete long file - not needed for wide
# ucr.agencyfile$FIPS<-factor(ucr.agencyfile$FIPS); ucr.agencyfile$offense<-factor(ucr.agencyfile$offense)
# ucr.agencyfile$race<-factor(ucr.agencyfile$race); ucr.agencyfile$gender<-factor(ucr.agencyfile$gender)
# ucr.agencyfile$YEAR<-factor(ucr.agencyfile$YEAR)
# ucr.agencyfile<-as.data.frame(ucr.agencyfile)
# 
# ### fill in missing combinations
# ucr.agencyfile<-ucr.agencyfile%>%complete(FIPS, YEAR, race, offense)
# ucr.agencyfile<-ucr.agencyfile%>%complete(FIPS, YEAR, gender, offense)
# 
# ### drop nonsense NA combos
# ucr.agencyfile<-ucr.agencyfile%>%filter(!is.na(gender))%>%filter(!is.na(race))
# 
# ucr.agencyfile$FIPS<-as.character(ucr.agencyfile$FIPS)
# ucr.agencyfile$YEAR<-as.numeric(ucr.agencyfile$YEAR)

c.offense.ucr<-c.offense.ucr%>%rename(YEAR=year)

length(unique(leoka.county$FIPS)); length(unique(c.offense.ucr$FIPS)); length(unique(ucr.agencyfile$FIPS))

ucr.out<-left_join(ucr.agencyfile, leoka.county)
ucr.out<-left_join(ucr.out, c.offense.ucr)

write.csv(ucr.out, "R:/Project/NCANDS/ncands-csv/ucr-wide.csv", row.names=FALSE)

