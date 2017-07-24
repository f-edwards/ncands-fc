rm(list=ls())
gc()

<<<<<<< HEAD
=======
.libPaths( c( .libPaths(), "U:/R") )

>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07
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

<<<<<<< HEAD
setwd("C:/Users/fre9/Box Sync/ncands-csv/")
#setwd("D:/Sync/ucr/")

#######################################################
##### Read in UCR arrests by age, sex, and race 2002 - 
#######################################################

### Use stata files to read in data - using haven

dir<-"C:/Users/fre9/Box Sync/ncands-csv/ucr/"
files<-c("ICPSR_03997/ICPSR_03997/DS0001/03997-0001-Data.dta",
=======
setwd("R:/Project/NCANDS/ncands-csv/")
#setwd("D:/Sync/ucr/")

###UCR READ
### load 2012 UCR, loads object da35018.001
dir<-"R:/Project/NCANDS/ucr/"
files<-c("ICPSR_03729/ICPSR_03729/DS0001/03729-0001-Data.dta",
         "ICPSR_03997/ICPSR_03997/DS0001/03997-0001-Data.dta",
>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07
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

ucr.dat<-bind_rows(list(ucr[[1]], ucr[[2]], ucr[[3]], ucr[[4]]), ucr[[5]], ucr[[6]],
                   ucr[[7]], ucr[[8]], ucr[[9]], ucr[[10]], ucr[[11]], ucr[[12]],
<<<<<<< HEAD
                   ucr[[13]], ucr[[14]])

ucr.dat<-ucr.dat%>%
  select(ORI, YEAR, OFFENSE, M0_9:AN)

ucr.dat<-ucr.dat%>%
  mutate_all(funs(
    replace(., . == 99999, 0)))%>%
  mutate_all(funs(
    replace(., .==99998, NA)))

rm(ucr)
gc()


########### check for presence of negatives in annual counts later - codebook says these
########### are used by agencies to adjust erroneous prior reports down, but can't have negative counts in models

###########################
#### 2015 data is only available monthly as of 7/21/17
###########################

ucr.2015<-read_dta("C:/Users/fre9/Box Sync/ncands-csv/ucr/ICPSR_36794/ICPSR_36794/DS0001/36794-0001-Data.dta")

####### 99999 is none/not reported on arrest measures, 99998 is not applicable.
####### could think more carefully about missingness - check number of reported months
####### check for any reports in ORI/month to confirm missing or zero
####### but this is recommended strategy for 

ucr.2015<-ucr.2015%>%
  mutate_all(funs(
    replace(., . == 99999, 0)))%>%
  mutate_all(funs(
    replace(., .==99998, NA)))


ucr.2015<-ucr.2015%>%
  group_by(ORI, YEAR, OFFENSE)%>%
  summarise_at(vars(M0_9:JN),
    sum)

ucr.dat<-bind_rows(ucr.dat, ucr.2015)

names(ucr.dat)[which(names(ucr.dat)=="ORI")]<-"ORI7"

##################################################
### Crosswalk onto FIPS
##################################################

=======
                   ucr[[13]], ucr[[14]], ucr[[15]])

rm(ucr)
gc()
names(ucr.dat)[which(names(ucr.dat)=="ORI")]<-"ORI7"

>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07
### load UCR crosswalk to convert into FIPS, ORI in UCR, ORI7 in crosswalk should match
crosswalk<-read.delim("35158-0001-Data.tsv", sep="\t", colClasses=c(rep(NA, 3), rep("character", 3), rep(NA, 40)),
                      stringsAsFactors = FALSE)

ucr.fips<-left_join(crosswalk, ucr.dat, by="ORI7")

### drop agencies w/o county fips
#ucr.fips<-ucr.fips[!(is.na(ucr.fips$FIPS)),]

### mark NA those with only agency flag
ucr.fips[which(ucr.fips$CONTENTS==1), which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)]<-NA
### some negative reported values plus coded missingness
ucr.fips[ucr.fips==999998]<-NA
ucr.fips[ucr.fips<0]<-NA
### switch from drop to place NA - want to include NA in county sums if appropriate
# ucr.fips[,which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)]<-ifelse(ucr.fips$REPORT==3, NA,
#                                                                  ucr.fips[,which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)])
# 
# ucr.fips[,which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)]<-ifelse(ucr.fips$ADJUST>3, NA,
#                                                                  ucr.fips[,which(names(ucr.fips)=="M0_9"):ncol(ucr.fips)])
### report missing as counties with no race data
#ucr.fips[ucr.fips$ADJUST>3,which(names(ucr.fips)=="JW"):ncol(ucr.fips)]<-NA

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

<<<<<<< HEAD
viol.codes<-c("011", "012", "020", "030", "040", "080")
=======
viol.codes<-c("011", "012", "020", "030", "040")
>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07
prop.codes<-c("050", "060", "070", "100", "110", "120", "130")
drug.codes<-c("18")
qol.codes<-c("140","190", "220", "230", "240", "250", "270", "280")

ucr.fips$offense<-ifelse(ucr.fips$OFFENSE%in%viol.codes, "viol", 
                         ifelse(ucr.fips$OFFENSE%in%prop.codes, "prop",
                                ifelse(ucr.fips$OFFENSE%in%drug.codes, "drug",
                                       ifelse(ucr.fips$OFFENSE%in%qol.codes, "qol", "other"
                                       ))))

### drop other and property
#ucr.fips<-ucr.fips%>%filter(!(offense%in%c("prop", "other")))


### rework into wide format
ucr.fips<-ucr.fips%>%rename(wht=AW, blk=AB, ai=AI, aa=AA, lat=AH)
### THERE's zero data on latinos


ucr.ori.offense<-ucr.fips%>%group_by(FIPS, YEAR)%>%
  filter(offense=="drug")%>%
  summarise(drug.all=sum(total.age),
            drug.wht=sum(wht),
            drug.blk=sum(blk),
            drug.ai=sum(ai),
            drug.aa=sum(aa))%>%
  full_join(
    ucr.fips%>%group_by(FIPS, YEAR)%>%
  filter(offense=="viol")%>%
  summarise(viol.all=sum(total.age),
            viol.wht=sum(wht),
            viol.blk=sum(blk),
            viol.ai=sum(ai),
            viol.aa=sum(aa)))%>%
  full_join(
    ucr.fips%>%group_by(FIPS, YEAR)%>%
<<<<<<< HEAD
  summarise(arrest.all=sum(total.age),
            arrest.wht=sum(wht),
            arrest.blk=sum(blk),
            arrest.ai=sum(ai),
            arrest.aa=sum(aa)))
=======
      filter(offense=="prop")%>%
  summarise(prop.all=sum(total.age),
            prop.wht=sum(wht),
            prop.blk=sum(blk),
            prop.ai=sum(ai),
            prop.aa=sum(aa)))%>%
  full_join(
    ucr.fips%>%group_by(FIPS, YEAR)%>%
      filter(offense=="qol")%>%
  summarise(qol.all=sum(total.age),
            qol.wht=sum(wht),
            qol.blk=sum(blk),
            qol.ai=sum(ai),
            qol.aa=sum(aa)))

>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07

#write.csv(ucr.ori.out, "ucr-fips-race-ts.csv")

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

<<<<<<< HEAD
setwd("C:/Users/fre9/Box Sync/ncands-csv/leoka/")
=======
setwd("R:/Project/NCANDS/ncands-csv/leoka/")
>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07

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
leoka.dat<-leoka.dat%>%mutate(officers=V12+V15)%>%
  dplyr::select(ORI7, YEAR, officers)
leoka.dat<-left_join(leoka.dat, crosswalk)%>%
  dplyr::select(ORI7, YEAR, officers, FIPS)

leoka.county<-leoka.dat%>%group_by(FIPS, YEAR)%>%summarise(officers=sum(officers))

############# COUNTY FILES - NO AGENCY / RACE / GENDER BREAKOUT - USE FOR TOTAL ARRESTS
<<<<<<< HEAD
setwd("C:/Users/fre9/Box Sync/ncands-csv/ucr-county")
=======
setwd("R:/Project/NCANDS/ncands-csv/ucr-county")
>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07
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

# ### this is the county file - going to axe it for now. weird to classify missing across multiple datasets
# ### MISSING RULE FOR OFFENSES. FROM 2000 Codebook:
# ### if coverage indicator >0, arrest data is
# ### either accurate or agency imputed. if COVIND==0
# ### AND if all arrest categories ==0, then NA
# ### OFF CATS: 15 - 56
c.ucr<-county.dat[[1]]
for(i in 2:length(files)){
  c.ucr<-bind_rows(c.ucr, county.dat[[i]])
}
for(j in 10:ncol(c.ucr)){
  c.ucr[, j]<-as.numeric(c.ucr[,j])
}

### following codebook missing data rules
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

### NYC IS STUPID.  NEED TO DO EDA ON QUALITY ACROSS COUNTIES.... PROBABLY CHECK ALL OF THEM. 36061 2013 and 2014 are too low
### WHAT IS ALGORITHM FOR CHECKING QUALITY?
### EMAIL ICPSR ABT NYC QUALITY FOR 13-14 - check total numbers against city stats
### NY EDA
### NYPD ORI7: "NY03030"
# View(c.ucr%>%filter(FIPS=="36061"))

#the 00-03, 13/14 data for nyc are wrong
c.ucr[which((c.ucr$FIPS=="36061")&(c.ucr$YEAR>2012)), 3:7]<-NA
c.ucr[which((c.ucr$FIPS=="36061")&(c.ucr$YEAR<2003)), 3:7]<-NA

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

<<<<<<< HEAD
setwd("C:/Users/fre9/Box Sync/ncands-csv/ucr-county-offenses/data")
=======
setwd("R:/Project/NCANDS/ncands-csv/ucr-county-offenses/data")
>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07
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

c.offense.ucr<-c.offense.ucr%>%
  mutate(VIOLENT.OFFENSE=MURDER+RAPE+ROBBERY+AGASSLT)%>%
<<<<<<< HEAD
  dplyr::select(FIPS, YEAR, MURDER, VIOLENT)
=======
  dplyr::select(FIPS, YEAR, MURDER, VIOLENT.OFFENSE)
>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07

####################################################
#### MERGE leoka.county, c.offense.ucr, ucr.agenycfile

<<<<<<< HEAD
ucr.out<-full_join(ucr.ori.offense, leoka.county)
ucr.out<-full_join(ucr.out, c.offense.ucr)
ucr.out<-full_join(ucr.out, c.ucr)%>%
  dplyr::select(-arrest.all, -drug.all, -viol.all)%>%
  filter(!(is.na(YEAR)))

write.csv(ucr.out, "C:/Users/fre9/Box Sync/ncands-csv/ucr-county-arrest-offense.csv", row.names=FALSE)
=======
### to mark as missing data for the agency file - 18015 2013; 21073 2005; 22071 all years; 42063 all years; 
### 42101 all years (philly and indiana county look switched in the data); 
### NYC COUNTIES: 36005 047 059 061 081 085 #### CHECK ALL THESE going to NA all of them, maybe manually check later
### NA the county file for 13, 14
error<-which(ucr.ori.offense$FIPS%in%(c("36005", "36047", "36059", "36061", "36081", "36085", "22071", "42063",
  "42101")))
error<-c(error, which((ucr.ori.offense$FIPS=="18015")&(ucr.ori.offense$YEAR==2013)))
error<-c(error, which((ucr.ori.offense$FIPS=="21073")&(ucr.ori.offense$YEAR==2005)))

ucr.ori.offense[error,3:ncol(ucr.ori.offense)]<-NA

### NYC county data is off for county file for 13, 14
error<-which((c.ucr$FIPS%in%(c("36005", "36047", "36059", "36061", "36081", "36085"))&
    c.ucr$YEAR>2012))
c.ucr[error, 3:7]<-NA

#### use new york crime data for county file
### data accessed at https://data.ny.gov/Public-Safety/Adult-Arrests-by-County-Beginning-1970/rikd-mt35
ny<-fread("R:/Project/NCANDS/ncands-csv/Adult_Arrests_by_County___Beginning_1970.csv")
nyfips<-fread("R:/Project/NCANDS/ncands-csv/nyfips.csv", colClasses = "character")
nycw<-cbind(unique(ny$County), paste("36", nyfips$V1, sep=""))
nycw<-as.data.frame(nycw); names(nycw)<-c("County", "FIPS")
ny<-left_join(ny, nycw)
### to match with c.ucr
ny<-ny%>%
  mutate(viol=`Violent Felony`,
    all=Total,
    drug=`Drug Felony`+`Drug Misd`)%>%
  rename(YEAR=Year)%>%
  dplyr::select(FIPS, YEAR, viol, drug, all)%>%
  filter(YEAR>1999)%>%
  filter(!(FIPS=="36NA"))
ny$FIPS<-as.character(ny$FIPS)

c.ucr<-full_join(c.ucr%>%
  filter(substr(FIPS, 1, 2)!="36"),
  ny)

ucr.out<-full_join(ucr.ori.offense, leoka.county)
ucr.out<-full_join(ucr.out, c.offense.ucr)
ucr.out<-full_join(ucr.out, c.ucr)%>%
  filter(!(is.na(YEAR)))

# #quality check on agency-level data
# z<-which(abs(ucr.out$drug - ucr.out$drug.all)>5000)
# table(ucr.out[z, "FIPS"])
# View(ucr.out[z,])
# ### to mark as missing data for the agency file - 18015 2013; 21073 2005; 22071 all years; 42063 all years; 
# ### 42101 all years (philly and indiana county look switched in the data); 
# ### NYC COUNTIES: 36005 047 059 061 081 085 #### CHECK ALL THESE going to NA all of them, maybe manually check later
# ### NA the county file for 13, 14

#check
hist(ucr.out$drug-ucr.out$drug.all)

#### DO CLOSE COMPARISON OF COUNTS - CHECK ON NAs

write.csv(ucr.out, "R:/Project/NCANDS/ncands-csv/ucr-county-arrest-offense.csv", row.names=FALSE)
>>>>>>> 5387fa18b59700e4a2c0402004b61b4f27f86e07


  
