rm(list=ls())
require(TTR)
require(data.table)
require(dplyr)
library(tidyr)
require(Amelia)
library(nlme)
library(lme4)
library(ggplot2)

path<-"H:/data/apes/"
years<-c("2012", "2011", "2010", "2009", "2008", "2007")
set.seed(1)

folders<-NULL
files<-list()
for(i in 1:length(years)){
  folders[i]<-paste(path, years[i], sep="")
  files[[i]]<-list.files(folders[i])
}

dat<-NULL
for(i in 1:length(folders)){
  setwd(folders[i])
  if(years[i] %in% c("2012", "2008", "2007")){
    for(j in 1:length(files[[i]])){
      temp<-fread(files[[i]][j], sep=",", skip= 9,
             colClasses=c(rep("character", 5), 
                          "numeric","character",
                          "numeric", "character",
                          "numeric", "character",
                          "numeric", "character",
                          "numeric", "character",
                          "numeric", "numeric",
                          rep("character", 4)),
             drop=c(7,9, 11, 13, 15, 17))
      temp$year<-years[i]
      dat<-rbind(dat, temp)
    }
  }
  if(years[i] %in% c("2011", "2010", "2009")){
    for(j in 1:length(files[[i]])){
      temp<-fread(files[[i]][j], sep=",", skip= 12,
                  colClasses=c(rep("character", 5), 
                               "numeric","character",
                               "numeric", "character",
                               "numeric", "character",
                               "numeric", "character",
                               "numeric", "character",
                               "numeric", "numeric",
                               rep("character", 4)),
                  drop=c(7,9, 11, 13, 15, 17))
      temp$year<-years[i]
      dat<-rbind(dat, temp)
    }
  }
}
setwd("H:/")

names(dat)<-c("state", "gov.type", "county",
              "govt-name", "funct", "ft.emp",
              "ft.pay", "pt.emp", "pt.pay",
              "pt.hrs", "ft.emp.eq","id", "region", 
              "FIPS.st", "FIPS.co", "year")


f.names<-names(table(dat$funct))
f.keeps<-c(4, 6, 7, 11, 14, 15, 16, 17, 19, 22, 24, 25, 27, 30)

### SET UP DATA FOR COUNTY COUNTS - DROP STATE TOTALS FIPS.co=""
dat1<-dat%>%
  filter(funct%in%c(f.names[f.keeps]))%>%
  filter(FIPS.co!="")%>%
  select(.,-c(ft.emp, ft.pay, pt.emp, pt.pay, pt.hrs))%>%
  mutate(FIPS=paste(FIPS.st, FIPS.co, sep=""))

### SPREAD FUNCT INTO FT EQUIVALENT COLUMNS BY CAT
### PROBLEM W/MULTI-COUNTY MUNICIPALITIES, GET SOAKED UP INTO SINGLE COUNTY - 
### NYC AS GOOD EXAMPLE - BECAUSE EACH CTY HAS SAME CITY GOVT RESOURCES - M
### MAY NEED TO CHECK EACH FAILED MATCH FROM NCANDS/CDC TO EMPLOY DATA

dat2<-dat1%>%
  group_by(year, id)%>%
  spread(value=ft.emp.eq, key=funct, fill=0)
 
dat3<-dat2%>%
  group_by(state, county, FIPS.st, FIPS.co, FIPS, year)%>%
  dplyr::summarise(cops=sum(`Police Officers Only`))  


