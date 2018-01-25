### read race/chpov ACS data via NHGIS
rm(list=ls())
gc()
library(dplyr)
library(tidyr)
library(purrr)
options(stringsAsFactors = FALSE)

setwd("C:/Box Sync/ncands-csv/nhgis0026_csv")

### retrieve all files in folder that are csvs
files<-list.files()[grep("csv", list.files())]

nhgis<-list()
for(i in 1:length(files)){
  nhgis[[i]]<-read.csv(files[i], colClasses="character")
}

### write loop that takes code / value pairs, loops over data, returns
### pe, se estimates for total chpov by race, county 
### setting up for either overimputation or stan model-based imputation

###DATA FRAME WITH CODE, VALUE for all data - use codebooks to figure out
###NUMBER OF AGE GROUPS CHANGES AT SOME POINT
key<-data.frame("file"=NULL, "code"=NULL, "value"=NULL, "name"=NULL)
for(i in 1:length(files)){
  if(files[i]=="nhgis0026_ds177_20105_2010_county.csv"){
    code<-c("J3T", "J3U", "J3V", "J3Z", "J30")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov", "lat.chpov")
    out<-data.frame("file"=rep(files[i], 
                               times=length(value)*length(code)),
                    "code"=rep(code, 
                               each=length(value)),
                    "value"=rep(value, 
                                times=length(code)),
                    "name"=rep(name, 
                               each=length(value)))
    out<-rbind(out, 
               data.frame(
               "file"=rep(files[i], 9),
               "code"=rep("J33", 9),
               "value"=c("003", "004", "005", "016", "017", "018", "029", "030", "031"),
               "name"=rep("chpov", 9)))
    key<-rbind(key, out)
    
  }
  if(files[i]=="nhgis0026_ds185_20115_2011_county.csv"){
    code<-c("M5D", "M5E", "M5F", "M5J", "M5K")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov", 'lat.chpov')
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("M5N", 9),
                 "value"=c("003", "004", "005", "016", "017", "018", "029", "030", "031"),
                 "name"=rep("chpov", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds192_20125_2012_county.csv"){
    code<-c("RAO", "RAP", "RAQ", "RAU", "RAV")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov", "lat.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("RAY", 9),
                 "value"=c("003", "004", "005", "016", "017", "018", "029", "030", "031"),
                 "name"=rep("chpov", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds196_20095_2009_county.csv"){
    code<-c("R2F", "R2G", "R2H", "R2L", "R2M")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov", "lat.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("R2P", 9),
                 "value"=c("003", "004", "005", "016", "017", "018", "029", "030", "031"),
                 "name"=rep("chpov", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds202_20135_2013_county.csv"){
    code<-c("UW8", "UW9", "UXA", "UXE", "UXF")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov", "lat.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("UXI", 9),
                 "value"=c("003", "004", "005", "016", "017", "018", "029", "030", "031"),
                 "name"=rep("chpov", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds207_20145_2014_county.csv"){
    code<-c("ABTD", "ABTE", "ABTF", "ABTJ", "ABTK")
    value<-c("003", "004", "005")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov", "lat.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("ABTN", 9),
                 "value"=c("003", "004", "005", "016", "017", "018", "029", "030", "031"),
                 "name"=rep("chpov", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds216_20155_2015_county.csv"){
    code<-c("AD2F", "AD2G", "AD2H", "AD2L", "AD2M")
    value<-c("003", "004", "005")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov", "lat.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("AD2P", 9),
                 "value"=c("003", "004", "005", "016", "017", "018", "029", "030", "031"),
                 "name"=rep("chpov", 9)))
    key<-rbind(key, out)
  }
  #clean first row NA
}

### read and calc se - give it the NHGIS code and needed values
nhgis.var<-function(data=NULL, code=NULL, values=NULL){
  pe<-0
  se<-0
  for(z in 1:length(values)){
    #extract point estimate
    pe.index<-which(names(data)==paste(code, "E", values[z], sep=""))
    pe<-pe+as.numeric(data[,pe.index])
    ### following census recommendation, treating estimates as iid Normal, clearly not true, should be scaled by covar(x,y), but far trickier
  }
  return(pe)
}

### Get this working for the key input
nhgis.out<-nhgis
for(h in 1:length(files)){
  nhgis.out[[h]]<-nhgis.out[[h]]%>%
    select(YEAR, STATEA, COUNTYA)
  file.temp<-files[h]
  key.temp<-key%>%
    filter(file==file.temp)
  for(i in unique(key.temp$code)){
    k<-key.temp[key.temp$code==i,]
    nvalue<-length(unique(k$value))
    temp.out<-as.data.frame(nhgis.var(data=nhgis[[h]], 
                                      code=k[1,2], 
                                      values=k[,3]))
    names(temp.out)<-k$name[1]
    nhgis.out[[h]]<-cbind(nhgis.out[[h]], temp.out)
  }
}

nhgis.out<-do.call("rbind", nhgis.out)
nhgis.out$FIPS<-paste(nhgis.out$STATEA, nhgis.out$COUNTYA, sep="")
nhgis.out$year<-as.numeric(substr(nhgis.out$YEAR, 6, 9))
nhgis.out<-nhgis.out%>%
  dplyr::select(-YEAR, -STATEA, -COUNTYA)

names(nhgis.out)<-c("blk", "ai", "aa", "wht", "lat", "tot", "FIPS", "year")

######################################################################
## this is for long file - child poverty
######################################################################
nhgis.pe.long<-gather(nhgis.out, race, chpov, -c(FIPS, year))

################################################################################
### now make full child pop counts - combining with SEER gives weird estimates
################################################################################

key<-data.frame("file"=NULL, "code"=NULL, "value"=NULL, "name"=NULL)
for(i in 1:length(files)){
  if(files[i]=="nhgis0026_ds177_20105_2010_county.csv"){
    code<-c("J3T", "J3U", "J3V", "J3Z", "J30")
    value<-c("003", "004", "005", "006", "011", "012", "013", "014")
    name<-c("blk.child", "ai.child", "aa.child", "wht.child", "lat.child")
    out<-data.frame("file"=rep(files[i], 
                               times=length(value)*length(code)),
                    "code"=rep(code, 
                               each=length(value)),
                    "value"=rep(value, 
                                times=length(code)),
                    "name"=rep(name, 
                               each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("J33", 9),
                 "value"=c("002", "015", "028"),
                 "name"=rep("child", 9)))
    key<-rbind(key, out)
    
  }
  if(files[i]=="nhgis0026_ds185_20115_2011_county.csv"){
    code<-c("M5D", "M5E", "M5F", "M5J", "M5K")
    value<-c("003", "004", "005", "006", "011", "012", "013", "014")
    name<-c("blk.child", "ai.child", "aa.child", "wht.child", 'lat.child')
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("M5N", 9),
                 "value"=c("002", "015", "028"),
                 "name"=rep("child", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds192_20125_2012_county.csv"){
    code<-c("RAO", "RAP", "RAQ", "RAU", "RAV")
    value<-c("003", "004", "005", "006", "011", "012", "013", "014")
    name<-c("blk.child", "ai.child", "aa.child", "wht.child", "lat.child")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("RAY", 9),
                 "value"=c("002", "015", "028"),
                 "name"=rep("child", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds196_20095_2009_county.csv"){
    code<-c("R2F", "R2G", "R2H", "R2L", "R2M")
    value<-c("003", "004", "005", "006", "011", "012", "013", "014")
    name<-c("blk.child", "ai.child", "aa.child", "wht.child", "lat.child")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("R2P", 9),
                 "value"=c("002", "015", "028"),
                 "name"=rep("child", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds202_20135_2013_county.csv"){
    code<-c("UW8", "UW9", "UXA", "UXE", "UXF")
    value<-c("003", "004", "005", "006", "011", "012", "013", "014")
    name<-c("blk.child", "ai.child", "aa.child", "wht.child", "lat.child")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("UXI", 9),
                 "value"=c("002", "015", "028"),
                 "name"=rep("child", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds207_20145_2014_county.csv"){
    code<-c("ABTD", "ABTE", "ABTF", "ABTJ", "ABTK")
    value<-c("003", "004", "005", "011", "012", "013")
    name<-c("blk.child", "ai.child", "aa.child", "wht.child", "lat.child")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("ABTN", 9),
                 "value"=c("002", "015", "028"),
                 "name"=rep("child", 9)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0026_ds216_20155_2015_county.csv"){
    code<-c("AD2F", "AD2G", "AD2H", "AD2L", "AD2M")
    value<-c("003", "004", "005", "011", "012", "013")
    name<-c("blk.child", "ai.child", "aa.child", "wht.child", "lat.child")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    out<-rbind(out, 
               data.frame(
                 "file"=rep(files[i], 9),
                 "code"=rep("AD2P", 9),
                 "value"=c("002", "015", "028"),
                 "name"=rep("child", 9)))
    key<-rbind(key, out)
  }
  #clean first row NA
}

### read and calc se - give it the NHGIS code and needed values
nhgis.var<-function(data=NULL, code=NULL, values=NULL){
  pe<-0
  se<-0
  for(z in 1:length(values)){
    #extract point estimate
    pe.index<-which(names(data)==paste(code, "E", values[z], sep=""))
    pe<-pe+as.numeric(data[,pe.index])
    ### following census recommendation, treating estimates as iid Normal, clearly not true, should be scaled by covar(x,y), but far trickier
  }
  return(pe)
}

### Get this working for the key input
nhgis.out<-nhgis
for(h in 1:length(files)){
  nhgis.out[[h]]<-nhgis.out[[h]]%>%
    select(YEAR, STATEA, COUNTYA)
  file.temp<-files[h]
  key.temp<-key%>%
    filter(file==file.temp)
  for(i in unique(key.temp$code)){
    k<-key.temp[key.temp$code==i,]
    nvalue<-length(unique(k$value))
    temp.out<-as.data.frame(nhgis.var(data=nhgis[[h]], 
                                      code=k[1,2], 
                                      values=k[,3]))
    names(temp.out)<-k$name[1]
    nhgis.out[[h]]<-cbind(nhgis.out[[h]], temp.out)
  }
}

nhgis.out<-do.call("rbind", nhgis.out)
nhgis.out$FIPS<-paste(nhgis.out$STATEA, nhgis.out$COUNTYA, sep="")
nhgis.out$year<-as.numeric(substr(nhgis.out$YEAR, 6, 9))
nhgis.out<-nhgis.out%>%
  dplyr::select(-YEAR, -STATEA, -COUNTYA)
names(nhgis.out)<-c("blk", "ai", "aa", "wht", "lat", "tot", "FIPS", "year")

nhgis.child.long<-gather(nhgis.out, race, child, -c(FIPS, year))

################################
# join poverty and child pop estimates
###############################

merge<-full_join(nhgis.child.long, nhgis.pe.long)

setwd("C:/Users/fre9/Box Sync/ncands-csv/")
write.csv(merge, "nhgis-chpov.csv", row.names=FALSE)