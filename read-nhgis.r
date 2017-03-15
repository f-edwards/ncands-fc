### read race/chpov ACS data via NHGIS

setwd("R:/Project/NCANDS/ncands-csv/nhgis0021_csv/")

### retrieve all files in folder that are csvs
files<-list.files()[grep("csv", list.files())]

nhgis<-list()
for(i in 1:length(files)){
  nhgis[[i]]<-read.csv(files[i], colClasses=c(rep("character", 8), NA))
  #Discard error data
  # index<-which(names(nhgis[[i]])=="NAME_M")
  # nhgis[[i]]<-nhgis[[i]][,-(index:ncol(nhgis[[i]]))]
}



### write loop that takes code / value pairs, loops over data, returns
### pe, se estimates for total chpov by race, county 
### setting up for either overimputation or stan model-based imputation

###DATA FRAME WITH CODE, VALUE for all data - use codebooks to figure out
###NUMBER OF AGE GROUPS CHANGES AT SOME POINT
key<-data.frame("file"=NULL, "code"=NULL, "value"=NULL, "name"=NULL)
for(i in 1:length(files)){
  if(files[i]=="nhgis0021_ds177_20105_2010_county.csv"){
    code<-c("J3T", "J3U", "J3V", "J3Z")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
      "code"=rep(code, each=length(value)),
      "value"=rep(value, times=length(code)),
      "name"=rep(name, each=length(value)))
    key<-rbind(key, out)
    
  }
  if(files[i]=="nhgis0021_ds185_20115_2011_county.csv"){
    code<-c("M5D", "M5E", "M5F", "M5J")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0021_ds192_20125_2012_county.csv"){
    code<-c("RAO", "RAP", "RAQ", "RAU")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0021_ds196_20095_2009_county.csv"){
    code<-c("R2F", "R2G", "R2H", "R2L")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0021_ds202_20135_2013_county.csv"){
    code<-c("UW8", "UW9", "UXA", "UXE")
    value<-c("003", "004", "005", "006")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0021_ds207_20145_2014_county.csv"){
    code<-c("ABTD", "ABTE", "ABTF", "ABTJ")
    value<-c("003", "004", "005")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    key<-rbind(key, out)
  }
  if(files[i]=="nhgis0021_ds216_20155_2015_county.csv"){
    code<-c("AD2F", "AD2G", "AD2H", "AD2L")
    value<-c("003", "004", "005")
    name<-c("blk.chpov", "ai.chpov", "aa.chpov", "wht.chpov")
    out<-data.frame("file"=rep(files[i], times=length(value)*length(code)),
                    "code"=rep(code, each=length(value)),
                    "value"=rep(value, times=length(code)),
                    "name"=rep(name, each=length(value)))
    key<-rbind(key, out)
  }
  #clean first row NA
}

### read and calc se - give it the NHGIS code and needed values
nhgis.var<-function(data=NULL, code=NULL, values=NULL){
  pe<-0
  se<-0
  for(i in 1:length(values)){
    #extract point estimate
    pe.index<-which(names(data)==paste(code, "E", values[i], sep=""))
    moe.index<-which(names(data)==paste(code, "M", values[i], sep=""))
    pe<-pe+as.numeric(data[,pe.index])
    ### following census recommendation, treating estimates as iid Normal, clearly not true, should be scaled by covar(x,y), but far trickier
    se<-se+((as.numeric(data[,moe.index])/1.645)^2)
  }
  se<-sqrt(se)
  out<-cbind(pe, se)
  return(out)
}

### Get this working for the key input

for(h in 1:length(files)){
  index<-h
  dat<-key%>%filter(file==files[index])
  ncode<-length(unique(dat$code))
  nvalue<-length(unique(dat$value))
  for(j in 1:ncode){
    temp<-dat%>%filter(code==unique(dat$code)[j])
    temp.out<-as.data.frame(nhgis.var(data=nhgis[[index]], code=temp[1,2], values=temp[,3]))
    names(temp.out)<-paste(temp$name, names(temp.out), sep="_")[1:2]
    nhgis[[index]]<-cbind(nhgis[[index]], temp.out)
  }
  nhgis[[index]]<-nhgis[[index]]%>%dplyr::select(YEAR, STATEA, COUNTYA, blk.chpov_pe, blk.chpov_se,
                                                 ai.chpov_pe, ai.chpov_se, aa.chpov_pe, aa.chpov_se,
                                                 wht.chpov_pe, wht.chpov_se)
}
#### START HERE TOMORROW

nhgis.out<-Reduce(rbind, nhgis)
nhgis.out$FIPS<-paste(nhgis.out$STATEA, nhgis.out$COUNTYA, sep="")
nhgis.out$year<-as.numeric(substr(nhgis.out$YEAR, 1, 4))
nhgis.out<-nhgis.out%>%dplyr::select(-YEAR, -STATEA, -COUNTYA)


###DIAGNOSTICS ON ratio of PE/SE
plot(jitter(log(nhgis.out$ai.chpov_pe)), jitter(nhgis.out$ai.chpov_pe/nhgis.out$ai.chpov_se), pch=".")
### Few with PE/SE<2
nhgis.pe<-nhgis.out%>%select(FIPS, year, blk.chpov_pe, ai.chpov_pe, aa.chpov_pe, wht.chpov_pe)
nhgis.pe.long<-gather(nhgis.pe, race, chpov_pe, -c(FIPS, year))
nhgis.pe.long$race<-ifelse(nhgis.pe.long$race=="blk.chpov_pe", "blk",
                        ifelse(nhgis.pe.long$race=="ai.chpov_pe", "ai",
                               ifelse(nhgis.pe.long$race=="aa.chpov_pe", "aa",
                                      ifelse(nhgis.pe.long$race=="wht.chpov_pe", "wht",
                                             "other"))))
nhgis.se<-nhgis.out%>%select(FIPS, year, blk.chpov_se, ai.chpov_se, aa.chpov_se, wht.chpov_se)
nhgis.se.long<-gather(nhgis.se, race, chpov_se, -c(FIPS, year))
nhgis.se.long$race<-ifelse(nhgis.se.long$race=="blk.chpov_se", "blk",
                           ifelse(nhgis.se.long$race=="ai.chpov_se", "ai",
                                  ifelse(nhgis.se.long$race=="aa.chpov_se", "aa",
                                         ifelse(nhgis.se.long$race=="wht.chpov_se", "wht",
                                                "other"))))

nhgis.long<-full_join(nhgis.pe.long, nhgis.se.long)

setwd("R:/Project/NCANDS/ncands-csv/")
write.csv(nhgis.long, "nhgis-chpov.csv", row.names=FALSE)