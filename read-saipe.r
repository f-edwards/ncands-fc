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
set.seed(1)

setwd("R:/Project/NCANDS/ncands-csv/Census-SAIPE")



readSAIPE<-function(x){
  starts<-c(1, 4, 8, 17, 26, 35, 40,
            45, 50, 59, 68, 77, 82,
            87, 92, 101, 110, 119,
            124, 129, 134, 141, 148, 194,
            240, 243)
  ends<-c(2,6,15,24,33,38,43,48,57,
          66,75,80,85,90,99,108,117,
          122,127,132,139,146,153,
          238,241,264)
  classes<-paste("c","c","d","d","d","d","d","d","d","d","d",
                 "d","d","d","d","d","d","d","d","d","d","d","d",
                 "c", "c", "c", sep="")
  names<-c("fips.st", "fips.cnty", "pov", "pov.lower", "pov.upper",
           "pov.pct", "pov.pct.lower", "pov.pct.upper",
           "child.pov", "child.pov.lower", "child.pov.upper",
           "child.pov.pct", "child.pov.pct.lower", "child.pov.pct.upper",
           "relate.chpov", "relate.chpov.lower", "relate.chpov.upper",
           "relate.chpov.pct", "relate.chpov.pct.lower", "relate.chpov.pct.upper",
           "median.hh.income", "median.hh.income.lower", "median.hh.income.upper",
           "stname", "state", "tag")
  return(read_fwf(x, fwf_positions(starts, ends, col_names=names),
                  col_types=classes, na="."))
}

files<-c("est15ALL.txt", "est14ALL.txt",
         "est13ALL.txt", "est12ALL.txt",
         "est11ALL.txt",
         "est10ALL.txt", "est09ALL.txt",
         "est08ALL.txt", "est07ALL.txt",
         "est06ALL.txt", "est05ALL.txt", 
         "est04ALL.txt", "est03ALL.dat",
         "est02ALL.dat", "est01ALL.dat",
         "est00ALL.dat")
out<-NULL
for(i in 1:length(files)){
  dat<-readSAIPE(files[i])
  out<-bind_rows(out, dat)
}

out$year<-as.numeric(paste("20", substr(out$tag, 4,5), sep=""))

write.csv(out, file="R:/Project/NCANDS/ncands-csv/saipe.csv", row.names=FALSE)