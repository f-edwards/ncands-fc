library(Amelia)

source("read-main.r")
dat.imp<-amelia(dat, ts="year", cs="FIPS", 
                idvars=c("county", "state"), polytime=3)

