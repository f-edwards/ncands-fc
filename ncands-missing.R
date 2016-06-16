setwd("H:/data")
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)

dat<-fread("Child2012.csv")

### mark all NA as 9999 for histograms
dat[is.na(dat$RptSrc), "RptSrc"]<-9999
rpt.miss<-table(dat$RptSrc, dat$StaTerr)

write.csv(rpt.miss, "rpt-desc-2012.csv", row.names=TRUE)

dat[is.na(dat$PostServ), "PostServ"]<-9999
srv.miss<-table(dat$PostServ, dat$StaTerr)

dat[is.na(dat$FCPublic), "FCPublic"]<-9999
pubsrv.miss<-table(dat$FCPublic, dat$StaTerr)

### TONS OF MISSING ON SERVICE AND RISK VARS
dat$R¶ptSrc<-as.factor(dat$RptSrc)
ggplot(dat, aes(RptSrc))+geom_bar()

plots<-list()
for(i in unique(dat$StaTerr)){
  z<-dat[dat$StaTerr==i,]
  plots[[i]]<-ggplot(z, aes(RptSrc))+geom_bar()+ggtitle(i)
}
do.call(grid.arrange,plots)