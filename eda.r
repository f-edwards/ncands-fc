source("R:/Project/NCANDS/ncands-fc/read-main.r", verbose = TRUE)
setwd("R:/Project/NCANDS/ncands-figures/")
### correlation matrix for arrest data
pdf("arrest-cor-matrix.pdf")
par(mfrow=c(4, 4))
arrest.dat<-as.data.frame(dat%>%dplyr::select(arrest.all.tot.pc, arrest.viol.tot.pc, arrest.drug.tot.pc, arrest.qol.tot.pc))
for(i in 2:5){
  for(k in 2:5){
    plot(arrest.dat[,i], arrest.dat[,k], xlab=names(arrest.dat[i]), ylab=names(arrest.dat[k]), 
         main=paste("r=", round(cor(arrest.dat[,i], arrest.dat[,k], use="pairwise.complete.obs"),2)))
  }
}
dev.off()

dat$pol.rpts.pc<-dat$pol.rpts/dat$child.pop
pdf("arrest-all-cor-matrix.pdf")
par(mfrow=c(2, 2))
pol.dat<-as.data.frame(dat%>%dplyr::select(pol.rpts.pc, arrest.all.tot.pc, arrest.viol.tot.pc, arrest.drug.tot.pc, arrest.qol.tot.pc))
for(i in 3:6){
  plot(pol.dat[,i], pol.dat[,2], xlab=names(pol.dat[i]), ylab=names(pol.dat[2]), 
       main=paste("r=", round(cor(pol.dat[,i], pol.dat[,2], use="pairwise.complete.obs"),2)),
       ylim=c(0,0.25))
  abline(lm(pol.dat[,i]~pol.dat[,2]))
}
dev.off()

pdf("arrest-blk-cor-matrix.pdf")
par(mfrow=c(2, 2))
pol.dat<-as.data.frame(dat%>%dplyr::select(pol.blk.pc, arrest.all.blk.pc, arrest.viol.blk.pc, arrest.drug.blk.pc, arrest.qol.blk.pc))
for(i in 3:6){
  plot(pol.dat[,i], pol.dat[,2], xlab=names(pol.dat[i]), ylab=names(pol.dat[2]), 
       main=paste("r=", round(cor(pol.dat[,i], pol.dat[,2], use="pairwise.complete.obs"),2)),
       ylim=c(0,0.25))
  abline(lm(pol.dat[,i]~pol.dat[,2]))
}
dev.off()

pdf("arrest-ai-cor-matrix.pdf")
par(mfrow=c(2, 2))
pol.dat<-as.data.frame(dat%>%dplyr::select(pol.ai.pc, arrest.all.ai.pc, arrest.viol.ai.pc, arrest.drug.ai.pc, arrest.qol.ai.pc))
pol.dat<-pol.dat%>%filter(pol.ai.pc!=Inf)
for(i in 3:6){
  plot(pol.dat[,i], pol.dat[,2], xlab=names(pol.dat[i]), ylab=names(pol.dat[2]), 
       main=paste("r=", round(cor(pol.dat[,i], pol.dat[,2], use="pairwise.complete.obs"),2)),
       ylim=c(0,0.25))
  abline(lm(pol.dat[,i]~pol.dat[,2]))
}
dev.off()

#### results: arrest per capita for all, violent, drug, qol highly correlated [0.4, 0.75]
#### should likely model each separately
dev.off()
plot(dat$infmort, dat$child.pov.pct, 
     main=paste("r=", round(cor(dat$infmort, dat$child.pov.pct, use="pairwise.complete.obs"),2)))

plot(dat$police.pc, dat$arrest.all.tot.pc, 
     main=paste("r=", round(cor(dat$police.pc, dat$arrest.all.tot.pc, use="pairwise.complete.obs"),2)))


#### TS PLOTS ON OUTCOMES
dat$fullname<-paste(dat$county, dat$state, dat$tot.pop)
library(ggplot2)


### EDA ON UCR FOR COUNTY TYPES
eda<-dat
eda$name<-paste(eda$county, eda$state)
eda<-eda%>%filter((year>2003)&(year<2012))
small.pop<-eda%>%filter(race=="all")%>%filter(tot.pop<100000)
big.pop<-eda%>%filter(race=="all")%>%filter(tot.pop>100000)

big.RptPCTS<-ggplot(big.pop, aes(x=year, y=cases/child.pop))+geom_point()
FIPS.samp<-sample(big.pop$FIPS, 50)
big.Rpt.cnty<-ggplot(big.pop%>%filter(FIPS%in%FIPS.samp), aes(x=year, y=cases/child.pop))+
  geom_line()+facet_wrap(~name)
big.Arrest.cnty<-ggplot(big.pop%>%filter(FIPS%in%FIPS.samp), aes(x=year, y=arrest/tot.pop, col=offense))+
  geom_line()+facet_wrap(~name)


small.RptPCTS<-ggplot(small.pop, aes(x=year, y=cases/child.pop))+geom_point()
FIPS.samp<-sample(small.pop$FIPS, 50)
small.Rpt.cnty<-ggplot(small.pop%>%filter(FIPS%in%FIPS.samp), aes(x=year, y=cases/child.pop))+
  geom_line()+facet_wrap(~name)
small.Arrest.cnty<-ggplot(small.pop%>%filter(FIPS%in%FIPS.samp), aes(x=year, y=arrest/tot.pop, col=offense))+
  geom_line()+facet_wrap(~name)


### NEED TO LOOP OVER INTO SMALLER FILES
# all.out<-ggplot(dat, aes(x=year, y=pol.rpts.pc))+facet_wrap(~fullname)
# ggsave(all.out, filename = "pol-rpts-allcountes.pdf", height=49, width=49)
# blk.out<-ggplot(dat, aes(x=year, y=pol.blk.pc))+facet_wrap(~fullname)
# ggsave(all.out, filename = "pol-blk-rpts-allcountes.pdf", height=49, width=49)
# ai.out<-ggplot(dat, aes(x=year, y=pol.ai.pc))+facet_wrap(~fullname)
# ggsave(all.out, filename = "pol-ai-rpts-allcountes.pdf", height=49, width=49)
