source("R:/Project/NCANDS/ncands-fc/main.r", verbose = TRUE)

### correlation matrix for arrest data
par(mfrow=c(4, 4))
arrest.dat<-as.data.frame(dat%>%dplyr::select(arrest.all.tot.pc, arrest.viol.tot.pc, arrest.drug.tot.pc, arrest.qol.tot.pc))
for(i in 2:5){
  for(k in 2:5){
    plot(arrest.dat[,i], arrest.dat[,k], xlab=names(arrest.dat[i]), ylab=names(arrest.dat[k]), 
         main=paste("r=", round(cor(arrest.dat[,i], arrest.dat[,k], use="pairwise.complete.obs"),2)))
  }
}

#### results: arrest per capita for all, violent, drug, qol highly correlated [0.4, 0.75]
#### should likely model each separately
dev.off()
plot(dat$infmort, dat$child.pov.pct, 
     main=paste("r=", round(cor(dat$infmort, dat$child.pov.pct, use="pairwise.complete.obs"),2)))

plot(dat$police.pc, dat$arrest.all.tot.pc, 
     main=paste("r=", round(cor(dat$police.pc, dat$arrest.all.tot.pc, use="pairwise.complete.obs"),2)))
