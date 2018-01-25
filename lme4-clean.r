library(lme4)
library(Amelia)
library(arm)
### Assorted functions for data cleaning and analysis
### Some unused in the current repository


### Merges imputation results according to Rubin for beta, se

makeMIRegTab<-function(x){
  m<-length(x)
  r<-mi.meld(t(sapply(x, fixef)), t(sapply(x, se.fixef)))
  beta<-t(r[[1]])
  se<-t(r[[2]])
  z<-beta/se
  p<-round(2*pnorm(-abs(z)), 5)
  ### TO SUMMARIZE VARIANCE OF REs ACROSS IMPUTATIONS
  ranefs.disp<-matrix(ncol=nrow(dat.in), nrow=m)
  ranefs.disp.se<-matrix(ncol=nrow(dat.in), nrow=m)
  ranefs.st<-ranefs.st.se<-matrix(ncol=length(unique(dat$state)), nrow=m)
  ranefs.fips<-ranefs.fips.se<-matrix(ncol=length(unique(dat$FIPS)), nrow=m)
  for(i in 1:m){
    if(length(ranef(x[[i]]))>1){ 	
      ranefs.disp[i,]<-ranef(x[[i]])[[1]][,1]
      ranefs.disp.se[i,]<-se.ranef(x[[i]])[[1]][,1]
      ranefs.st[i,]<-ranef(x[[i]])[[3]][,1]
      ranefs.st.se[i,]<-as.numeric(se.ranef(x[[i]])[[3]][,1])
      ranefs.fips[i,]<-ranef(x[[i]])[[2]][,1]
      ranefs.fips.se[i,]<-as.numeric(se.ranef(x[[i]])[[2]][,1])
    }
    # if(length(ranef(x[[i]]))==1) {
    #   ranefs.st[i,]<-ranef(x[[i]])[[1]][,1]
    #   ranefs.st.se[i,]<-as.numeric(se.ranef(x[[i]])[[1]][,1])
    # }
    
  }
  re.d<-mi.meld(ranefs.disp, ranefs.disp.se)
  re.s<-mi.meld(ranefs.st, ranefs.st.se)
  re.f<-mi.meld(ranefs.fips, ranefs.fips.se)
  Sig2.ep<-c(sd(t(re.d[[1]])), NA, NA, NA)
  names(Sig2.ep)<-"sd(obs)"
  Sig2.gam<-c(sd(t(re.s[[1]])), NA, NA, NA)
  names(Sig2.gam)<-"sd(st)"
  Sig2.zeta<-c(sd(t(re.f[[1]])), NA, NA, NA)
  names(Sig2.zeta)<-"sd(fips)"
  var<-rbind(Sig2.ep, Sig2.gam, Sig2.zeta)
  row.names(var)<-c("sd(obs)", "sd(st)", "sd(fips)")
  
  results<-as.data.frame(cbind(beta, se, z,p))
  names(results)<-c("Beta", "SE", "z","p")
  colnames(var)<-c("Beta", "SE", "z","p")
  results<-rbind(results, var)
  return(results)
}

load("~/sync/ncands-fc/models-lme4-imp.RData")

models<-ls()[grep("b\\.", ls())]

regout<-list()
for(i in 1:length(models)){
  regout[[i]]<-makeMIRegTab(get(models[i]))
  names(regout)[i]<-models[i]
}

