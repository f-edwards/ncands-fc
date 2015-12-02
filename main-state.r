rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(nlme)
library(lme4)
library(texreg)
library(ggplot2)
set.seed(1)

setwd("H:/")

### PULLED COL WIDTH, COL NAMES FROM VariableLayout.xlsx, transposed
### READ AND CLEAN DATA
mal<-read.csv("H:/data/state-malt.csv")
names(mal)[2]<-"stname"
mal<-mal[,-1]

state<-read.csv("H:/ncands-fc/statedat.csv")
state<-state[,-1]
state<-state[,-(3:58)]
drops<-c("edu.tot.pay", "edu.ft.emp", "hosp.tot.pay", "hosp.ft.emp", "drop", "drop.1",
         "blkchild", "whtchild", "blkadult", "whtadult", "adult")
state<-state[, !(names(state)%in%drops)]

### STATE LEVEL MEASURES
state$police.pc<-state$police.ft.emp/state$pop
state$welfare.pc<-state$welfare.ft.emp/state$pop
state$emp.pc<-state$tot.ft.emp/state$pop
state$pctblk<-state$blkpop/state$pop
state$chpovrt<-state$childpov/state$child
state$unemprt<-state$unemp/(state$unemp+state$emp)
state$childnot2par<-1-(state$kids2par/state$child)
state$gsppercap<-state$GSP*1000000/state$pop

s.dat<-left_join(state,mal,  by=c("stname", "year"))

s.dat<-s.dat%>%
  filter(year<2012)%>%
  filter(year>2001)%>%
  filter(stname!="XX")%>%
  filter(stname!="PR")%>%
  filter(stname!="DC")

s.dat$rpts.pc<-s.dat$tot.rpt/s.dat$child
s.dat$year.c<-s.dat$year-2002

rpts.plot<-ggplot(data=s.dat,
       aes(x=year, y=rpts.pc))+
  geom_point()+
  facet_wrap(~stname)+
  theme_bw()+
  ggtitle("Total maltreatment reports per capita")+
  scale_x_continuous(breaks=c(2004,2007,2010))
ggsave("H:/fc-surv-pres/rpts-plot.pdf", rpts.plot)

source.plot<-ggplot(data=s.dat,
       aes(x=year))+
  geom_point(aes(y=(rpt.cj/child)), colour="red", size=2, alpha=0.9)+
  geom_point(aes(y=(rpt.socserv/child)), colour="grey1", size=2, alpha=0.9)+
    facet_wrap(~stname)+
  theme_bw()+
  ggtitle("Reporting rates per capita, police in red, social services in grey")+
  scale_x_continuous(breaks=c(2004,2007,2010))
ggsave("H:/fc-surv-pres/source-plot.pdf", source.plot)

emp.plot<-ggplot(data=s.dat,
                 aes(x=year))+
  geom_point(aes(y=(police.pc)), colour="red", size=2, alpha=0.9)+
  geom_point(aes(y=(welfare.pc)), colour="grey1", size=2, alpha=0.9)+
  facet_wrap(~stname)+
  theme_bw()+
  ggtitle("Staffing rates per capita, police in red, social services in grey")+
  scale_x_continuous(breaks=c(2004,2007,2010))
ggsave("H:/fc-surv-pres/emp-plot.pdf", emp.plot)


### NEED TO IMPUTE MISSING
pdf("H:/fc-surv-pres/missmap.pdf")
missmap(s.dat)
dev.off()

m<-round((sum(is.na(s.dat$rpt.foster))/nrow(s.dat))*100)
### Constrain to [0,Inf]
bounds<-cbind(1:ncol(s.dat), rep(0, ncol(s.dat)), rep(Inf, ncol(s.dat)))
### missing data on education, hospital workers from 2007 onward, will get later, don't want to impute now
rpt.imp<-amelia(s.dat, ts="year", cs="state", 
                idvars=c("stname", "statename", "year.c"),
                m=m, polytime=1, empri=0.01*nrow(s.dat), bounds=bounds)
plot(rpt.imp)
par(mfrow=c(1,1))
### UNCONDITIONAL GROWTH
m0.pol<-lme(fixed=scale(I(rpt.cj/child))~year.c,
        random=~year.c|state,
	data=s.dat, na.action="na.omit")

m0.socserv<-lme(fixed=scale(I(rpt.socserv/child))~year.c,
                random=~year.c|state,
                data=s.dat, na.action="na.omit")
### POLICE REPORTS - RATES OR COUNTS? 
m.pol<-lme(fixed=scale(I(rpt.cj/child))~scale(police.pc)+scale(I(crime/pop))+scale(I(incartot/pop))+
             scale(pctblk)+
             scale(chpovrt)+scale(unemprt)+
             scale(childnot2par)+year.c,
            random=~year.c|state,
            data=s.dat, na.action="na.omit")

m.socserv<-lme(fixed=scale(I(rpt.socserv/child))~scale(welfare.pc)+scale(I(AFDCRec/childpov))+
                 scale(I(WIC.par/childpov))+
                 scale(pctblk)+
                 scale(chpovrt)+scale(unemprt)+
                 scale(childnot2par)+year.c,
               random=~year.c|state,
               data=s.dat, na.action="na.omit")

### Create null matrices for FE, SE output
m0p.g <- matrix(0, nrow=m, ncol=length(fixef(m0.pol)))
m0p.se <- matrix(0, nrow=m, ncol=length(fixef(m0.pol)))
colnames(m0p.g) <- colnames(m0p.se) <- names(fixef(m0.pol))

m0s.g <- matrix(0, nrow=m, ncol=length(fixef(m0.socserv)))
m0s.se <- matrix(0, nrow=m, ncol=length(fixef(m0.socserv)))
colnames(m0p.g) <- colnames(m0p.se) <- names(fixef(m0.socserv))

m.pol.g <- matrix(0, nrow=m, ncol=length(fixef(m.pol)))
m.pol.se <- matrix(0, nrow=m, ncol=length(fixef(m.pol)))
colnames(m.pol.g) <- colnames(m.pol.se) <- names(fixef(m.pol))

m.socserv.g <- matrix(0, nrow=m, ncol=length(fixef(m.socserv)))
m.socserv.se <- matrix(0, nrow=m, ncol=length(fixef(m.socserv)))
colnames(m.socserv.g) <- colnames(m.socserv.se) <- names(fixef(m.socserv))


for(i in (1:m)){
  dat<-rpt.imp$imputations[[i]]
  
  m0p.i<-lme(fixed=scale(I(rpt.cj/child))~year.c,
              random=~year.c|state,
              data=dat, control=lmeControl(maxIter=500,
                                           msMaxIter=500,
                                           msMaxEval=500,
                                           sing.tol=1e-20))
  
  m0s.i<-lme(fixed=scale(I(rpt.socserv/child))~year.c,
                  random=~year.c|state,
                  data=dat, control=lmeControl(maxIter=500,
                                               msMaxIter=500,
                                               msMaxEval=500,
                                               sing.tol=1e-20))
  
  m0p.g[i,]<-fixef(m0p.i)
  m0p.se[i,]<-summary(m0p.i)$tTable[,"Std.Error"]
  m0s.g[i,]<-fixef(m0s.i)
  m0s.se[i,]<-summary(m0s.i)$tTable[,"Std.Error"]
 
  m.pol.i<-lme(fixed=scale(I(rpt.cj/child))~scale(police.pc)+scale(I(crime/pop))+scale(I(incartot/pop))+
               scale(pctblk)+
               scale(chpovrt)+scale(unemprt)+
               scale(childnot2par)+year.c,
             random=~year.c|state,
             data=dat, control=lmeControl(maxIter=500,
                                            msMaxIter=500,
                                            msMaxEval=500,
                                            sing.tol=1e-20))
  
  m.pol.g[i,]<-fixef(m.pol.i)
  m.pol.se[i,]<-summary(m.pol.i)$tTable[,"Std.Error"]
  
  m.socserv.i<-lme(fixed=scale(I(rpt.socserv/child))~scale(welfare.pc)+scale(I(AFDCRec/childpov))+
                   scale(I(WIC.par/childpov))+
                   scale(pctblk)+
                   scale(chpovrt)+scale(unemprt)+
                   scale(childnot2par)+year.c,
                 random=~year.c|state,
                 data=dat, control=lmeControl(maxIter=500,
                                                msMaxIter=500,
                                                msMaxEval=500,
                                                sing.tol=1e-20))
  
  m.socserv.g[i,]<-fixef(m.socserv.i)
  m.socserv.se[i,]<-summary(m.socserv.i)$tTable[,"Std.Error"]
  
}

#### Combine results
m0p.t<-mi.meld(m0p.g, m0p.se)
m0s.t<-mi.meld(m0s.g,m0s.se)
m.pol.t<-mi.meld(m.pol.g, m.pol.se)
m.socserv.t<-mi.meld(m.socserv.g, m.socserv.se)

#### LATEX OUTPUT
stargazer(list(m0p.i, m0s.i),
          coef=list(t(m0p.t[[1]]), t(m0s.t[[1]])),
          se=list(t(m0p.t[[2]]), t(m0s.t[[2]])),
          out="H:/fc-surv-pres/uncond-models.tex",
          keep.stat=c("n", "bic"),
          style="asr",
          title="Unconditional Growth of Police Maltreatment Reports per capita (1) and Social Services reports per capita (2)"
)

stargazer(list(m.pol.i),
          coef=list(t(m.pol.t[[1]])),
          se=list(t(m.pol.t[[2]])),
          out="H:/fc-surv-pres/pol-models.tex",
          keep.stat=c("n", "bic"),
          style="asr",
          title="Predictors of Police Maltreatment Reports per capita"
)

stargazer(list(m.socserv.i),
          coef=list(t(m.socserv.t[[1]])),
          se=list(t(m.socserv.t[[2]])),
          out="H:/fc-surv-pres/socserv-models.tex",
          keep.stat=c("n", "bic"),
          style="asr",
          title="Predictors of Social Services reports per capita"
)

plotreg(list(m.pol, m.pol.i, m.socserv, m.socserv.i),
        override.coef=list(as.vector(fixef(m.pol)),
          as.vector(m.pol.t[[1]])), 
        override.se=list(as.vector(summary(m.pol)$tTable[,"Std.Error"]), as.vector(m.pol.t[[2]])), 
        custom.model.names=c("Police Reporting w/o MI", "Police Reporting w/ MI", 
                             "Social Services w/o MI", "Social Services w/ MI"))
