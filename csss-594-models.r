rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(nlme)
library(lme4)
library(ggplot2)
library(Amelia)
library(arm)
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
s.dat$rpts.mand.pct<-(s.dat$tot.rpt-s.dat$rpt.inf)/s.dat$tot.rpt
s.dat$year.c<-s.dat$year-2002

rpts.plot<-ggplot(data=s.dat,
       aes(x=year, y=rpts.pc))+
  geom_point()+
  facet_wrap(~stname, ncol=5)+
  theme_bw()+
  scale_x_continuous(breaks=c(2004,2010))+
  xlab("Year")+
  ylab("Reports per capita")
ggsave("H:/surveillance-analysis/paper-draft/rpts-plot.pdf", rpts.plot)

rpts.mand.plot<-ggplot(data=s.dat,
                     aes(x=year, y=rpts.mand.pct))+
  geom_point()+
  facet_wrap(~stname, ncol=5)+
  theme_bw()+
  scale_x_continuous(breaks=c(2004,2010))+
  xlab("Year")+
  ylab("Reports per capita")


emp.plot<-ggplot(data=s.dat,
                 aes(x=year))+
  geom_point(aes(y=(police.pc)), colour="red", size=2, alpha=0.7)+
  geom_point(aes(y=(welfare.pc)), colour="grey1", size=2, alpha=0.7)+
  facet_wrap(~stname, ncol=5)+
  theme_bw()+
  scale_x_continuous(breaks=c(2004,2010))+
  scale_y_continuous(breaks=c(0.001, 0.003))+
  ylab("Year")+
  xlab("Staffing per cap")
ggsave("H:/surveillance-analysis/paper-draft/emp-plot.pdf", emp.plot)

s.dat[s.dat$stname=="PA", c("tot.rpt", "rpts.pc")]<-NA

m<-round((sum(is.na(s.dat$tot.rpt))/nrow(s.dat))*100)
### Constrain to [0,Inf]
bounds<-cbind(1:ncol(s.dat), rep(0, ncol(s.dat)), rep(Inf, ncol(s.dat)))
### missing data on education, hospital workers from 2007 onward, will get later, don't want to impute now
rpt.imp<-amelia(s.dat, ts="year", cs="state", 
                idvars=c("stname", "statename", "year.c"),
                m=m, polytime=1, empri=0.01*nrow(s.dat), bounds=bounds)

# plot(rpt.imp)
# par(mfrow=c(1,1))
### UNCONDITIONAL GROWTH
### RE-WRITING FOR TOTAL REPORTS 
### AND FOR TOTAL VICTIMS
### PERHAPS LATER DO NO VICTIM REPORTS AS A SURPLUS EFFECT?
m0<-lme(fixed=scale(I(tot.rpt/child))~year.c,
        random=~year.c|state,
	data=s.dat, na.action="na.omit")

### POLICE REPORTS - RATES OR COUNTS? 
m1<-lme(fixed=scale(I(tot.rpt/child))~year.c+
          scale(police.pc)+
          scale(welfare.pc),
            random=~year.c|state,
            data=s.dat, na.action="na.omit")


m2<-lme(fixed=scale(I(tot.rpt/child))~year.c+
          scale(police.pc)+
            scale(welfare.pc)+
            scale(chpovrt)+
            scale(childnot2par),
          random=~year.c|state,
          data=s.dat, na.action="na.omit")

m3<-lme(fixed=scale(I(tot.rpt/child))~year.c+
          scale(police.pc)+
          scale(welfare.pc)+
          scale(chpovrt)+
          scale(childnot2par)+
          scale(I(AFDCRec/childpov))+
          scale(I(WIC.par/childpov))+
          scale(I(crime/pop))+
          scale(I(incartot/pop))+
          scale(pctblk),
        random=~year.c|state,
        data=s.dat, na.action="na.omit")

### Create null matrices for FE, SE output
m0.g <- matrix(0, nrow=m, ncol=length(fixef(m0)))
m0.se <- matrix(0, nrow=m, ncol=length(fixef(m0)))
colnames(m0.g) <- colnames(m0.se) <- names(fixef(m0))


m1.g <- matrix(0, nrow=m, ncol=length(fixef(m1)))
m1.se <- matrix(0, nrow=m, ncol=length(fixef(m1)))
colnames(m1.g) <- colnames(m1.se) <- names(fixef(m1))

m2.g <- matrix(0, nrow=m, ncol=length(fixef(m2)))
m2.se <- matrix(0, nrow=m, ncol=length(fixef(m2)))
colnames(m2.g) <- colnames(m2.se) <- names(fixef(m2))

m3.g <- matrix(0, nrow=m, ncol=length(fixef(m3)))
m3.se <- matrix(0, nrow=m, ncol=length(fixef(m3)))
colnames(m3.g) <- colnames(m3.se) <- names(fixef(m3))



for(i in (1:m)){
  dat<-rpt.imp$imputations[[i]]
  
  m0.i<-lme(fixed=scale(I(tot.rpt/child))~year.c,
              random=~year.c|state,
              data=dat, control=lmeControl(maxIter=5000,
                                           msMaxIter=5000,
                                           msMaxEval=5000,
                                           sing.tol=1e-20))

  
  m0.g[i,]<-fixef(m0.i)
  m0.se[i,]<-summary(m0.i)$tTable[,"Std.Error"]

 
  m1.i<-lme(fixed=scale(I(tot.rpt/child))~year.c+
              scale(police.pc)+
              scale(welfare.pc),
            random=~year.c|state,
            data=dat, control=lmeControl(maxIter=5000,
                                            msMaxIter=5000,
                                            msMaxEval=5000,
                                            sing.tol=1e-20))
  
  m1.g[i,]<-fixef(m1.i)
  m1.se[i,]<-summary(m1.i)$tTable[,"Std.Error"]
  
  m2.i<-lme(fixed=scale(I(tot.rpt/child))~year.c+
              scale(police.pc)+
              scale(welfare.pc)+
              scale(chpovrt)+
              scale(childnot2par),
            random=~year.c|state,
            data=dat,  control=lmeControl(maxIter=5000,
                                          msMaxIter=5000,
                                          msMaxEval=5000,
                                          sing.tol=1e-20))
  
  m2.g[i,]<-fixef(m2.i)
  m2.se[i,]<-summary(m2.i)$tTable[,"Std.Error"]
  
  m3.i<-lme(fixed=scale(I(tot.rpt/child))~year.c+
              scale(police.pc)+
              scale(welfare.pc)+
              scale(chpovrt)+
              scale(childnot2par)+
              scale(I(AFDCRec/childpov))+
              scale(I(WIC.par/childpov))+
              scale(I(crime/pop))+
              scale(I(incartot/pop))+
              scale(pctblk),
            random=~year.c|state,
            data=dat, control=lmeControl(maxIter=5000,
                                         msMaxIter=5000,
                                         msMaxEval=5000,
                                         sing.tol=1e-20))
  
  m3.g[i,]<-fixef(m3.i)
  m3.se[i,]<-summary(m3.i)$tTable[,"Std.Error"]
  
}

#### Combine results
m0.t<-mi.meld(m0.g, m0.se)
m1.t<-mi.meld(m1.g,m1.se)
m2.t<-mi.meld(m2.g, m2.se)
m3.t<-mi.meld(m3.g, m3.se)

#### LATEX OUTPUT
library(stargazer)
stargazer(list(m0.i, m1.i, m2.i, m3.i),
          coef=list(t(m0.t[[1]]), t(m1.t[[1]]), t(m2.t[[1]]), t(m3.t[[1]])),
          se=list(t(m0.t[[2]]), t(m1.t[[2]]), t(m2.t[[2]]), t(m3.t[[2]])),
          out="H:/surveillance-analysis/paper-draft/models.tex",
          keep.stat=c("n", "bic"),
          dep.var.labels.include=FALSE,
          dep.var.caption="",
          covariate.labels=c("Year", "Police per cap", "Soc serv per cap", "Child pov", 
                             "Single parent", "TANF enrollment", "WIC enrollment", "Crime", 
                             "Incarceration", "Pct Black pop", "Intercept"),
          title="Predictors of child maltreatment reports per capita, multilevel models with state-level random intercepts and slopes. Standard errors in parentheses",
          star.cutoffs = c(0.05, 0.01, 0.001),
          label="models"
)

### New forest plots using coefplot from arm
pdf("H:/surveillance-analysis/paper-draft/forest.pdf", width=7, height=7)
par(mar=c(0,8,0,0))
coefplot(as.vector(m3.t[[1]]), as.vector(m3.t[[2]]), col.pts="black", 
         varnames=c("Intercept", "Year", "Police per cap", "Soc serv per cap", "Child pov", 
                    "Single parent", "TANF enrollment", "WIC enrollment", "Crime", "Incarceration", "Pct Black pop"),
         main="Parameter Estimate with 95 percent CI")
coefplot(as.vector(m2.t[[1]]), as.vector(m2.t[[2]]), col.pts="blue", add=TRUE, offset=0.15)
coefplot(as.vector(m1.t[[1]]), as.vector(m1.t[[2]]), col.pts="red", add=TRUE, offset=0.3)
coefplot(as.vector(m0.t[[1]]), as.vector(m0.t[[2]]), add=TRUE, offset=0.45, col.pts="darkgreen")
dev.off()

# plotreg(list(m.pol, m.pol.i, m.socserv, m.socserv.i), file="H:/fc-surv-pres/model-plots.pdf",
#         override.coef=list(as.vector(fixef(m.pol)),
#           as.vector(m.pol.t[[1]]), as.vector(fixef(m.socserv)), as.vector(m.socserv.t[[1]])), 
#         override.se=list(as.vector(summary(m.pol)$tTable[,"Std.Error"]), as.vector(m.pol.t[[2]]),
#                          as.vector(summary(m.socserv)$tTable[,"Std.Error"]),  as.vector(m.socserv.t[[2]])), 
#         custom.model.names=c("Police Reporting w/o MI", "Police Reporting w/ MI", 
#                              "Social Services w/o MI", "Social Services w/ MI"))