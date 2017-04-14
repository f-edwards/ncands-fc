#load data from models

.libPaths( c( .libPaths(), "U:/R") )

library(arm)
library(ggplot2)
library(dplyr)
library(rstanarm)
library(shinystan)
library(lme4)
library(gridExtra)
library(grid)

load("R:/Project/NCANDS/ncands-fc/partial-models-norural.RData")
setwd("R:/Project/NCANDS/ncands-fc/")
#### beta plots for all models
models<-ls()[grep("b.", ls())]

model_out<-list()
for(i in (models)){
  temp<-get(i)
  model_out[[i]]<-plot(temp, pars="beta")+ggtitle(i)
}

pdf("model-out-partial.pdf", width=18, height=12)
grid.arrange(model_out[[1]],model_out[[2]], model_out[[3]], model_out[[4]], model_out[[5]], model_out[[6]],
             model_out[[7]],model_out[[8]],model_out[[9]],model_out[[10]],model_out[[11]],model_out[[12]],model_out[[13]],
             model_out[[14]],model_out[[15]],model_out[[16]],model_out[[17]],model_out[[18]],model_out[[19]],
             model_out[[20]],model_out[[21]],model_out[[22]],model_out[[23]],model_out[[24]])
dev.off()
### compare model estimate medians
#cbind(round(fixef(b.all.white),4), round(fixef(b.all.black),4), round(fixef(b.all.amind),4))

# ### extract coefficiencts from models
# ### all race, all gender, all offense
# all.within.sim<-sim(all.within[[1]], n.sims=1000)
# yhat<-fitted(all.within.sim, all.within[[1]])
# ## to get the uncertainty for the simulated estimates
# apply(coef(all.within.sim)$fixef, 2, function(x)quantile(x, c(0.005, 0.025, 0.975, 0.995)))
# 
# ### Want visuals for predicted report levels for low/high avg arrests by gender/race/offense
### for predicted change in report for change in arrest by gender/race/offense

#this would be easier to just sample from posterior predictive... I'll check the state vs county models, then re-run
#prefered models through rstanarm
#ultimately - (MUCH LATER) would like to do some imputation of known measurement error

### make scenarios for each
### write up what I think are interesting counterfactuals and run them
### do shifts in arrests by crime type by gender and race
### do low/high mean arrests by crime type by gender and race
### do shifts in police pc by gender and race
### do low/high mean police pc by gender and race
### pull some notable urban/rural counties, cross regions
#race.dat.bayes<-race.dat.bayes%>%filter(FIPS%in%(sample(race.dat.bayes$FIPS, 10)))
#sand<-runOffense.stan_glmer(data=race.dat.bayes, model=race.within)

# race.dat.bayes<-as.data.frame(race.dat.bayes)

### set mean values for all variables

##posterior predictive checks
### this is weird - not sure if it's drawing from the right data
### try with seattle


# ###posterior model fit against observed for b.all.all
# ydat<-b.all.all$data
# ydat[is.na(ydat)]<-0
# yrep<-posterior_predict(b.all.all, newdata = ydat)
# 
# plot(density(yrep))
# lines(density(ydat$cases), col="red")
# 
# #REWRITE FOR WITHIN.DAT - new models, make newdata for each model.
# #want to plot mean expectation, then scenario differences for each fit
# 
# 
# 
# temp<-within.dat[within.dat$n_obs==43873,]
# temp[is.na(temp)]<-0
# yhat<-posterior_predict(b.all.all, newdata = temp)
# 
# plot(density(yhat))
# points(temp$cases)

# ###################################################################################################
# ####All race, all arrest scenario
# ###################################################################################################
# ### make scenario with median values, king county wa
# newdata<-as.data.frame(lapply(within.dat, function(x) ifelse(is.numeric(x), median(as.numeric(x), na.rm=TRUE), NA)))
# ### set id variables - using king county WA for now - just moves intercepts
# newdata$FIPS<-"53033";newdata$year<-2012;newdata$stname<-"WA";newdata$state<-"53"
# newdata$race<-"all";newdata$gender<-"all";newdata$offense<-"all";newdata[which(is.na(newdata))]<-0
# newdata$child.pop<-100000
# newdata$n_obs<-43873
# scen.mean<-posterior_predict(b.all.all, newdata=newdata[1,])
# 
# ###make new scenario - diff.arrest.rt+2sd
# ###all new scenarios start at mean
# newdata<-bind_rows(newdata, newdata[1,])
# newdata$diff.arrest.rt[2]<-newdata$diff.arrest.rt[2]+2*sd(within.dat$diff.arrest.rt)
# scen.arrest.diff<-posterior_predict(b.all.all, newdata=newdata[2,])
# 
# ### mean.arrest + 2sd
# newdata<-bind_rows(newdata, newdata[1,])
# newdata$mean.arrest.rt[3]<-newdata$mean.arrest.rt[3]+2*sd(within.dat$mean.arrest.rt)
# scen.arrest.mean<-posterior_predict(b.all.all, newdata=newdata[3,])
# ### set raceblk for mean arrest + 2sd
# ### NEAT FD DENSITY PLOTS
# # plot(density(scen.mean))
# # plot(density(scen.mean-scen.arrest1sd))
# 
# plot.dat<-data.frame("model"="All", "name"="Mean predictors", "lower"=quantile(scen.mean, 0.25), "median"=quantile(scen.mean, 0.5), "upper"=quantile(scen.mean, 0.75))
# plot.dat$name<-as.character(plot.dat$name); plot.dat
# plot.dat[2,]<-c("model"="All","Arrest difference +2 sd", quantile(scen.arrest.diff, c(0.25, 0.5, 0.75)))
# plot.dat[3,]<-c("model"="All","Arrest mean + 2 sd", quantile(scen.arrest.mean, c(0.25, 0.5, 0.75)))
# 
# ###convert into rates per 
# plot.dat$lower<-as.numeric(plot.dat$lower); plot.dat$median<-as.numeric(plot.dat$median); plot.dat$upper<-as.numeric(plot.dat$upper)
# 
# ###################################################################################################
# ####African American, all arrest scenario
# ###################################################################################################
# 
# ### make scenario with median values, king county wa
# newdata<-as.data.frame(lapply(within.dat%>%filter(race=="blk"), function(x) ifelse(is.numeric(x), median(as.numeric(x), na.rm=TRUE), NA)))
# ### set id variables - using king county WA for now - just moves intercepts
# newdata$FIPS<-"53033";newdata$year<-2012;newdata$stname<-"WA";newdata$state<-"53"
# newdata$race<-"blk";newdata$gender<-"all";newdata$offense<-"all";newdata[which(is.na(newdata))]<-0
# newdata$child.pop<-100000
# newdata$n_obs<-43873
# scen.mean<-posterior_predict(b.all.black, newdata=newdata[1,])
# 
# ###make new scenario - diff.arrest.rt+2sd
# ###all new scenarios start at mean
# newdata<-bind_rows(newdata, newdata[1,])
# newdata$diff.arrest.rt[2]<-newdata$diff.arrest.rt[2]+2*sd(within.dat$diff.arrest.rt)
# scen.arrest.diff<-posterior_predict(b.all.all, newdata=newdata[2,])
# 
# ### mean.arrest + 2sd
# newdata<-bind_rows(newdata, newdata[1,])
# newdata$mean.arrest.rt[3]<-newdata$mean.arrest.rt[3]+2*sd(within.dat$mean.arrest.rt)
# scen.arrest.mean<-posterior_predict(b.all.all, newdata=newdata[3,])
# 
# plot.temp<-data.frame("model"="African American", "name"="Mean predictors", "lower"=quantile(scen.mean, 0.25), "median"=quantile(scen.mean, 0.5), "upper"=quantile(scen.mean, 0.75))
# plot.temp$name<-as.character(plot.temp$name)
# plot.temp[2,]<-c("model"="African American","Arrest difference +2 sd", quantile(scen.arrest.diff, c(0.25, 0.5, 0.75)))
# plot.temp[3,]<-c("model"="African American","Arrest mean + 2 sd", quantile(scen.arrest.mean, c(0.25, 0.5, 0.75)))
# plot.temp$lower<-as.numeric(plot.temp$lower); plot.temp$median<-as.numeric(plot.temp$median); plot.temp$upper<-as.numeric(plot.temp$upper)
# 
# plot.dat<-bind_rows(plot.dat, plot.temp)


### scenarios include: mean for all

make.plot.dat<-function(data, model, label){
  ### for RE terms, uses King County Washington 2012
  ### data term must be filtered for appropriate data
  newdata<-as.data.frame(lapply(data, function(x) ifelse(is.numeric(x), mean(as.numeric(x), na.rm=TRUE), NA)))
  newdata$FIPS<-"53033";newdata$year<-2012;newdata$stname<-"WA";newdata$state<-"53"
  newdata$race<-"all";newdata$gender<-"all";newdata$offense<-"all";newdata[which(is.na(newdata))]<-0
  newdata$child.pop<-100000
  newdata$n_obs<-0
  newdata$diff.arrest.rt<-0
  ### median scenario
  scen.mean<-posterior_predict(model, newdata=newdata[1,])
                               #, offset=log(newdata$child.pop[1]))
  #### diff.arrest + 2sd
  newdata<-bind_rows(newdata, newdata[1,])
  newdata$diff.arrest.rt[2]<-0.005
  newdata$mean.arrest.rt[2]<-as.numeric(quantile(data$mean.arrest.rt, 0.10))
  scen.arrest.diff<-posterior_predict(model, newdata=newdata[2,], offset=log(newdata$child.pop[2]))
  ### mean.arrest + 2sd
  newdata<-bind_rows(newdata, newdata[1,])
  newdata$diff.arrest.rt[3]<-0.005
  newdata$mean.arrest.rt[2]<-as.numeric(quantile(data$mean.arrest.rt, 0.90))
  scen.arrest.mean<-posterior_predict(model, newdata=newdata[3,], offset=log(newdata$child.pop[3]))
  
  ### make officers pc low/high
  newdata<-bind_rows(newdata, newdata[1,])
  newdata$mean.officers.pc<-as.numeric(quantile(data$mean.officers.pc, 0.10))
  newdata$diff.officers.pc<-0.0005
  scen.off.low<-posterior_predict(model, newdata[4,], offset=log(newdata$child.pop[4]))
  
  ### make officers pc low/high
  newdata<-bind_rows(newdata, newdata[1,])
  newdata$mean.officers.pc<-as.numeric(quantile(data$mean.officers.pc, 0.90))
  newdata$diff.officers.pc<-0.0005
  scen.off.high<-posterior_predict(model, newdata[5,], offset=log(newdata$child.pop[5]))
  
  plot.temp<-data.frame("model"=label, "name"="    -Median county", "lower"=quantile(scen.mean, 0.25), "median"=quantile(scen.mean, 0.5), "upper"=quantile(scen.mean, 0.75))
  plot.temp$name<-as.character(plot.temp$name)
  plot.temp[2,]<-c("model"=label,"  -Low avg arrest, large arrest increase", quantile(scen.arrest.diff, c(0.25, 0.5, 0.75)))
  plot.temp[3,]<-c("model"=label,"  -High avg arrest, large arrest increase", quantile(scen.arrest.mean, c(0.25, 0.5, 0.75)))
  plot.temp[4,]<-c("model"=label,"  -Low avg police, large police increase", quantile(scen.off.low, c(0.25, 0.5, 0.75)))
  plot.temp[5,]<-c("model"=label,"  -High avg police, large police increase", quantile(scen.off.high, c(0.25, 0.5, 0.75)))
  
  return(plot.temp)
}

plot.dat<-make.plot.dat(data=within.dat%>%filter(offense=="all")%>%filter(race=="all")%>%filter(gender=="all"),
                    model=b.all.all, label="Full pop.")

plot.dat<-bind_rows(plot.dat, 
                    make.plot.dat(within.dat%>%filter(offense=="all")%>%filter(gender=="male"),
                                  model=b.men.all, label="Men"))

plot.dat<-bind_rows(plot.dat, 
                    make.plot.dat(within.dat%>%filter(offense=="all")%>%filter(gender=="female"),
                                  model=b.women.all, label="Women"))

plot.dat<-bind_rows(plot.dat, 
                    make.plot.dat(within.dat%>%filter(offense=="all")%>%filter(race=="wht"),
                                  model=b.all.white, label="White"))

plot.dat<-bind_rows(plot.dat, 
                    make.plot.dat(within.dat%>%filter(offense=="all")%>%filter(race=="blk"),
                                  model=b.all.black, label="African American"))

plot.dat<-bind_rows(plot.dat, 
                    make.plot.dat(within.dat%>%filter(offense=="all")%>%filter(race=="ai"),
                                  model=b.all.amind, label="Native American"))

# plot.dat<-bind_rows(plot.dat, 
#                     make.plot.dat(within.dat%>%filter(offense=="all")%>%filter(gender=="female"),
#                                   model=b.men.drug, label="  -Men, drug arrests"))
# 
# plot.dat<-bind_rows(plot.dat, 
#                     make.plot.dat(within.dat%>%filter(offense=="all")%>%filter(gender=="female"),
#                                   model=b.women.drug, label="  -Women, drug arrests"))

plot.dat$order<-ifelse(plot.dat$model=="Full pop.", 1, ifelse(plot.dat$model=="African American", 2,
                ifelse(plot.dat$model=="Native American", 3, ifelse(plot.dat$model=="White", 4,
                ifelse(plot.dat$model=="Men", 5, ifelse(plot.dat$model=="Women", 6, NA))))))

plot.dat<-plot.dat%>%arrange((order))
plot.dat<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.dat)
index<-grep("Median", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
                plot.dat[index[2]:nrow(plot.dat),]))
index<-grep("Median", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[3]-1),], rbind(c(NA, "Native American", NA, NA, NA, NA),  
                                                 plot.dat[index[3]:nrow(plot.dat),]))
index<-grep("Median", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[4]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
                                                 plot.dat[index[4]:nrow(plot.dat),]))
index<-grep("Median", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[5]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
                                                 plot.dat[index[5]:nrow(plot.dat),]))
index<-grep("Median", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[6]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
                                                 plot.dat[index[6]:nrow(plot.dat),]))
plot.dat$offense<-"all"
plot.dat$yval<-seq(1:nrow(plot.dat))


###############################
## Violent
###############################
plot.temp<-make.plot.dat(data=within.dat%>%filter(offense=="viol")%>%filter(race=="all")%>%filter(gender=="all"),
                        model=b.all.viol, label="Full pop.")

plot.temp<-bind_rows(plot.temp, 
                    make.plot.dat(within.dat%>%filter(offense=="viol")%>%filter(gender=="male"),
                                  model=b.men.viol, label="Men"))

plot.temp<-bind_rows(plot.temp, 
                    make.plot.dat(within.dat%>%filter(offense=="viol")%>%filter(gender=="female"),
                                  model=b.women.viol, label="Women"))

plot.temp<-bind_rows(plot.temp, 
                    make.plot.dat(within.dat%>%filter(offense=="viol")%>%filter(race=="wht"),
                                  model=b.white.viol, label="White"))

plot.temp<-bind_rows(plot.temp, 
                    make.plot.dat(within.dat%>%filter(offense=="viol")%>%filter(race=="blk"),
                                  model=b.black.viol, label="African American"))

plot.temp<-bind_rows(plot.temp, 
                    make.plot.dat(within.dat%>%filter(offense=="viol")%>%filter(race=="ai"),
                                  model=b.amind.viol, label="Native American"))

# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat(within.dat%>%filter(offense=="viol")%>%filter(gender=="female"),
#                                   model=b.men.drug, label="  -Men, drug arrests"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat(within.dat%>%filter(offense=="viol")%>%filter(gender=="female"),
#                                   model=b.women.drug, label="  -Women, drug arrests"))

plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
                                                              ifelse(plot.temp$model=="Native American", 3, ifelse(plot.temp$model=="White", 4,
                                                                                                                  ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA))))))

plot.temp<-plot.temp%>%arrange((order))
plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
                                                 plot.temp[index[2]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "Native American", NA, NA, NA, NA),  
                                                 plot.temp[index[3]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
                                                 plot.temp[index[4]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
                                                 plot.temp[index[5]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[6]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
                                                 plot.temp[index[6]:nrow(plot.temp),]))
plot.temp$offense<-"viol"

plot.temp$yval<-seq(1:nrow(plot.temp))

plot.dat<-rbind(plot.dat, plot.temp)


#############################
## Drug
#############################

plot.temp<-make.plot.dat(data=within.dat%>%filter(offense=="drug")%>%filter(race=="all")%>%filter(gender=="all"),
                          model=b.all.drug, label="Full pop.")

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="drug")%>%filter(gender=="male"),
                                    model=b.men.drug, label="Men"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="drug")%>%filter(gender=="female"),
                                    model=b.women.drug, label="Women"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="drug")%>%filter(race=="wht"),
                                    model=b.white.drug, label="White"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="drug")%>%filter(race=="blk"),
                                    model=b.black.drug, label="African American"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="drug")%>%filter(race=="ai"),
                                    model=b.amind.drug, label="Native American"))

# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat(within.dat%>%filter(offense=="drug")%>%filter(gender=="female"),
#                                   model=b.men.drug, label="  -Men, drug arrests"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat(within.dat%>%filter(offense=="drug")%>%filter(gender=="female"),
#                                   model=b.women.drug, label="  -Women, drug arrests"))

plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
                                                                ifelse(plot.temp$model=="Native American", 3, ifelse(plot.temp$model=="White", 4,
                                                                                                                     ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA))))))

plot.temp<-plot.temp%>%arrange((order))
plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
                                                   plot.temp[index[2]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "Native American", NA, NA, NA, NA),  
                                                   plot.temp[index[3]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
                                                   plot.temp[index[4]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
                                                   plot.temp[index[5]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[6]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
                                                   plot.temp[index[6]:nrow(plot.temp),]))
plot.temp$offense<-"drug"
plot.temp$yval<-seq(1:nrow(plot.temp))


plot.dat<-rbind(plot.dat, plot.temp)

#############################
## QoL
#############################

plot.temp<-make.plot.dat(data=within.dat%>%filter(offense=="qol")%>%filter(race=="all")%>%filter(gender=="all"),
                          model=b.all.qol, label="Full pop.")

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="qol")%>%filter(gender=="male"),
                                    model=b.men.qol, label="Men"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="qol")%>%filter(gender=="female"),
                                    model=b.women.qol, label="Women"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="qol")%>%filter(race=="wht"),
                                    model=b.white.qol, label="White"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="qol")%>%filter(race=="blk"),
                                    model=b.black.qol, label="African American"))

plot.temp<-bind_rows(plot.temp, 
                     make.plot.dat(within.dat%>%filter(offense=="qol")%>%filter(race=="ai"),
                                    model=b.amind.qol, label="Native American"))

# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat(within.dat%>%filter(offense=="qol")%>%filter(gender=="female"),
#                                   model=b.men.drug, label="  -Men, drug arrests"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat(within.dat%>%filter(offense=="qol")%>%filter(gender=="female"),
#                                   model=b.women.drug, label="  -Women, drug arrests"))

plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
                                                                ifelse(plot.temp$model=="Native American", 3, ifelse(plot.temp$model=="White", 4,
                                                                                                                     ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA))))))

plot.temp<-plot.temp%>%arrange((order))
plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
                                                   plot.temp[index[2]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "Native American", NA, NA, NA, NA),  
                                                   plot.temp[index[3]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
                                                   plot.temp[index[4]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
                                                   plot.temp[index[5]:nrow(plot.temp),]))
index<-grep("Median", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[6]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
                                                   plot.temp[index[6]:nrow(plot.temp),]))
plot.temp$offense<-"qol"

plot.temp$yval<-seq(1:nrow(plot.temp))

plot.dat<-rbind(plot.dat, plot.temp)
plot.dat<-plot.dat[-grep("Median", plot.dat$name),]

plot.dat$lower<-as.numeric(plot.dat$lower); plot.dat$median<-as.numeric(plot.dat$median); plot.dat$upper<-as.numeric(plot.dat$upper)


#############################
## Plot
#############################





forest<-ggplot(data=plot.dat)+
  theme_bw()+
  aes(x=median, xmin=lower, xmax=upper, y=yval)+
  geom_point()+
  geom_errorbarh(height=0.2)+
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(), 
    panel.border = element_blank() 
  )+
  xlab("Predicted reports by police, 50 percent credible interval")+
  scale_y_reverse()+
  facet_wrap(~offense)

table_plot<-ggplot(plot.dat)+
  theme_bw() + 
  aes(y=yval)+
  #geom_text(aes(label=gsub("\\s2", "", model), x=0), hjust=0)+
  geom_text(aes(label=name, x=0), hjust=0)+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank() 
  )+xlim(0,6)+
  scale_y_reverse()+
  facet_wrap(~offense)

grid.draw(gridExtra::cbind.gtable(ggplotGrob(table_plot), ggplotGrob(forest),size="last"))

# 
#   geom_point(shape=21, size=3,aes(colour=model, fill=model, group=name))+
#   geom_errorbarh(aes(xmin=lower,xmax=upper, colour=model, group=name),height=0.2)
###scenario 1 - use random for now, just matters for intercept
### high diff.arrest - 1 sd up

### sample
# fit <- stan_glmer(mpg ~ wt + am + (1|cyl), data = mtcars, 
#                   iter = 400, chains = 2) # just to keep example quick
# pp_check(fit)
# pp_check(fit, plotfun="boxplot")
# plot(fit)

