#load data from models
rm(list=ls())
gc()
.libPaths( c( .libPaths(), "U:/R") )

library(arm)
library(ggplot2)
library(dplyr)
library(rstanarm)
library(rstan)
library(shinystan)
library(lme4)
library(gridExtra)
library(grid)
library(bayesplot)
library(xtable)
library(extrafont)
library(fontcm)
font_install("fontcm")
loadfonts()

load("R:/Project/NCANDS/ncands-fc/models-ALL.RData")
dat.imp<-readRDS("R:/Project/NCANDS/ncands-fc/impdata.rds")

setwd("R:/Project/NCANDS/ncands-fc/tables")

models<-ls()[grep("b\\.", ls())]

### for incomplete set to finish script
# models<-c("b.all.all", "b.men.all", "b.women.all", "b.all.black",  "b.all.white")

### make regression tables
### scenarios include: mean for all

#### TO GET PARAMETER ESTIMATES, POOL PARAMETER DRAWS ACROSS IMPUTATIONS, DRAW QUANTILES OF INTEREST (95 CI)
#### ESTIMATE VARIANCE PARAMETERS
### as in https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html

#### to do this: 
### library(rstan)
### pars(extract(b.all.all[[1]]$stanfit))
### intercept in alpha, betas in beta, REs in b (but hard to figure index)
### this function pools posterior samples across imputations, returns CI
### and variance estimates


make.plot.dat.arrest<-function(data, model, label, diff.var, mean.var){
  diff.index<-which(names(data)==diff.var)
  mean.index<-which(names(data)==mean.var)
  ### for RE terms, uses King County Washington 2012
  ### with first difference models, RE terms are canceled out, just sample those for consistency
  newdata<-as.data.frame(lapply(data, function(x) ifelse(is.numeric(x), mean(as.numeric(x), na.rm=TRUE), NA)))
  newdata$FIPS<-"00000";newdata$year<-2012;newdata$stname<-"00";newdata$state<-"00"
  newdata[which(is.na(newdata))]<-0
  newdata$n_obs<-0
  newdata[diff.index]<-0
  newdata<-rbind(newdata, newdata)
  newdata[2, diff.var]<-as.numeric(quantile(data[,diff.index], 0.9))
  newdata$child.pov<-median(data$child.pov/data$child)*newdata$child
  newdata$blk.chpov_pe<-median(data$blk.chpov_pe/data$child.blk)*newdata$child.blk
  ### 90th percentile arrest increase scenario, diff from median
  #### for list imputation objects
  sim.diff<-NULL
  m<-length(model)
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.diff<-rbind(sim.diff, sim.temp)
  }
  ###90th percentile avg arrest increase, diff from median
  newdata[2, diff.index]<-0
  newdata[2, mean.index]<-as.numeric(quantile(data[,mean.index], 0.9))
  sim.mean<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.mean<-rbind(sim.mean, sim.temp)
  }

  plot.temp<-data.frame("model"=label, "name"="   -High change in arrest", 
    "lower"=quantile(sim.diff, 0.05), "median"=quantile(sim.diff, 0.5), 
    "upper"=quantile(sim.diff, 0.95))
  plot.temp$name<-as.character(plot.temp$name)
  plot.temp[2,]<-c("model"=label,"   -High mean arrest", 
    quantile(sim.mean, c(0.05, 0.5, 0.95)))
  if(label%in%c("Full pop.", "Men", "Women")){
  plot.temp<-plot.temp%>%mutate(median=(as.numeric(median)/newdata$child[1])*1000, 
    lower=(as.numeric(lower)/newdata$child[1])*1000, 
    upper=(as.numeric(upper)/newdata$child[1])*1000)}
  if(label=="African American"){
  plot.temp<-plot.temp%>%mutate(median=(as.numeric(median)/newdata$child.blk[1])*1000, 
    lower=(as.numeric(lower)/newdata$child.blk[1])*1000, 
    upper=(as.numeric(upper)/newdata$child.blk[1])*1000)}
  if(label=="White"){
  plot.temp<-plot.temp%>%mutate(median=(as.numeric(median)/newdata$child.wht[1])*1000, 
    lower=(as.numeric(lower)/newdata$child.wht[1])*1000, 
    upper=(as.numeric(upper)/newdata$child.wht[1])*1000)}
  
  return(plot.temp)
}

make.plot.dat.officer<-function(data, model, label){
  newdata<-as.data.frame(lapply(data, function(x) ifelse(is.numeric(x), median(as.numeric(x), na.rm=TRUE), NA)))
  newdata$FIPS<-"000";newdata$year<-2012;newdata$stname<-"00";newdata$state<-"00"
  newdata[which(is.na(newdata))]<-0
  newdata$n_obs<-0
  newdata$diff.officers<-0
  newdata$diff.pol.infl.pc<-0
  newdata<-rbind(newdata, newdata)
  newdata$diff.officers[2]<-as.numeric(quantile(data$diff.officers, 0.9))
  ### 90th percentile officers and budgets increase scenario, diff from median
  m<-length(model)
  sim.diff.officers<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.diff.officers<-rbind(sim.diff.officers, sim.temp)
  }
  
  newdata$diff.officers[2]<-0
  newdata$diff.pol.infl.pc[2]<-as.numeric(quantile(data$diff.pol.infl.pc, 0.9))
  
  sim.diff.budget<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.diff.budget<-rbind(sim.diff.budget, sim.temp)
  }
  
  ###90th percentile avg officer, budget increase, diff from median
  newdata$diff.officers[2]<-0; newdata$diff.pol.infl.pc<-0
  newdata$mean.officers[2]<-as.numeric(quantile(data$mean.officers, 0.9))
  
  sim.mean.officers<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.mean.officers<-rbind(sim.mean.officers, sim.temp)
  }
  
  newdata$mean.officers[2]<-newdata$mean.officers[1]
  newdata$mean.pol.infl.pc[2]<-as.numeric(quantile(data$mean.pol.infl.pc, 0.9))
  
  sim.mean.budgets<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.mean.budgets<-rbind(sim.mean.budgets, sim.temp)
  }
  
  plot.temp<-data.frame("model"=label, "name"="   -High change in officers", 
    "lower"=quantile(sim.diff.officers, 0.05), 
    "median"=quantile(sim.diff.officers, 0.5), 
    "upper"=quantile(sim.diff.officers, 0.95))
  plot.temp$name<-as.character(plot.temp$name)
  plot.temp[2,]<-c("model"=label,"   -High change in budget", 
    quantile(sim.diff.budget, c(0.05, 0.5, 0.95)))
  plot.temp[3,]<-c("model"=label,"   -High average officers", 
    quantile(sim.mean.officers, c(0.05, 0.5, 0.95)))
  plot.temp[4,]<-c("model"=label,"   -High average budget", 
    quantile(sim.mean.budgets, c(0.05, 0.5, 0.95)))
  
  if(label%in%c("Full pop.", "Men", "Women")){
  plot.temp<-plot.temp%>%mutate(median=(as.numeric(median)/newdata$child[1])*1000, 
    lower=(as.numeric(lower)/newdata$child[1])*1000, 
    upper=(as.numeric(upper)/newdata$child[1])*1000)}
  if(label=="African American"){
  plot.temp<-plot.temp%>%mutate(median=(as.numeric(median)/newdata$child.blk[1])*1000, 
    lower=(as.numeric(lower)/newdata$child.blk[1])*1000, 
    upper=(as.numeric(upper)/newdata$child.blk[1])*1000)}
  if(label=="White"){
  plot.temp<-plot.temp%>%mutate(median=(as.numeric(median)/newdata$child.wht[1])*1000, 
    lower=(as.numeric(lower)/newdata$child.wht[1])*1000, 
    upper=(as.numeric(upper)/newdata$child.wht[1])*1000)}
  
  return(plot.temp)
}

plot.dat<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.all, label="Full pop.", diff.var="diff.arrest.all", mean.var="mean.arrest.all")

plot.dat<-bind_rows(plot.dat, 
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.men.all, label="Men", diff.var="diff.arrest.male", mean.var="mean.arrest.male"))

plot.dat<-bind_rows(plot.dat, 
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.women.all, label="Women", diff.var="diff.arrest.female", mean.var="mean.arrest.female"))

plot.dat<-bind_rows(plot.dat, 
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.black, label="African American", diff.var="diff.arrest.blk", mean.var="mean.arrest.blk"))

plot.dat<-bind_rows(plot.dat, 
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.white, label="White", diff.var="diff.arrest.wht", mean.var="mean.arrest.wht"))

# plot.dat<-bind_rows(plot.dat, 
#                     make.plot.dat.arrest(data=dat.imp$imputations[[1]],
#                                          model=b.all.amind, label="Native American", diff.var="diff.arrest.ai", mean.var="mean.arrest.ai"))


### DROP Native American for this analysis, exclusion of rural has huge effect on results

plot.dat$order<-ifelse(plot.dat$model=="Full pop.", 1, 
                ifelse(plot.dat$model=="African American", 2,
                # ifelse(plot.dat$model=="Native American", 3, 
                ifelse(plot.dat$model=="White", 3,
                ifelse(plot.dat$model=="Men", 4, ifelse(plot.dat$model=="Women", 5, NA)))))

plot.dat<-plot.dat%>%arrange((order))
plot.dat<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.dat)
index<-grep("High change in arrest", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
                plot.dat[index[2]:nrow(plot.dat),]))
# index<-grep("High change in arrest", plot.dat$name)
# plot.dat<-rbind(plot.dat[1:(index[3]-1),], rbind(c(NA, "Native American", NA, NA, NA, NA),  
#                                                  plot.dat[index[3]:nrow(plot.dat),]))
index<-grep("High change in arrest", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
                                                 plot.dat[index[3]:nrow(plot.dat),]))
index<-grep("High change in arrest", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[4]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
                                                 plot.dat[index[4]:nrow(plot.dat),]))
index<-grep("High change in arrest", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[5]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
                                                 plot.dat[index[5]:nrow(plot.dat),]))
plot.dat$offense<-"All"
plot.dat$yval<-seq(1:nrow(plot.dat))

# ###############################
# ## Violent
# ###############################
plot.temp<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.viol, label="Full pop.", diff.var="diff.viol.all", mean.var="mean.viol.all")

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.men.viol, label="Men", diff.var="diff.viol.male", mean.var="mean.viol.male"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.women.viol, label="Women", diff.var="diff.viol.female", mean.var="mean.viol.female"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.blk.viol, label="African American", diff.var="diff.viol.blk", mean.var="mean.viol.blk"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.wht.viol, label="White", diff.var="diff.viol.wht", mean.var="mean.viol.wht"))


plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
                                      #ifelse(plot.temp$model=="Native American", 3,
                                      ifelse(plot.temp$model=="White", 4,
                                        ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA)))))

plot.temp<-plot.temp%>%arrange((order))
plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),
                                                 plot.temp[index[2]:nrow(plot.temp),]))

index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),
                                                 plot.temp[index[3]:nrow(plot.temp),]))
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),
                                                 plot.temp[index[4]:nrow(plot.temp),]))
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),
                                                 plot.temp[index[5]:nrow(plot.temp),]))
plot.temp$offense<-"Violent"

plot.temp$yval<-seq(1:nrow(plot.temp))

plot.dat<-rbind(plot.dat, plot.temp)
#
#
# #############################
# ## Drug
# #############################
plot.temp<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.drug, label="Full pop.", diff.var="diff.drug.all", mean.var="mean.drug.all")

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.men.drug, label="Men", diff.var="diff.drug.male", mean.var="mean.drug.male"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.women.drug, label="Women", diff.var="diff.drug.female", mean.var="mean.drug.female"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.blk.drug, label="African American", diff.var="diff.drug.blk", mean.var="mean.drug.blk"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.wht.drug, label="White", diff.var="diff.drug.wht", mean.var="mean.drug.wht"))


plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
                                      #ifelse(plot.temp$model=="Native American", 3,
                                      ifelse(plot.temp$model=="White", 4,
                                        ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA)))))

plot.temp<-plot.temp%>%arrange((order))
plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),
                                                 plot.temp[index[2]:nrow(plot.temp),]))

index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),
                                                 plot.temp[index[3]:nrow(plot.temp),]))
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),
                                                 plot.temp[index[4]:nrow(plot.temp),]))
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),
                                                 plot.temp[index[5]:nrow(plot.temp),]))
plot.temp$offense<-"Drug"

plot.temp$yval<-seq(1:nrow(plot.temp))

plot.dat<-rbind(plot.dat, plot.temp)
#
#
# #############################
# ## QoL
# #############################
plot.temp<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.qol, label="Full pop.", diff.var="diff.qol.all", mean.var="mean.qol.all")

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.men.qol, label="Men", diff.var="diff.qol.male", mean.var="mean.qol.male"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.women.qol, label="Women", diff.var="diff.qol.female", mean.var="mean.qol.female"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.blk.qol, label="African American", diff.var="diff.qol.blk", mean.var="mean.qol.blk"))

plot.temp<-bind_rows(plot.temp,
  make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.wht.qol, label="White", diff.var="diff.qol.wht", mean.var="mean.qol.wht"))


plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
                                      #ifelse(plot.temp$model=="Native American", 3,
                                      ifelse(plot.temp$model=="White", 4,
                                        ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA)))))

plot.temp<-plot.temp%>%arrange((order))
plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),
                                                 plot.temp[index[2]:nrow(plot.temp),]))

index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),
                                                 plot.temp[index[3]:nrow(plot.temp),]))
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),
                                                 plot.temp[index[4]:nrow(plot.temp),]))
index<-grep("High change in arrest", plot.temp$name)
plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),
                                                 plot.temp[index[5]:nrow(plot.temp),]))
plot.temp$offense<-"Quality of Life"

plot.temp$yval<-seq(1:nrow(plot.temp))

plot.dat<-rbind(plot.dat, plot.temp)
#############################
## Plot
#############################

plot.dat$lower<-as.numeric(plot.dat$lower); plot.dat$median<-as.numeric(plot.dat$median); plot.dat$upper<-as.numeric(plot.dat$upper)

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
    text = element_text(family="CM Roman"),
    panel.border = element_blank() 
  )+
  xlab("Predicted change in reports by police per 1,000 children")+
  scale_y_reverse()+
  geom_vline(xintercept=0, linetype=2)+
  facet_wrap(~offense)

table_plot<-ggplot(plot.dat%>%filter(offense%in%c("All", "Quality of Life")))+
  theme_bw() + 
  aes(y=yval)+
  #geom_text(aes(label=gsub("\\s2", "", model), x=0), hjust=0)+
  geom_text(aes(label=name, x=0), hjust=0)+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    text = element_text(family="CM Roman"),
    strip.text.x = element_blank()
  )+xlim(0,6)+
  scale_y_reverse()+
  facet_wrap(~offense, ncol=1)

pdf("within-predict-diff.pdf", family="CM Roman", width=7, height=8)
grid.draw(gridExtra::cbind.gtable(ggplotGrob(table_plot), ggplotGrob(forest),size="last"))
dev.off()

sink("plot-dat.txt")
plot.dat
sink()

###########################################
# For officers, budgets
########################################3
plot.dat<-make.plot.dat.officer(data=dat.imp$imputations[[1]],
                               model=b.all.all, label="Full pop.")

plot.dat<-bind_rows(plot.dat,
                    make.plot.dat.officer(data=dat.imp$imputations[[1]],
                                         model=b.all.white, label="White"))

plot.dat<-bind_rows(plot.dat,
                    make.plot.dat.officer(data=dat.imp$imputations[[1]],
                                         model=b.all.black, label="African American"))

# plot.dat<-bind_rows(plot.dat,
#                     make.plot.dat.officer(data=dat.imp$imputations[[1]],
#                                           model=b.all.amind, label="Native American"))

plot.dat$order<-ifelse(plot.dat$model=="Full pop.", 1, 
                       ifelse(plot.dat$model=="African American", 2,
                        # ifelse(plot.dat$model=="Native American", 3,
                        ifelse(plot.dat$model=="White", 3, NA)))
                                                                     

plot.dat<-plot.dat%>%arrange((order))
plot.dat<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.dat)
index<-grep("High change in officers", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),
                                                 plot.dat[index[2]:nrow(plot.dat),]))
# index<-grep("High change in officers", plot.dat$name)
# plot.dat<-rbind(plot.dat[1:(index[3]-1),], rbind(c(NA, "Native American", NA, NA, NA, NA),
#                                                  plot.dat[index[3]:nrow(plot.dat),]))

index<-grep("High change in officers", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),
                                                 plot.dat[index[3]:nrow(plot.dat),]))
plot.dat$yval<-seq(1:nrow(plot.dat))

plot.dat$median<-as.numeric(plot.dat$median); plot.dat$upper<-as.numeric(plot.dat$upper); plot.dat$lower<-as.numeric(plot.dat$lower)

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
    panel.border = element_blank() ,
    text=element_text(family="CM Roman")
  )+
  xlab("Predicted change in reports by police per 1,000 children")+
  geom_vline(xintercept=0, linetype=2)+
  scale_y_reverse()

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
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+xlim(0,6)+
  scale_y_reverse()
pdf("within-predict-officer-diff.pdf", family="CM Roman", width=8, height=8)
grid.draw(gridExtra::cbind.gtable(ggplotGrob(table_plot), ggplotGrob(forest),size="last"))
dev.off()

sink("officer-plot.txt")
plot.dat
sink()
##### for tables

### make regression tables - use ciList and models[] as index

impPars<-function(model){
  m<-length(model)
  alphas<-NULL # model intercept
  betas<-NULL #regression betas
  variance<-NULL #variance parameters
  for(j in 1:m){
    samples<-rstan::extract(model[[j]]$stanfit)
    alphas<-rbind(alphas, samples$alpha)
    betas<-rbind(betas, samples$beta)
    variance<-rbind(variance, samples$b)
  }
  ParNames<-row.names(model[[1]]$stan_summary)[1:(nrow(model[[1]]$stan_summary)-5)]
  ### draw medians and 95 percent intervals for intercept, betas
  ci<-t(quantile(alphas, c(0.025, 0.5, 0.975)))
  ci<-rbind(ci, 
            t(apply(betas, 2, function(x){quantile(x, c(0.025, 0.5, 0.975))})))
  row.names(ci)<-ParNames[1:nrow(ci)]
  
  obs_error.index<-grep("n_obs", ParNames)-nrow(ci)
  fips_error.index<-grep("FIPS", ParNames)-nrow(ci)
  st_error.index<-grep("state", ParNames)-nrow(ci)
  var_obs<-sd(as.vector(variance[,obs_error.index]))
  var_fips<-sd(as.vector(variance[,fips_error.index]))
  var_st<-sd(as.vector(variance[,st_error.index]))
  
  ci<-rbind(ci, 
            c(var_obs, NA, NA))
  row.names(ci)[nrow(ci)]<-"SD:n_obs"
  ci<-rbind(ci, 
            c(var_fips, NA, NA))
  row.names(ci)[nrow(ci)]<-"SD:FIPS"
  ci<-rbind(ci, 
            c(var_st, NA, NA))
  row.names(ci)[nrow(ci)]<-"SD:state"
  colnames(ci)<-c("Lower", "Median", "Upper")
  return(ci)
}

ciList<-list()
for(i in 1:length(models)){
  print(models[[i]])
  ciList[[i]]<-impPars(get(models[[i]]))
  names(ciList)[[i]]<-models[[i]]
}

### clean up table names
nameClean<-function(x){
  n<-row.names(x)
  for(i in 1:length(n)){
    if(n[i]%in%c("scale(diff.arrest.all)","scale(diff.arrest.blk)",
                 "scale(diff.arrest.wht)", "scale(diff.arrest.ai)",
                 "scale(diff.arrest.male)", "scale(diff.arrest.female)"))
    {n[i]<-"Change in arrests"}
    
    if(n[i]%in%c("scale(diff.drug.all)","scale(diff.drug.blk)",
                 "scale(diff.drug.wht)", "scale(diff.drug.ai)",
                 "scale(diff.drug.male)", "scale(diff.drug.female)"))
    {n[i]<-"Change in drug arrests"}
    
    if(n[i]%in%c("scale(diff.viol.all)","scale(diff.viol.blk)",
                 "scale(diff.viol.wht)", "scale(diff.viol.ai)",
                 "scale(diff.viol.male)", "scale(diff.viol.female)"))
    {n[i]<-"Change in violent arrests"}
    
    if(n[i]%in%c("scale(diff.qol.all)","scale(diff.qol.blk)",
                 "scale(diff.qol.wht)", "scale(diff.qol.ai)",
                 "scale(diff.qol.male)", "scale(diff.qol.female)"))
    {n[i]<-"Change in quality of life arrests"}
    
    if(n[i]%in%c("scale(diff.officers)"))
    {n[i]<-"Change in officers"}
    if(n[i]%in%c("scale(diff.pol.infl.pc)"))
    {n[i]<-"Change in police budgets (state)"}
    if(n[i]%in%c("scale(diff.child.pov)", "scale(diff.wht.chpov_pe)", 
                 "scale(diff.blk.chpov_pe)", "scale(diff.ai.chpov_pe"))
    {n[i]<-"Change in child poverty"}
    if(n[i]%in%c("scale(diff.MURDER_mav)"))
    {n[i]<-"Change in homicide"}
    if(n[i]%in%c("scale(diff.infmort)", "scale(diff.wht.infmort)", "scale(diff.nonwht.infmort)"))
    {n[i]<-"Change in infant mortality"}
    if(n[i]%in%c("scale(diff.median.hh.income)"))
    {n[i]<-"Change in median income"}
    if(n[i]%in%c("scale(mean.arrest.all)","scale(mean.arrest.blk)",
                 "scale(mean.arrest.wht)", "scale(mean.arrest.ai)"))
    {n[i]<-"Mean arrests"}
    if(n[i]%in%c("scale(mean.officers)"))
    {n[i]<-"Mean officers"}
    if(n[i]%in%c("scale(mean.pol.infl.pc)"))
    {n[i]<-"Mean police budgets (state)"}
    if(n[i]%in%c("scale(mean.child.pov)", "scale(mean.wht.chpov_pe)", 
                 "scale(mean.blk.chpov_pe)", "scale(mean.ai.chpov_pe"))
    {n[i]<-"Mean child poverty"}
    if(n[i]%in%c("scale(mean.MURDER_mav)"))
    {n[i]<-"Mean homicide"}
    if(n[i]%in%c("scale(mean.infmort)", "scale(mean.wht.infmort)", "scale(mean.nonwht.infmort)"))
    {n[i]<-"Mean infant mortality"}
    if(n[i]%in%c("scale(mean.median.hh.income)"))
    {n[i]<-"Mean median income"}
    
    if(n[i]%in%c("scale(pop.density)"))
    {n[i]<-"Population density"}
    if(n[i]%in%c("scale(pct.blk)"))
    {n[i]<-"Percent African American"}
    if(n[i]%in%c("scale(pct.ai)"))
    {n[i]<-"Percent American Indian"}
    if(n[i]%in%c("scale(year)"))
    {n[i]<-"Year"}
    if(n[i]%in%c("SD:n_obs"))
    {n[i]<-"SD: Observation"}
    if(n[i]%in%c("SD:FIPS"))
    {n[i]<-"SD: County"}
    if(n[i]%in%c("SD:state"))
    {n[i]<-"SD: State"}
    
    if(n[i]%in%c("scale(mean.drug.all)","scale(mean.drug.blk)",
                 "scale(mean.drug.wht)", "scale(mean.drug.ai)",
                 "scale(mean.drug.male)", "scale(mean.drug.female)"))
    {n[i]<-"Mean drug arrests"}
    
    if(n[i]%in%c("scale(mean.viol.all)","scale(mean.viol.blk)",
                 "scale(mean.viol.wht)", "scale(mean.viol.ai)",
                 "scale(mean.viol.male)", "scale(mean.viol.female)"))
    {n[i]<-"Mean violent arrests"}
    
    if(n[i]%in%c("scale(mean.qol.all)","scale(mean.qol.blk)",
                 "scale(mean.qol.wht)", "scale(mean.qol.ai)",
                 "scale(mean.qol.male)", "scale(mean.qol.female)"))
    {n[i]<-"Mean quality of life arrests"}
    
  }
  row.names(x)<-n
  return(x)
}

print(xtable(nameClean(ciList$b.all.all), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, all arrests"),  file="b-all-all.tex",
      caption.placement = getOption("xtable.caption.placement", "top"))
print(xtable(nameClean(ciList$b.all.viol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, violent arrests"),  file="b-all-viol.tex")
print(xtable(nameClean(ciList$b.all.drug), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, drug arrests"),  file="b-all-drug.tex")
print(xtable(nameClean(ciList$b.all.qol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, quality of life arrests"),  file="b-all-qol.tex")

print(xtable(nameClean(ciList$b.men.all), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, male arrests"),  file="b-men-all.tex")
print(xtable(nameClean(ciList$b.men.viol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, male violent arrests"),  file="b-men-viol.tex")
print(xtable(nameClean(ciList$b.men.drug), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, male drug arrests"),  file="b-men-drug.tex")
print(xtable(nameClean(ciList$b.men.qol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, male quality of life arrests"),  file="b-men-qol.tex")

print(xtable(nameClean(ciList$b.women.all), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, female arrests"),  file="b-women-all.tex")
print(xtable(nameClean(ciList$b.women.viol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, female violent arrests"),  file="b-women-viol.tex")
print(xtable(nameClean(ciList$b.women.drug), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, female drug arrests"),  file="b-women-drug.tex")
print(xtable(nameClean(ciList$b.women.qol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. All children, female quality of life arrests"),  file="b-women-qol.tex")

print(xtable(nameClean(ciList$b.all.black), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. African American children, all African American arrests"),  file="b-blk-all.tex")
print(xtable(nameClean(ciList$b.blk.viol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. African American children, African American violent arrests"),  file="b-blk-viol.tex")
print(xtable(nameClean(ciList$b.blk.drug), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. African American children, African American drug arrests"),  file="b-blk-drug.tex")
print(xtable(nameClean(ciList$b.blk.qol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. African American children, African American quality of life arrests"),  file="b-blk-qol.tex")

print(xtable(nameClean(ciList$b.all.amind), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. American Indian children, all American Indian arrests"),  file="b-ai-all.tex")
print(xtable(nameClean(ciList$b.ai.viol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. American Indian children, American Indian violent arrests"),  file="b-ai-viol.tex")
print(xtable(nameClean(ciList$b.drug.ai), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. American Indian children, American Indian drug arrests"),  file="b-ai-drug.tex")
print(xtable(nameClean(ciList$b.ai.qol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. American Indian children, American Indian quality of life arrests"),  file="b-ai-qol.tex")


print(xtable(nameClean(ciList$b.all.white), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. White children, all white arrests"),  file="b-wht-all.tex")
print(xtable(nameClean(ciList$b.wht.viol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. White children, white violent arrests"),  file="b-blk-viol.tex")
print(xtable(nameClean(ciList$b.wht.drug), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. White children, white drug arrests"),  file="b-blk-drug.tex")
print(xtable(nameClean(ciList$b.wht.qol), caption = "Parameter estimates and 95 percent posterior intervals, multilevel models of 
             police child maltreatment reports. White children, white quality of life arrests"),  file="b-blk-qol.tex")

### pull top pop in sample from each census sub-region
### list is: Cook, IL (17031); Harris, TX (48201); Maricopa, Az(04013); King, WA (53033); Suffolk, NY (36103; 
### Miami-Dade, FL (12086); Hennepin, MN (27053); 
### Middlesex, MA (25017); Shelby, TN (47157)
### However, not all counties here have x race, x gender
### pull top pop counties from 2012, get different states

c.index<-c("17031", "48201", "4013", "53033", "36103", "12086", "27053", "25017", "47157")
county.samp<-dat.imp$imputations[[1]]%>%filter(FIPS%in%c.index)
county.samp[is.na(county.samp)]<-0

plot.temp<-county.samp

ts.check<-ggplot(plot.temp)+aes(x=year, y=(arrest.all/(men+women))*100000)+geom_line()+facet_wrap(~county)

### predict a new year for these counties using 2011 data (all cases in), drop n_obs term
county.newdat<-county.samp%>%filter(year==2008)
county.newdat$n_obs<-rnorm(nrow(county.newdat), 1, 1)

county.newdat$rpt.pc<-county.newdat$polrpt/county.newdat$child * 1000
county.newdat$rpt.pc.wht<-county.newdat$polrpt.wht/county.newdat$child.wht *1000
county.newdat$rpt.pc.blk<-county.newdat$polrpt.blk/county.newdat$child.blk *1000
county.newdat$rpt.pc.ai<-county.newdat$polrpt.ai/county.newdat$child.ai *1000

all.c<-lapply(b.all.all, function(x) posterior_predict(x, county.newdat))  
all.c<-Reduce(function(...) merge(..., all=T), all.c)
names(all.c)<-paste(county.newdat$county, county.newdat$stname, sep=",")

white.c<-lapply(b.all.white, function(x) posterior_predict(x, county.newdat))  
white.c<-Reduce(function(...) merge(..., all=T), white.c)
names(white.c)<-paste(county.newdat$county, county.newdat$stname, sep=",")

black.c<-lapply(b.all.black, function(x) posterior_predict(x, county.newdat))  
black.c<-Reduce(function(...) merge(..., all=T), black.c)
names(black.c)<-paste(county.newdat$county, county.newdat$stname, sep=",")

# amind.c<-lapply(b.all.amind, function(x) posterior_predict(x, county.newdat))  
# amind.c<-Reduce(function(...) merge(..., all=T), amind.c)
# names(amind.c)<-paste(county.newdat$county, county.newdat$stname, sep=",")

makeCRow<-function(x, cat){
  out<-data.frame("County"=rep(NA, ncol(x)), "cat"=rep(cat, ncol(x)), 
                  "lower"=rep(NA, ncol(x)), "median"=rep(NA, ncol(x)), "upper"=rep(NA, ncol(x)))
  for(i in 1:ncol(x)){
    out$County[i]<-names(x)[i]
    out$lower[i]<-quantile(x[,i], 0.05)
    out$median[i]<-median(x[,i])
    out$upper[i]<-quantile(x[,i], 0.95)
  }
  return(out)
}

#### then draw CIs and prepare for plotting, standardize by child pop

all.ci<-makeCRow(all.c, "All")
offset<-county.newdat$child
all.ci$lower<-(all.ci$lower/offset)*1000; all.ci$median<-(all.ci$median/offset)*1000; all.ci$upper<-(all.ci$upper/offset)*1000

temp.ci<-makeCRow(black.c, "African American")
offset<-county.newdat$child.blk
temp.ci$lower<-(temp.ci$lower/offset)*1000; temp.ci$median<-(temp.ci$median/offset)*1000; temp.ci$upper<-(temp.ci$upper/offset)*1000
all.ci<-bind_rows(all.ci, temp.ci)

#really crappy predictions for American Indian kids in these cities - small pop problem

# temp.ci<-makeCRow(amind.c, "American Indian")
# offset<-county.newdat$child.ai
# temp.ci$lower<-(temp.ci$lower/offset)*1000; temp.ci$median<-(temp.ci$median/offset)*1000; temp.ci$upper<-(temp.ci$upper/offset)*1000
# all.ci<-bind_rows(all.ci, temp.ci)

temp.ci<-makeCRow(white.c, "White")
offset<-county.newdat$child.wht
temp.ci$lower<-(temp.ci$lower/offset)*1000; temp.ci$median<-(temp.ci$median/offset)*1000; temp.ci$upper<-(temp.ci$upper/offset)*1000
all.ci<-bind_rows(all.ci, temp.ci)

all.ci$cat<-factor(all.ci$cat, levels=c("White", "African American", "All"))

### add observed data

forest<-ggplot(data=all.ci)+
  theme_bw()+
  aes(x=median, xmin=lower, xmax=upper, y=cat)+
  geom_point()+
  geom_errorbarh(height=0.2)+
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    text=element_text(family="CM Roman"),
    panel.border = element_blank()
  )+
  xlab("Predicted reports by police per 1,000 children")+
  # scale_y_reverse()+
  facet_wrap(~County)
pdf("county-predict.pdf",family="CM Roman", width=7, height=8)
forest
dev.off()

sink("county-plot.txt")
all.ci
sink()

###descriptive tables

desc<-dat.in

desc<-desc%>%mutate(rpt.pc=polrpt/child*1000, rpt.pc.blk=polrpt.blk/child.blk*1000,
                    rpt.pc.wht=polrpt.wht/child.wht*1000, arrest.rt=desc$arrest.all/desc$adult * 1000,
                    arrest.rt.blk=arrest.blk/adult.blk * 1000, arrest.rt.wht=arrest.wht/adult.wht * 1000,
                    arrest.rt.men=arrest.male/men * 1000,arrest.rt.women= arrest.female/women * 1000,
                    viol.rt=viol.all/adult * 1000, viol.rt.blk=viol.blk/adult.blk * 1000,
                    viol.rt.wht=viol.wht/adult.wht * 1000, viol.rt.women= viol.female/women * 1000,
                    viol.rt.men=viol.male/men*1000, drug.rt=drug.all/adult * 1000, 
                    drug.rt.blk=drug.blk/adult.blk * 1000,
                    drug.rt.wht=drug.wht/adult.wht*1000, drug.rt.men=drug.male/men*1000,
                    drug.rt.women=drug.female/women*1000, qol.rt=qol.all/adult*1000,
                    qol.rt.wht=qol.wht/adult.wht*1000, qol.rt.blk=qol.blk/adult.blk*1000,
                    qol.rt.men=qol.male/men*1000, qol.rt.women=qol.female/women*1000,
                    officers.pc=officers/(adult+child)*1000)

makeDesc<-function(x, var){
  mean<-mean(x[,var], na.rm=TRUE)
  within<-as.numeric(unlist(x%>%group_by(FIPS)%>%rename_(var=var)%>%summarise(sd=sd(var, na.rm=TRUE))%>%summarise(within=mean(sd, na.rm=TRUE))))
  between<-as.numeric(unlist(x%>%group_by(FIPS)%>%rename_(var=var)%>%summarise(sd=mean(var, na.rm=TRUE))%>%summarise(between=sd(sd, na.rm=TRUE))))
  return(c(mean, within, between))
}

desctab<-data.frame("name"=NA, "Mean"=NA, "Within"=NA, "Between"=NA)
desctab[1,]<-c("Police maltreatment reports per 1,000 children",NA, NA, NA)
desctab<-rbind(desctab, c("       - All children",makeDesc(desc, "rpt.pc")))
desctab<-rbind(desctab, c("       - African American children", makeDesc(desc, "rpt.pc.blk")))
desctab<-rbind(desctab, c("       - White children",makeDesc(desc, "rpt.pc.wht")))

desctab<-rbind(desctab, c("Arrests per 1,000 adults",NA, NA, NA))
desctab<-rbind(desctab, c("       - All adults",makeDesc(desc, "arrest.rt")))
desctab<-rbind(desctab, c("       - African American adults",makeDesc(desc, "arrest.rt.blk")))
desctab<-rbind(desctab, c("       - White adults",makeDesc(desc, "arrest.rt.wht")))
desctab<-rbind(desctab, c("       - Men",makeDesc(desc, "arrest.rt.men")))
desctab<-rbind(desctab, c("       - Women",makeDesc(desc, "arrest.rt.women")))

desctab<-rbind(desctab, c("Violent arrests per 1,000 adults", NA, NA, NA))
desctab<-rbind(desctab, c("       - All adults",makeDesc(desc, "viol.rt")))
desctab<-rbind(desctab, c("       - African American adults",makeDesc(desc, "viol.rt.blk")))
desctab<-rbind(desctab, c("       - White adults",makeDesc(desc, "viol.rt.wht")))
desctab<-rbind(desctab, c("       - Men",makeDesc(desc, "viol.rt.men")))
desctab<-rbind(desctab, c("       - Women",makeDesc(desc, "viol.rt.women")))


desctab<-rbind(desctab, c("Drug arrests per 1,000 adults",NA, NA, NA))
desctab<-rbind(desctab, c("       - All adults",makeDesc(desc, "drug.rt")))
desctab<-rbind(desctab, c("       - African American adults",makeDesc(desc, "drug.rt.blk")))
desctab<-rbind(desctab, c("       - White adults",makeDesc(desc, "drug.rt.wht")))
desctab<-rbind(desctab, c("       - Men",makeDesc(desc, "drug.rt.men")))
desctab<-rbind(desctab, c("       - Women",makeDesc(desc, "drug.rt.women")))


desctab<-rbind(desctab, c("Quality of life arrests per 1,000 adults", NA, NA, NA))
desctab<-rbind(desctab, c("       - All adults",makeDesc(desc, "qol.rt")))
desctab<-rbind(desctab, c("       - African American adults",makeDesc(desc, "qol.rt.blk")))
desctab<-rbind(desctab, c("       - White adults", makeDesc(desc, "qol.rt.wht")))
desctab<-rbind(desctab, c("       - Men",makeDesc(desc, "qol.rt.men")))
desctab<-rbind(desctab, c("       - Women",makeDesc(desc, "qol.rt.women")))

desctab<-rbind(desctab, c("Full-time police officers per 1,000 persons",makeDesc(desc,"officers.pc")))

desctab<-rbind(desctab, c("Police operating budgets per capita, state-level",makeDesc(desc, "pol.infl.pc")))

desctab$Mean<-as.numeric(desctab$Mean); desctab$Within<-as.numeric(desctab$Within); desctab$Between<-as.numeric(desctab$Between)

descout<-xtable(desctab, align="llccc", digits=1, label="desc3",
  caption="Police and maltreatment reporting in large US Counties. Average values, average within-county standard deviation, and between-county standard deviation")
names(descout)<-c(" ", "Mean", "Within SD", "Between SD")
print.xtable(descout, include.rownames=FALSE, caption.placement = "top", file="descriptives.tex")
