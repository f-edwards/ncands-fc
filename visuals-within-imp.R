#load data from models

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

load("R:/Project/NCANDS/ncands-fc/models-imp.RData")

setwd("R:/Project/NCANDS/ncands-fc/tables")

models<-ls()[grep("b\\.", ls())]

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
impPars<-function(model){
  m<-length(model)
  alphas<-NULL # model intercept
  betas<-NULL #regression betas
  variance<-NULL #variance parameters
  for(i in 1:m){
    samples<-extract(model[[i]]$stanfit)
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
  return(ci)
}

make.plot.dat.arrest<-function(data, model, label, diff.var, mean.var){
  diff.index<-which(names(data)==diff.var)
  mean.index<-which(names(data)==mean.var)
  ### for RE terms, uses King County Washington 2012
  ### with first difference models, RE terms are canceled out, just sample those for consistency
  newdata<-as.data.frame(lapply(data, function(x) ifelse(is.numeric(x), median(as.numeric(x), na.rm=TRUE), NA)))
  newdata$FIPS<-"00000";newdata$year<-2012;newdata$stname<-"00";newdata$state<-"00"
  newdata[which(is.na(newdata))]<-0
  newdata$n_obs<-0
  newdata[diff.index]<-0
  newdata<-rbind(newdata, newdata)
  newdata[2, diff.var]<-as.numeric(quantile(data[,diff.index], 0.90))
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
  newdata[2, mean.index]<-as.numeric(quantile(data[,mean.index], 0.90))
  sim.mean<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.mean<-rbind(sim.mean, sim.temp)
  }

  plot.temp<-data.frame("model"=label, "name"="   -High change in arrest", 
    "lower"=quantile(sim.diff, 0.25), "median"=quantile(sim.diff, 0.5), 
    "upper"=quantile(sim.diff, 0.75))
  plot.temp$name<-as.character(plot.temp$name)
  plot.temp[2,]<-c("model"=label,"   -High mean arrest", 
    quantile(sim.mean, c(0.25, 0.5, 0.75)))
  
  plot.temp<-plot.temp%>%mutate(median=(as.numeric(median)/newdata$child[1])*100000, 
    lower=(as.numeric(lower)/newdata$child[1])*100000, 
    upper=(as.numeric(upper)/newdata$child[1])*100000)
  
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
  newdata$diff.officers[2]<-as.numeric(quantile(data$diff.officers, 0.90))
  ### 90th percentile officers and budgets increase scenario, diff from median
  m<-length(model)
  sim.diff.officers<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.diff.officers<-rbind(sim.diff.officers, sim.temp)
  }
  
  newdata$diff.officers[2]<-0
  newdata$diff.pol.infl.pc[2]<-as.numeric(quantile(data$diff.pol.infl.pc, 0.90))
  
  sim.diff.budget<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.diff.budget<-rbind(sim.diff.budget, sim.temp)
  }
  
  ###90th percentile avg officer, budget increase, diff from median
  newdata$diff.officers[2]<-0; newdata$diff.pol.infl.pc<-0
  newdata$mean.officers[2]<-as.numeric(quantile(data$mean.officers, 0.90))
  
  sim.mean.officers<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.mean.officers<-rbind(sim.mean.officers, sim.temp)
  }
  
  newdata$mean.officers[2]<-newdata$mean.officers[1]
  newdata$mean.pol.infl.pc[2]<-as.numeric(quantile(data$mean.pol.infl.pc, 0.90))
  
  sim.mean.budgets<-NULL
  for(i in 1:m){
    sim.temp<-apply(posterior_predict(model[[i]], newdata=newdata), 1, diff)
    sim.mean.budgets<-rbind(sim.mean.budgets, sim.temp)
  }
  
  plot.temp<-data.frame("model"=label, "name"="   -High change in officers", 
    "lower"=quantile(sim.diff.officers, 0.25), 
    "median"=quantile(sim.diff.officers, 0.5), 
    "upper"=quantile(sim.diff.officers, 0.75))
  plot.temp$name<-as.character(plot.temp$name)
  plot.temp[2,]<-c("model"=label,"   -High change in budget", 
    quantile(sim.diff.budget, c(0.25, 0.5, 0.75)))
  plot.temp[3,]<-c("model"=label,"   -High average officers", 
    quantile(sim.mean.officers, c(0.25, 0.5, 0.75)))
  plot.temp[4,]<-c("model"=label,"   -High average budget", 
    quantile(sim.mean.budgets, c(0.25, 0.5, 0.75)))

  return(plot.temp)
}

plot.dat<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.all, label="Full pop.", diff.var="diff.arrest.all", mean.var="mean.arrest.all")

plot.dat<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.all, label="Full pop.", diff.var="diff.arrest.all", mean.var="mean.arrest.all")

plot.dat<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.all, label="Full pop.", diff.var="diff.arrest.all", mean.var="mean.arrest.all")

plot.dat<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.all, label="Full pop.", diff.var="diff.arrest.all", mean.var="mean.arrest.all")

plot.dat<-make.plot.dat.arrest(data=dat.imp$imputations[[1]],
                    model=b.all.all, label="Full pop.", diff.var="diff.arrest.all", mean.var="mean.arrest.all")

### DROP Native American for this analysis, exclusion of rural has huge effect on results

plot.dat$order<-ifelse(plot.dat$model=="Full pop.", 1, ifelse(plot.dat$model=="African American", 2,
                #ifelse(plot.dat$model=="Native American", 3, 
                ifelse(plot.dat$model=="White", 3,
                ifelse(plot.dat$model=="Men", 4, ifelse(plot.dat$model=="Women", 5, NA)))))

plot.dat<-plot.dat%>%arrange((order))
plot.dat<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.dat)
index<-grep("High change in arrest", plot.dat$name)
plot.dat<-rbind(plot.dat[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
                plot.dat[index[2]:nrow(plot.dat),]))
# index<-grep("Median", plot.dat$name)
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
#plot.dat$offense<-"all"
plot.dat$yval<-seq(1:nrow(plot.dat))

# ###############################
# ## Violent
# ###############################
# plot.temp<-make.plot.dat.arrest(data=within.dat%>%filter(offense=="viol")%>%filter(race=="all")%>%filter(gender=="all"),
#                         model=b.all.viol, label="Full pop.")
# 
# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat.arrest(within.dat%>%filter(offense=="viol")%>%filter(gender=="male"),
#                                   model=b.men.viol, label="Men"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat.arrest(within.dat%>%filter(offense=="viol")%>%filter(gender=="female"),
#                                   model=b.women.viol, label="Women"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat.arrest(within.dat%>%filter(offense=="viol")%>%filter(race=="wht"),
#                                   model=b.white.viol, label="White"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                     make.plot.dat.arrest(within.dat%>%filter(offense=="viol")%>%filter(race=="blk"),
#                                   model=b.black.viol, label="African American"))
# 
# plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
#                                       #ifelse(plot.temp$model=="Native American", 3, 
#                                       ifelse(plot.temp$model=="White", 4,
#                                         ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA)))))
# 
# plot.temp<-plot.temp%>%arrange((order))
# plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
#                                                  plot.temp[index[2]:nrow(plot.temp),]))
# 
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
#                                                  plot.temp[index[3]:nrow(plot.temp),]))
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
#                                                  plot.temp[index[4]:nrow(plot.temp),]))
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
#                                                  plot.temp[index[5]:nrow(plot.temp),]))
# plot.temp$offense<-"viol"
# 
# plot.temp$yval<-seq(1:nrow(plot.temp))
# 
# plot.dat<-rbind(plot.dat, plot.temp)
# 
# 
# #############################
# ## Drug
# #############################
# plot.temp<-make.plot.dat.arrest(data=within.dat%>%filter(offense=="drug")%>%filter(race=="all")%>%filter(gender=="all"),
#                          model=b.all.drug, label="Full pop.")
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="drug")%>%filter(gender=="male"),
#                                    model=b.men.drug, label="Men"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="drug")%>%filter(gender=="female"),
#                                    model=b.women.drug, label="Women"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="drug")%>%filter(race=="wht"),
#                                    model=b.white.drug, label="White"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="drug")%>%filter(race=="blk"),
#                                    model=b.black.drug, label="African American"))
# 
# plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
#                                                                 #ifelse(plot.temp$model=="Native American", 3, 
#                                                                 ifelse(plot.temp$model=="White", 4,
#                                                                        ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA)))))
# plot.temp<-plot.temp%>%arrange((order))
# plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
#                                                    plot.temp[index[2]:nrow(plot.temp),]))
# 
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
#                                                    plot.temp[index[3]:nrow(plot.temp),]))
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
#                                                    plot.temp[index[4]:nrow(plot.temp),]))
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
#                                                    plot.temp[index[5]:nrow(plot.temp),]))
# plot.temp$offense<-"drug"
# 
# plot.temp$yval<-seq(1:nrow(plot.temp))
# 
# plot.dat<-rbind(plot.dat, plot.temp)
# 
# #############################
# ## QoL
# #############################
# 
# plot.temp<-make.plot.dat.arrest(data=within.dat%>%filter(offense=="qol")%>%filter(race=="all")%>%filter(gender=="all"),
#                          model=b.all.qol, label="Full pop.")
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="qol")%>%filter(gender=="male"),
#                                    model=b.men.qol, label="Men"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="qol")%>%filter(gender=="female"),
#                                    model=b.women.qol, label="Women"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="qol")%>%filter(race=="wht"),
#                                    model=b.white.qol, label="White"))
# 
# plot.temp<-bind_rows(plot.temp, 
#                      make.plot.dat.arrest(within.dat%>%filter(offense=="qol")%>%filter(race=="blk"),
#                                    model=b.black.qol, label="African American"))
# 
# plot.temp$order<-ifelse(plot.temp$model=="Full pop.", 1, ifelse(plot.temp$model=="African American", 2,
#                                                                 #ifelse(plot.temp$model=="Native American", 3, 
#                                                                 ifelse(plot.temp$model=="White", 4,
#                                                                        ifelse(plot.temp$model=="Men", 5, ifelse(plot.temp$model=="Women", 6, NA)))))
# plot.temp<-plot.temp%>%arrange((order))
# plot.temp<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.temp)
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
#                                                    plot.temp[index[2]:nrow(plot.temp),]))
# 
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
#                                                    plot.temp[index[3]:nrow(plot.temp),]))
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[4]-1),], rbind(c(NA, "Men", NA, NA, NA, NA),  
#                                                    plot.temp[index[4]:nrow(plot.temp),]))
# index<-grep("High change in arrest", plot.temp$name)
# plot.temp<-rbind(plot.temp[1:(index[5]-1),], rbind(c(NA, "Women", NA, NA, NA, NA),  
#                                                    plot.temp[index[5]:nrow(plot.temp),]))
# plot.temp$offense<-"qol"
# 
# plot.temp$yval<-seq(1:nrow(plot.temp))
# 
# plot.dat<-rbind(plot.dat, plot.temp)
# 
# plot.dat$lower<-as.numeric(plot.dat$lower); plot.dat$median<-as.numeric(plot.dat$median); plot.dat$upper<-as.numeric(plot.dat$upper)

#############################
## Plot
#############################

# plot.dat$offense<-ifelse(plot.dat$offense=="all", "All",
#                          ifelse(plot.dat$offense=="viol", "Violent",
#                                 ifelse(plot.dat$offense=="drug", "Drug", 
#                                        ifelse(plot.dat$offense=="qol", "Quality of Life", NA))))



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
  xlab("Change in police maltreatment reports, 50 percent credible interval")+
  scale_y_reverse()+
  geom_vline(xintercept=0, linetype=2)#+
  #facet_wrap(~offense)

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
  scale_y_reverse()#+
  #facet_wrap(~offense, ncol=1)

pdf("within-predict-king.pdf", width=7, height=8)
grid.draw(gridExtra::cbind.gtable(ggplotGrob(table_plot), ggplotGrob(forest),size="last"))
dev.off()


# forest<-ggplot(data=plot.dat%>%filter(offense=="All"))+
#   theme_bw()+
#   aes(x=median, xmin=lower, xmax=upper, y=yval)+
#   geom_point()+
#   geom_errorbarh(height=0.2)+
#   theme(
#     axis.text.y = element_blank(),
#     axis.title.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.grid.major.y = element_blank(), 
#     panel.grid.minor.y = element_blank(), 
#     panel.border = element_blank() 
#   )+
#   xlab("Predicted reports by police, 50 percent credible interval")+
#   scale_y_reverse()+
#   geom_vline(xintercept=0, linetype=2)+
#   facet_wrap(~offense)
# 
# table_plot<-ggplot(plot.dat%>%filter(offense==("All")))+
#   theme_bw() + 
#   aes(y=yval)+
#   #geom_text(aes(label=gsub("\\s2", "", model), x=0), hjust=0)+
#   geom_text(aes(label=name, x=0), hjust=0)+
#   theme(
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank(),
#     panel.border = element_blank(),
#     strip.background = element_blank(),
#     strip.text.x = element_blank()
#   )+xlim(0,6)+
#   scale_y_reverse()+
#   facet_wrap(~offense, ncol=1)
# pdf("within-predict-only-all-king.pdf", width=7, height=8)
# grid.draw(gridExtra::cbind.gtable(ggplotGrob(table_plot), ggplotGrob(forest),size="last"))
# dev.off()

# sink("arrest-plot-out.txt")
# plot.dat
# print(quantile(within.dat$diff.arrest.rt, c(0.1, 0.5 ,0.9), na.rm=TRUE))
# print(quantile(within.dat$mean.arrest.rt, c(0.1, 0.5 ,0.9), na.rm=TRUE))
# sink()

###########################################
# For officers, budgets
########################################3
plot.dat<-make.plot.dat.officer(data=dat.imp$imputations[[1]],
                               model=b.all.all, label="Full pop.")

# plot.dat<-bind_rows(plot.dat, 
#                     make.plot.dat.officer(within.dat%>%filter(offense=="all")%>%filter(race=="wht"),
#                                          model=b.all.white, label="White"))
# 
# plot.dat<-bind_rows(plot.dat, 
#                     make.plot.dat.officer(within.dat%>%filter(offense=="all")%>%filter(race=="blk"),
#                                          model=b.all.black, label="African American"))
# 
# plot.dat$order<-ifelse(plot.dat$model=="Full pop.", 1, ifelse(plot.dat$model=="African American", 2,
#                         ifelse(plot.dat$model=="White", 3, NA)))
                                                                     

#plot.dat<-plot.dat%>%arrange((order))
#plot.dat<-rbind(c(NA,"Full pop.",NA, NA, NA, NA), plot.dat)
#index<-grep("High change in officers", plot.dat$name)
# plot.dat<-rbind(plot.dat[1:(index[2]-1),], rbind(c(NA, "African American", NA, NA, NA, NA),  
#                                                  plot.dat[index[2]:nrow(plot.dat),]))
# # index<-grep("High change in officers", plot.dat$name)
# # plot.dat<-rbind(plot.dat[1:(index[3]-1),], rbind(c(NA, "Native American", NA, NA, NA, NA),  
# #                                                  plot.dat[index[3]:nrow(plot.dat),]))
# index<-grep("High change in officers", plot.dat$name)
# plot.dat<-rbind(plot.dat[1:(index[3]-1),], rbind(c(NA, "White", NA, NA, NA, NA),  
#                                                  plot.dat[index[3]:nrow(plot.dat),]))
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
    panel.border = element_blank() 
  )+
  xlab("Predicted reports by police, 50 percent credible interval")+
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
pdf("within-predict-officer-king.pdf", width=7, height=8)
grid.draw(gridExtra::cbind.gtable(ggplotGrob(table_plot), ggplotGrob(forest),size="last"))
dev.off()

sink("officer-plot-out.txt")
plot.dat
print(quantile(within.dat$diff.officers.pc, c(0.1, 0.5 ,0.9), na.rm=TRUE))
print(quantile(within.dat$mean.officers.pc, c(0.1, 0.5 ,0.9), na.rm=TRUE))
print(quantile(within.dat$diff.pol.infl.pc, c(0.1, 0.5 ,0.9), na.rm=TRUE))
print(quantile(within.dat$mean.pol.infl.pc, c(0.1, 0.5 ,0.9), na.rm=TRUE))
sink()
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

##### for tables

makeMedianInt<-function(m1, m2, m3, m4){
  row1<-c(NA, "Difference" ,round(m1$coefficients[which(names(m1$coefficients)=="scale(diff.arrest.rt)")],2),
    round(m2$coefficients[which(names(m2$coefficients)=="scale(diff.arrest.rt)")],2),
    round(m3$coefficients[which(names(m3$coefficients)=="scale(diff.arrest.rt)")],2),
    round(m4$coefficients[which(names(m4$coefficients)=="scale(diff.arrest.rt)")],2))
  row2<-c(rep(NA,6))
  interval<-round(posterior_interval(m1, prob=0.95)[which(names(m1$coefficients)=="scale(diff.arrest.rt)"),],2)
  row2[3]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m2, prob=0.95)[which(names(m2$coefficients)=="scale(diff.arrest.rt)"),],2)
  row2[4]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m3, prob=0.95)[which(names(m3$coefficients)=="scale(diff.arrest.rt)"),],2)
  row2[5]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m4, prob=0.95)[which(names(m4$coefficients)=="scale(diff.arrest.rt)"),],2)
  row2[6]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  row3<-c(NA, "Mean" ,round(m1$coefficients[which(names(m1$coefficients)=="scale(mean.arrest.rt)")],2),
          round(m2$coefficients[which(names(m2$coefficients)=="scale(mean.arrest.rt)")],2),
          round(m3$coefficients[which(names(m3$coefficients)=="scale(mean.arrest.rt)")],2),
          round(m4$coefficients[which(names(m4$coefficients)=="scale(mean.arrest.rt)")],2))
  row4<-c(rep(NA,6))
  interval<-round(posterior_interval(m1, prob=0.95)[which(names(m1$coefficients)=="scale(mean.arrest.rt)"),],2)
  row4[3]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m2, prob=0.95)[which(names(m2$coefficients)=="scale(mean.arrest.rt)"),],2)
  row4[4]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m3, prob=0.95)[which(names(m3$coefficients)=="scale(mean.arrest.rt)"),],2)
  row4[5]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m4, prob=0.95)[which(names(m4$coefficients)=="scale(mean.arrest.rt)"),],2)
  row4[6]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  out<-rbind(row1, row2, row3, row4)
  return(out)
}


makeMedianOfficer<-function(m1){
  row1<-c(NA, "Difference" ,round(m1$coefficients[which(names(m1$coefficients)=="scale(diff.officers.pc)")],2),
          round(m1$coefficients[which(names(m1$coefficients)=="scale(diff.pol.infl.pc)")],2))  
  row2<-c(rep(NA,4))
  interval<-round(posterior_interval(m1, prob=0.95)[which(names(m1$coefficients)=="scale(diff.officers.pc)"),],2)
  row2[3]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m1, prob=0.95)[which(names(m1$coefficients)=="scale(diff.pol.infl.pc)"),],2)
  row2[4]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  row3<-c(NA, "Mean" ,round(m1$coefficients[which(names(m1$coefficients)=="scale(mean.officers.pc)")],2),
          round(m1$coefficients[which(names(m1$coefficients)=="scale(mean.pol.infl.pc)")],2))
  row4<-c(rep(NA,4))
  interval<-round(posterior_interval(m1, prob=0.95)[which(names(m1$coefficients)=="scale(mean.officers.pc)"),],2)
  row4[3]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  interval<-round(posterior_interval(m1, prob=0.95)[which(names(m1$coefficients)=="scale(mean.pol.infl.pc)"),],2)
  row4[4]<-paste("[",interval[1], ", ", interval[2], "]", sep="")
  out<-rbind(row1, row2, row3, row4)
  return(out)
}


reg.out.arrest<-c("{\\textit{All}}", rep(NA, 5))
reg.out.arrest<-rbind(reg.out.arrest, makeMedianInt(b.all.all, b.all.viol, b.all.drug, b.all.qol))
reg.out.arrest<-rbind(reg.out.arrest, c("{\\textit{Women}}", rep(NA, 5)))
reg.out.arrest<-rbind(reg.out.arrest, makeMedianInt(b.women.all, b.women.viol, b.women.drug, b.women.qol))
reg.out.arrest<-rbind(reg.out.arrest, c("{\\textit{Men}}", rep(NA, 5)))
reg.out.arrest<-rbind(reg.out.arrest, makeMedianInt(b.men.all, b.men.viol, b.men.drug, b.men.qol))
reg.out.arrest<-rbind(reg.out.arrest, c("{\\textit{African American}}", rep(NA, 5)))
reg.out.arrest<-rbind(reg.out.arrest, makeMedianInt(b.all.black, b.black.viol, b.black.drug, b.black.qol))
reg.out.arrest<-rbind(reg.out.arrest, c("{\\textit{Native American}}", rep(NA, 5)))
reg.out.arrest<-rbind(reg.out.arrest, makeMedianInt(b.all.amind, b.amind.viol, b.amind.drug, b.amind.qol))
reg.out.arrest<-rbind(reg.out.arrest, c("{\\textit{White}}", rep(NA, 5)))
reg.out.arrest<-rbind(reg.out.arrest, makeMedianInt(b.all.white, b.white.viol, b.white.drug, b.white.qol))

colnames(reg.out.arrest)<-c(" ", " ", "All", "Violent", "Drug", "Quality of Life")

library(xtable)
arrest.table<-xtable(reg.out.arrest, 
                     caption="Regression results for arrests and police reports of child abuse and neglect by offense, race, and gender. Within and between county parameter estimates and 95 percent posterior credible intervals.",
                     align="llrcccc")
print(arrest.table, include.rownames=FALSE,
      caption.placement = getOption("xtable.caption.placement", "top"),
      sanitize.text.function=identity,
                                    file="arrest-reg.tex")



reg.out.officers<-c("{\\textit{All}}", rep(NA, 3))
reg.out.officers<-rbind(reg.out.officers, makeMedianOfficer(b.all.all))
reg.out.officers<-rbind(reg.out.officers, c("{\\textit{African American}}", rep(NA, 3)))
reg.out.officers<-rbind(reg.out.officers, makeMedianOfficer(b.all.black))
reg.out.officers<-rbind(reg.out.officers, c("{\\textit{Native American}}", rep(NA, 3)))
reg.out.officers<-rbind(reg.out.officers, makeMedianOfficer(b.all.amind))
reg.out.officers<-rbind(reg.out.officers, c("{\\textit{White}}", rep(NA, 3)))
reg.out.officers<-rbind(reg.out.officers, makeMedianOfficer(b.all.white))

colnames(reg.out.officers)<-c(" ", " ", "Officers per capita", "Operating budget per capita (state)")

library(xtable)
officer.table<-xtable(reg.out.officers, 
                     caption="Regression results for police agency characteristics and police reports of child abuse and neglect by race. Within and between county parameter estimates and 95 percent posterior credible intervals.",
                     align="llrcc")
print(officer.table, include.rownames=FALSE,
      caption.placement = getOption("xtable.caption.placement", "top"),
      sanitize.text.function=identity,
      file="officer-reg.tex")

### make regression tables
print(xtable(b.all.all$stan_summary), file="b-all-all.tex")
print(xtable(b.all.viol$stan_summary), file="b-all-viol.tex")
print(xtable(b.all.drug$stan_summary), file="b-all-drug.tex")
print(xtable(b.all.qol$stan_summary), file="b-all-qol.tex")

print(xtable(b.all.black$stan_summary), file="b-black-all.tex")
print(xtable(b.black.viol$stan_summary), file="b-black-viol.tex")
print(xtable(b.black.drug$stan_summary), file="b-black-drug.tex")
print(xtable(b.black.qol$stan_summary), file="b-black-qol.tex")

print(xtable(b.all.white$stan_summary), file="b-white-all.tex")
print(xtable(b.white.viol$stan_summary), file="b-white-viol.tex")
print(xtable(b.white.drug$stan_summary), file="b-white-drug.tex")
print(xtable(b.white.qol$stan_summary), file="b-white-qol.tex")

print(xtable(b.women.all$stan_summary), file="b-women-all.tex")
print(xtable(b.women.viol$stan_summary), file="b-women-viol.tex")
print(xtable(b.women.drug$stan_summary), file="b-women-drug.tex")
print(xtable(b.women.qol$stan_summary), file="b-women-qol.tex")

print(xtable(b.men.all$stan_summary), file="b-men-all.tex")
print(xtable(b.men.viol$stan_summary), file="b-men-viol.tex")
print(xtable(b.men.drug$stan_summary), file="b-men-drug.tex")
print(xtable(b.men.qol$stan_summary), file="b-men-qol.tex")

#### Pull counties from each sub-region

### pull top pop in sample from each census sub-region
### list is: Cook, IL (17031); Harris, TX (48201); Maricopa, Az(04013); San Diego, CA (06073); Kings, NY (36047); 
### Broward, FL (12011); Hennepin, MN (27053); 
### Fairfield, CT (09011); Shelby, TN (47157)
### However, not all counties here have x race, x gender
### pull top pop counties from 2012, get different states
sample.dat<-within.dat%>%filter(year==2012)%>%filter(offense=="all")%>%filter(race=="all")%>%filter(gender=="all")
pop.sample<-sample.dat[order(sample.dat$child.pop, decreasing = TRUE),]


#################SHIT - SOME DUPLICATE COUNTIES - LA and other have >1 entry for this
##########################################################
#################THE LONG FORMAT REALLY ISN'T WORTH IT. COULD RESHAPE LATER. JUST GET IT DONE NOW
#########################################################

which(as.data.frame(table(pop.sample$FIPS))>1)


c.index<-c("17031", "48201", "4013", "6073", "36047", "12011", "27053", "9011", "47157")
county.samp<-dat%>%filter(FIPS%in%c.index)
county.samp[is.na(county.samp)]<-0

plot.temp<-county.samp%>%filter(race=='all', gender=="all", offense=="all")

ts.check<-ggplot(plot.temp)+aes(x=year, y=arrest.rt*100000)+geom_line()+facet_wrap(~county)
### LOOKING AT THE TS, THERE'S FULL ZEROES IN BROWARD, FL, COOK
### THE UCR DATA IS REALLY BAD
### NEED TO DO IMPUTATION, MAY AS WELL DO FOR ALL

### predict a new year for these counties using 2011 data (all cases in), drop n_obs term
county.newdat<-county.samp%>%filter(year==2011)%>%filter(offense=="all")
county.newdat$n_obs<-rnorm(nrow(county.newdat), 1, 1)

all.c<-posterior_predict(b.all.all, county.newdat%>%filter(gender=="all")%>%filter(race=="all"))
blk.c<-posterior_predict(b.all.black, county.newdat%>%filter(race=="blk"))
wht.c<-posterior_predict(b.all.black, county.newdat%>%filter(race=="wht"))
men.c<-posterior_predict(b.all.black, county.newdat%>%filter(gender=="male"))
women.c<-posterior_predict(b.all.black, county.newdat%>%filter(gender=="female"))