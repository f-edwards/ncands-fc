##### motivates selection - provide context - political data? voteshare -

library(ggplot2)
library(RColorBrewer)
library(maps)
library(rgdal)
library(dplyr)
library(mapproj)
library(xtable)

state.mal<-read.csv("R:/Project/NCANDS/Data-archive/data/state-malt.csv")

returnquant<-function(x){
  l<-5 ### number of quantiles
  temp<-cut_number(x,l)
  quant<-rep(NA, length(x))
  for(i in (1:l)){
    z<-which(levels(temp)[i]==temp)
    quant[z]<-i
    }
    return(as.factor(quant))
}

rq<-function(x,l){
  require(gglpot2)
  l<-5 ### number of quantiles
  temp<-cut_number(x,l)
  quant<-rep(NA, length(x))
  for(i in (1:l)){
    z<-which(levels(temp)[i]==temp)
    quant[z]<-i
    }
    return(as.factor(quant))
}

cleanstate<-function(x){
  x$STATE<-ifelse((x$State=="AL"),"Alabama", x$STATE)
  x$STATE<-ifelse((x$State=="AK"),"Alaska", x$STATE)
  x$STATE<-ifelse((x$State=="AZ"),"Arizona", x$STATE)
  x$STATE<-ifelse((x$State=="AR"),"Arkansas", x$STATE)
  x$STATE<-ifelse((x$State=="CA"),"California", x$STATE)
  x$STATE<-ifelse((x$State=="CO"),"Colorado", x$STATE)
  x$STATE<-ifelse((x$State=="CT"),"Connecticut", x$STATE)
  x$STATE<-ifelse((x$State=="DE"),"Delaware", x$STATE)
  x$STATE<-ifelse((x$State=="DC"),"District of Columbia", x$STATE)
  x$STATE<-ifelse((x$State=="FL"),"Florida", x$STATE)
  x$STATE<-ifelse((x$State=="GA"),"Georgia", x$STATE)
  x$STATE<-ifelse((x$State=="HI"),"Hawaii", x$STATE)
  x$STATE<-ifelse((x$State=="ID"),"Idaho", x$STATE)
  x$STATE<-ifelse((x$State=="IL"),"Illinois", x$STATE)
  x$STATE<-ifelse((x$State=="IN"),"Indiana", x$STATE)
  x$STATE<-ifelse((x$State=="IA"),"Iowa", x$STATE)
  x$STATE<-ifelse((x$State=="KS"),"Kansas", x$STATE)
  x$STATE<-ifelse((x$State=="KY"),"Kentucky", x$STATE)
  x$STATE<-ifelse((x$State=="LA"),"Louisiana", x$STATE)
  x$STATE<-ifelse((x$State=="ME"),"Maine", x$STATE)
  x$STATE<-ifelse((x$State=="MD"),"Maryland", x$STATE)
  x$STATE<-ifelse((x$State=="MA"),"Massachusetts", x$STATE)
  x$STATE<-ifelse((x$State=="MI"),"Michigan", x$STATE)
  x$STATE<-ifelse((x$State=="MN"),"Minnesota", x$STATE)
  x$STATE<-ifelse((x$State=="MS"),"Mississippi", x$STATE)
  x$STATE<-ifelse((x$State=="MO"),"Missouri", x$STATE)
  x$STATE<-ifelse((x$State=="MT"),"Montana", x$STATE)
  x$STATE<-ifelse((x$State=="NE"),"Nebraska", x$STATE)
  x$STATE<-ifelse((x$State=="NV"),"Nevada", x$STATE)
  x$STATE<-ifelse((x$State=="NH"),"New Hampshire", x$STATE)
  x$STATE<-ifelse((x$State=="NJ"),"New Jersey", x$STATE)
  x$STATE<-ifelse((x$State=="NM"),"New Mexico", x$STATE)
  x$STATE<-ifelse((x$State=="NY"),"New York", x$STATE)
  x$STATE<-ifelse((x$State=="NC"),"North Carolina", x$STATE)
  x$STATE<-ifelse((x$State=="ND"),"North Dakota", x$STATE)
  x$STATE<-ifelse((x$State=="OH"),"Ohio", x$STATE)
  x$STATE<-ifelse((x$State=="OK"),"Oklahoma", x$STATE)
  x$STATE<-ifelse((x$State=="OR"),"Oregon", x$STATE)
  x$STATE<-ifelse((x$State=="PA"),"Pennsylvania", x$STATE)
  x$STATE<-ifelse((x$State=="RI"),"Rhode Island", x$STATE)
  x$STATE<-ifelse((x$State=="SC"),"South Carolina", x$STATE)
  x$STATE<-ifelse((x$State=="SD"),"South Dakota", x$STATE)
  x$STATE<-ifelse((x$State=="TN"),"Tennessee", x$STATE)
  x$STATE<-ifelse((x$State=="TX"),"Texas", x$STATE)
  x$STATE<-ifelse((x$State=="UT"),"Utah", x$STATE)
  x$STATE<-ifelse((x$State=="VT"),"Vermont", x$STATE)
  x$STATE<-ifelse((x$State=="VA"),"Virginia", x$STATE)
  x$STATE<-ifelse((x$State=="WA"),"Washington", x$STATE)
  x$STATE<-ifelse((x$State=="WV"),"West Virginia", x$STATE)
  x$STATE<-ifelse((x$State=="WI"),"Wisconsin", x$STATE)
  x$STATE<-ifelse((x$State=="WY"),"Wyoming", x$STATE)
  x$STATE<-ifelse((x$State=="PR"), "Puerto Rico", x$STATE)
  x$STATE<-tolower(x$STATE)
  return(x)
}
# 
# county_map <- map_data("county")
# county_map$subregion<-ifelse(county_map$subregion=="de kalb", "dekalb", county_map$subregion)
# county_map$subregion<-ifelse(county_map$subregion=="du page", "dupage", county_map$subregion)
# county_map$subregion<-ifelse(county_map$subregion=="la salle", "lasalle", county_map$subregion)
# 
# cnty.dat$State<-cnty.dat$staterr
# cnty.dat$STATE<-cnty.dat$State
# cnty.dat<-cleanstate(cnty.dat)
# cnty.dat$region<-cnty.dat$STATE
# cnty.dat$subregion<-tolower(cnty.dat$cname)
# cnty.dat$subregion<-as.character(strsplit(cnty.dat$subregion, " county"))
# cnty.dat$subregion<-as.character(strsplit(cnty.dat$subregion, " parish"))
# cnty.dat$subregion<-gsub("[.]", "", cnty.dat$subregion)
# 
# choro<-left_join(county_map, cnty.dat, by=c("subregion", "region"))
# choro <- choro[order(choro$order), ]
# 
# choro$q<-returnquant(choro$tot.rpt/choro$child.pop)
# 
# CntyPlot <- ggplot(choro,
#                        aes(x = long, y = lat, group = group, fill = q)) +
#   geom_polygon(aes(fill = q), colour = "black", size=0.1) +
#   scale_fill_brewer(palette = "Blues", labels=c("Lowest 10%","", "", "", "", "", "", " ", "Highest 10%"),
#                     name=" ", na.value="grey50") +
#   theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#         panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL) +
#   scale_x_continuous(name="", breaks=NULL) +
#   theme(strip.background=element_blank(), 
#         strip.text.x=element_text(size=10),
#         strip.text.y=element_blank())+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))+
#   coord_map(project="albers", at0 = 45.5, lat1 = 29.5)+
#   xlab(NULL) + ylab(NULL)
# 
# ggsave("AllCountiesTotRpt.pdf", CntyPlot, width=7, height=5)

st.11<-state.mal[state.mal$year==2011,]

state_map <- map_data("state")
st.11$State<-st.11$StaTerr
st.11$STATE<-st.11$StaTerr
st.11<-cleanstate(st.11)
st.11$region<-st.11$STATE

choro1<-left_join(state_map, st.11, by="region")
choro1 <- choro1[order(choro1$order), ]

choro1$q<-returnquant(choro1$rpt.cj/choro1$child)


StPlot <- ggplot(choro1,
                   aes(x = long, y = lat, group = group, fill = q)) +
  geom_polygon(aes(fill = q), colour = "black") +
  scale_fill_brewer(palette = "Blues", labels=c("Lowest 10%","", "", "", "", "", "", " ", "Highest 10%"),
                    name=" ", na.value="grey50") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.border = element_blank(), panel.background=element_blank())+
  scale_y_continuous(name="", breaks=NULL) +
  scale_x_continuous(name="", breaks=NULL) +
  theme(strip.background=element_blank(), 
        strip.text.x=element_text(size=10),
        strip.text.y=element_blank())+
  theme(legend.position="bottom")+
  theme(legend.key.size= unit(0.3, "cm"))+
  coord_map(project="albers", at0 = 45.5, lat1 = 29.5)+
  xlab(NULL) + ylab(NULL)

ggsave("StTotRpts.pdf", StPlot, width=7, height=5)