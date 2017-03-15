#load data from models

### extract coefficiencts from models

### p2.all

p2.vis<-list()
p2.vis[["ranef"]]<-ranef(p2.all)
p2.vis[["fixef"]]<-fixef(p2.all)