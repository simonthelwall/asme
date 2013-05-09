all.expanded <- survSplit(all, cut=c(12021,12142,12386,12508,12752,12873), 
                        start="doe2", end="dexit", event="case", id="id", 
                        episode="season", zero="11688" )
head(all.expanded)
all.expanded$dar <- all.expanded$dexit-all.expanded$doe2
all.expanded$season2[all.expanded$season==0] <- "Not winter"
all.expanded$season2[all.expanded$season==1] <- "Winter"
all.expanded$season2[all.expanded$season==2] <- "Not winter"
all.expanded$season2[all.expanded$season==3] <- "Winter"
all.expanded$season2[all.expanded$season==4] <- "Not winter"
all.expanded$season2[all.expanded$season==5] <- "Winter"
all.expanded$season2[all.expanded$season==6] <- "Not winter"
trend <- ddply(all.expanded, .(season), summarise, cases=sum(case), tdar = sum(dar))
trend