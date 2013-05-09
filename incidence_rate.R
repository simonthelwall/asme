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
trend$tyar <- trend$tdar/365.25
trend$rate<-rate_per_k(trend$cases, trend$tyar)
trend$rr <- NA
trend$rr[trend$season==1] <- trend$rate[trend$season==1]/trend$rate[trend$season==0]
trend$rr[trend$season==2] <- trend$rate[trend$season==2]/trend$rate[trend$season==0]
trend$rr[trend$season==3] <- trend$rate[trend$season==3]/trend$rate[trend$season==0]
trend$rr[trend$season==4] <- trend$rate[trend$season==4]/trend$rate[trend$season==0]
trend$rr[trend$season==5] <- trend$rate[trend$season==5]/trend$rate[trend$season==0]
trend$rr[trend$season==6] <- trend$rate[trend$season==6]/trend$rate[trend$season==0]
trend$se <- NA
trend$se[trend$season==1] <- se_log_rr(trend$cases[trend$season==1],trend$cases[trend$season==0])
trend$se[trend$season==2] <- se_log_rr(trend$cases[trend$season==2],trend$cases[trend$season==0])
trend$se[trend$season==3] <- se_log_rr(trend$cases[trend$season==3],trend$cases[trend$season==0])
trend$se[trend$season==4] <- se_log_rr(trend$cases[trend$season==4],trend$cases[trend$season==0])
trend$se[trend$season==5] <- se_log_rr(trend$cases[trend$season==5],trend$cases[trend$season==0])
trend$se[trend$season==6] <- se_log_rr(trend$cases[trend$season==6],trend$cases[trend$season==0])
trend$lci <- trend$rr/exp(1.96*trend$se)
trend$uci <- trend$rr*exp(1.96*trend$se)
trend$z <- z_rr(trend$rr, trend$se)
trend$p <- round(p_rr(trend$z),4)
trend$se <- NULL
trend$z <- NULL
trend$rr <- paste(round(trend$rr,2), " (", round(trend$lci ,2), " - ", round(trend$uci ,2), ")", sep="")
trend$lci <- NULL
trend$uci <- NULL
trend$tdar <- NULL
trend$rate <- round(trend$rate ,2)
trend$tyar <- round(trend$tyar, 2)
trend$rr[trend$season==0] <- ""
trend$p[trend$season==0] <- ""
trend$s <- ""
trend$s[trend$season==0] <- "Apr - Nov 2002"
trend$s[trend$season==1] <- "Dec 2002 - Mar 2003"
trend$s[trend$season==2] <- "Apr - Nov 2003"
trend$s[trend$season==3] <- "Dec 2003 - Mar 2004"
trend$s[trend$season==4] <- "Apr - Nov 2004"
trend$s[trend$season==5] <- "Dec 2004 - Mar 2005"
trend$s[trend$season==6] <- "Apr 2005 - Jul 2005"
trend$season <- NULL
trend <- trend[c(6,1,2,3,4,5)]
names(trend) <- c("Period", "n cases", "\\shortstack{Person-years \\\\at risk}",
                  "\\shortstack{Rate per 1000 \\\\person years at risk}", "Rate raio", "p")
t <- xtable(trend, 
            caption = "Secular trends in incidence rate of rotaviral diarrhoea, India 2002 to 2005",
            label = "trend")
digits(t)[3] <- 0
rm(all.expanded)
rm(trend)