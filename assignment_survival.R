library(foreign)
library(survival)
library(plyr)
library(ggplot2)
library(epicalc)
#"2002-04-07" min
#"2005-08-05" max
setwd("/home/simon/Documents/MSc/Modules/ASME/asme")
#all <- read.dta("/home/simon/Documents/MSc_modules/asme/ASMEdata/allrv.dta")
all <- read.dta("/home/simon/Documents/MSc/Modules/ASME/allrv.dta")
all$male <- 0
all$male[all$sex=="M"] <- 1
head(all)
all$case <- 0
all$case[all$exittype=="RV diarrhoea"] <- 1
first<-read.dta("/home/simon/Documents/MSc/Modules/ASME/firstrv.dta")
#first<-read.dta("/home/simon/Documents/MSc_modules/asme/ASMEdata/firstrv.dta")
first$case <- 0
first$case[first$exittype=="RV diarrhoea"] <- 1

first$doe2 <- as.numeric(first$doe)
first$dexit <-as.numeric(first$exitdate)
first$futime <- first$dexit - first$doe2

# Does recoding mother's education make much of a difference?
chisq.test(first$edumoth,first$case)
first$ed2 <- 0
first$ed2[first$edumoth=="None"] <- 1
table(first$ed2,first$case, dnn=c("ed2", "case"))
103/(205+103)
57/(82+57)
chisq.test(first$ed2,first$case)

m1 <- survreg(Surv(first$futime, first$case) ~ beediwork, 
              first, dist="exponential")
extractCI <- function(m){
  beta <- round(coef(m)["coef"], 3)
  se <- round(coef(m)["se(coef)"], 3)
  p <- round(coef(m)["Pr(>|z|)"], 3)
  ci <- round(m$conf.int[c("lower .95", "upper .95")],3)
  res <- rbind(beta, round(exp(beta),3), ci[1], ci[2], p)
  res
}
#extractCI(m1)

srOrWrapper <- function(x){
  cbind(HR=round(1/exp(coef(x)), 2),
        se=round(summary(x)$table[,2], 2), 
        lci=round((1/exp(coef(x)))/exp(1.96*summary(x)$table[,2]), 2),
        uci=round((1/exp(coef(x)))*exp(1.96*summary(x)$table[,2]), 2),
        p=round(summary(x)$table[,4], 4))
}

lrtestSurv <- function(x,y){
  logLx <- x$loglik[2]
  logLy <- y$loglik[2]
  df <- abs(y$df-x$df)
  chi2 <- abs(2*(logLx - logLy))
  p <- pchisq(chi2, df, lower.tail=FALSE)
  result <- list(chi2=chi2, df=df, p=p)
  return(result)
}

summary(month.m)
month.m$loglik[2]

# beta beedi=-0.409, se=0.158
# hr <- exp(-0.409)
# ef <- exp(1.96*0.158)
# lci <- hr/ef
# uci <- hr*ef
# paste(round(hr, 3), " (", round(lci, 3), " - ", round(uci, 3), ")", sep="")
#2 hr:2.587618 se:.8068068 ci: 1.404428 4.76761
2.587618/exp(1.96*0.8068068) # no
2.587618-exp((1.96*0.8068068)) #no
2.587618/(1.96*exp(0.8068068)) # no
log(exp(2.587618)*exp(1.96*0.8068068)) # sort of
log(exp(2.587618)/exp(1.96*0.8068068)) # sort of
exp(log(2.587618)*(1.96*0.8068068)) #close
exp(log(2.587618)/(1.96*0.8068068)) #close
exp(log(2.587618)-(1.96*0.8068068)) #no

first$age.entry <- round(as.numeric(first$doe - first$dob, format="days")/28, 2)
range(first$age.entry)
first$age.exit <- round(as.numeric(first$exitdate - first$dob, format="days")/28, 2)
range(first$age.exit)
f.expanded <-survSplit(first, cut = c(2, 4, 8, 12, 16, 24), 
                       end = "age.exit", event = "case",
                       start = "age.entry", zero = "dob", id = "id", episode = "age")
head(f.expanded)
length(f.expanded$id)
f.expanded[f.expanded$id==6,]
# need to code a denominator here. Not sure survSplit is formed correctly, should be numeric dates?. 
#Error in newdata[[end]] : no such index at level 1
duplicated(f.expanded$id[f.expanded$id==6])
f.expanded$obs <- 1
f.expanded$obs[duplicated(f.expanded$id)==TRUE]<-0
head(f.expanded)
f.expanded[f.expanded$id==6,]
sum(f.expanded$obs)
as.numeric(as.Date("01/01/1970", format="%d/%m/%Y")) # 1st jan 1970 is t0
f.expanded$futime2 <- f.expanded$dexit-f.expanded$doe2 # days
age <- ddply(f.expanded, .(age), summarise, pdar=sum(futime2, na.rm=TRUE), cases=sum(case, na.rm=TRUE))
age$pyar <- age$pdar/365.25
age$rate <- age$cases/age$pyar
age$rate <- age$rate*1000
#cut = c(2, 4, 8, 12, 16, 24)
age$age[age$age == 0] <- "0-2 months"
age$age[age$age == 1] <- "2-4 months"
age$age[age$age == 2] <- "4-8 months"
age$age[age$age == 3] <- "8-12 months"
age$age[age$age == 4] <- "12-16 months"
age$age[age$age == 5] <- "16-24 months"
age$age[age$age == 6] <- ">24 months"
age

m2 <- survreg(Surv(f.expanded$futime2, f.expanded$case) ~ beediwork+factor(age), 
              f.expanded, dist="exponential")
summary(m2)
srOrWrapper(m2)

# rate by season ####
head(first)
class(first$doe)
min(first$doe) #"2002-04-07"
max(first$exitdate) #"2005-08-05"
as.numeric(min(first$doe)) #11784
as.numeric(as.Date("01/01/2002", format="%d/%m/%Y"))
# hmm, split into months for first year only. How to resolve?

# April 2002 to Aug 2005 is three years four months.
# This is 36+4 months = 40
year<-c(rep(2002, times=9), rep(2003, times=12), rep(2004, times=12), rep(2005, times=8) )
length(year)
month <- c(seq(4,12,1), seq(1,12,1), seq(1,12,1), seq(1,8,1))
length(month)
cal <- as.data.frame(cbind(year, month))
cal$day <- "01"
cal$date <- paste(cal$day, "/", month, "/", year, sep="")
cal$date <- as.Date(cal$date, format="%d/%m/%Y")
cal$date2 <- as.numeric(cal$date)
head(cal)
f.expanded2 <- survSplit(first, cut=cal$date2, 
                         start="doe2", end="dexit", event="case", id="id", 
                         episode="month", zero="11688" )
head(f.expanded2)
levels(factor(f.expanded2$month))
f.expanded2$futime <- NULL
f.expanded2$tdar <- f.expanded2$dexit-f.expanded2$doe2
f.expanded2$tyar <- f.expanded2$tdar/365.25
f.expanded2[f.expanded2$id==6,]
first[first$id==6,]

period <- ddply(f.expanded2, .(month), summarise, 
                cases=sum(case, na.rm=TRUE), tyar = sum(tyar, na.rm=TRUE))
period$rate <- round((period$cases/period$tyar)*1000,2)
period
cal$p <- seq(1,41,1)
cal2 <- subset(cal, select=c(month, p))
names(cal2)<-c("m","month")
period<-merge(period, cal2, by.x="month", by.y="month",all.x=TRUE)
length(period$month)
head(period)
qplot(m, cases, data=period, geom="bar", stat="identity") + scale_x_discrete()
p <- ddply(period, .(m), summarise, cases=sum(cases), tyar=sum(tyar))
p$rate <- round((p$cases/p$tyar)*1000,2)
p
qplot(m, rate, data=p, geom="bar", stat="identity") + scale_x_discrete()

head(first)
cases <- subset(first, case==1)
class(cases$exitdate)
cases$month <- as.numeric(format(cases$exitdate, "%m"))
head(cases)
cases2 <- ddply(cases, .(month), summarise, cases=sum(case))
head(cases2)
mean(cases2$cases)
qplot(month, cases, data=cases2, geom="bar", stat="identity") + scale_x_discrete()
# Splitting winter vs non-winter
# want to split at start dec, and end march
#"2002-04-07" min
#"2005-08-05" max
as.numeric(as.Date("2002-11-30", format="%Y-%m-%d")) #12021
as.numeric(as.Date("2003-03-31", format="%Y-%m-%d")) #12142
as.numeric(as.Date("2003-11-30", format="%Y-%m-%d")) #12386
as.numeric(as.Date("2004-03-31", format="%Y-%m-%d")) #12508
as.numeric(as.Date("2004-11-30", format="%Y-%m-%d")) #12752
as.numeric(as.Date("2005-03-31", format="%Y-%m-%d")) #12873

f.expanded3 <- survSplit(first, cut=c(12021,12142,12386,12508,12752,12873), 
                         start="doe2", end="dexit", event="case", id="id", 
                         episode="season", zero="11688" )
head(f.expanded3)
levels(factor(f.expanded3$season))
f.expanded3$futime <- NULL
f.expanded3$tdar <- f.expanded3$dexit-f.expanded3$doe2
f.expanded3$tyar <- f.expanded3$tdar/365.25
f.expanded3[f.expanded3$id==6,]
first[first$id==6,]

period2 <- ddply(f.expanded3, .(season), summarise, 
                cases=sum(case, na.rm=TRUE), tyar = sum(tyar, na.rm=TRUE))
period2$rate <- round((period2$cases/period2$tyar)*1000,2)
period2$season2<-""
period2$season2[period2$season==0] <- "Not winter"
period2$season2[period2$season==1] <- "Winter"
period2$season2[period2$season==2] <- "Not winter"
period2$season2[period2$season==3] <- "Winter"
period2$season2[period2$season==4] <- "Not winter"
period2$season2[period2$season==5] <- "Winter"
period2$season2[period2$season==6] <- "Not winter"
period2
p2 <- ddply(period2, .(season2), summarise, cases=sum(cases), tyar=sum(tyar))
p2$rate <- round((p2$cases/p2$tyar)*1000,2)
p2
# regression 
range(f.expanded3$season)
f.expanded3$season2[f.expanded3$season==0] <- "Not winter"
f.expanded3$season2[f.expanded3$season==1] <- "Winter"
f.expanded3$season2[f.expanded3$season==2] <- "Not winter"
f.expanded3$season2[f.expanded3$season==3] <- "Winter"
f.expanded3$season2[f.expanded3$season==4] <- "Not winter"
f.expanded3$season2[f.expanded3$season==5] <- "Winter"
f.expanded3$season2[f.expanded3$season==6] <- "Not winter"
season.m <- survreg(Surv(f.expanded3$tyar,f.expanded3$case)~factor(season2), data=f.expanded3, dist="exponential")
season.ci<-srOrWrapper(season.m)
season.ci
# compare with monthly model
head(f.expanded2)
levels(factor(f.expanded2$month))
cal
cal2
length(f.expanded2$case)
f.expanded2 <- merge(f.expanded2, cal2, by.x="month", by.y="month", all.x=TRUE)
length(f.expanded2$case)
month.m <- survreg(Surv(f.expanded2$tyar,f.expanded2$case)~factor(m), data=f.expanded2, dist="exponential")
month.ci <- srOrWrapper(month.m)
month.ci # difficult to interpret. Everything is protective vs January? could make sense. 
#What if set baseline to low risk month, then order factor? Would be easier to interpret.
lrtestSurv(month.m, season.m)