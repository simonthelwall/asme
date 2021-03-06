library(foreign)
library(survival)
library(plyr)
library(ggplot2)
#all <- read.dta("E:\\My Documents B\\MSc stuff\\SME\\asme\\allrv.dta")
all <- read.dta("/home/simon/Documents/MSc_modules/asme/ASMEdata/allrv.dta")

all$male <- 0
all$male[all$sex=="M"] <- 1
head(all)
all$case <- 0
all$case[all$exittype=="RV diarrhoea"] <- 1
first<-read.dta("/home/simon/Documents/MSc_modules/asme/ASMEdata/firstrv.dta")
first$case <- 0
first$case[first$exittype=="RV diarrhoea"] <- 1

# Does recoding mother's education make much of a difference?
chisq.test(first$edumoth,first$case)
first$ed2 <- 0
first$ed2[first$edumoth=="None"] <- 1
table(first$ed2,first$case, dnn=c("ed2", "case"))
103/(205+103)
57/(82+57)
chisq.test(first$ed2,first$case)

first$doe2 <- as.numeric(first$doe)
first$dexit <-as.numeric(first$exitdate)
first$futime <- first$dexit - first$doe2

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
extractCI(m1)

# beta beedi=-0.409, se=0.158
hr <- exp(-0.409)
ef <- exp(1.96*0.158)
lci <- hr/ef
uci <- hr*ef
paste(round(hr, 3), " (", round(lci, 3), " - ", round(uci, 3), ")", sep="")
#2 hr:2.587618 se:.8068068 ci: 1.404428 4.76761
2.587618/exp(1.96*0.8068068) # no
2.587618-exp((1.96*0.8068068)) #no
2.587618/(1.96*exp(0.8068068)) # no
log(exp(2.587618)*exp(1.96*0.8068068)) # sort of
log(exp(2.587618)/exp(1.96*0.8068068)) # sort of
exp(log(2.587618)*(1.96*0.8068068)) #close
exp(log(2.587618)/(1.96*0.8068068)) #close
exp(log(2.587618)-(1.96*0.8068068)) #no
data(ovarian)
summary(survreg(Surv(futime, fustat) ~ rx, ovarian,
        dist="exponential"))

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
rm(cal2)

# by season
head(first)
class(first$doe)
as.numeric(as.Date("30/04/2002", format="%d/%m/%Y")) # 11807
as.numeric(as.Date("30/11/2002", format="%d/%m/%Y")) # 12021
as.numeric(as.Date("30/04/2003", format="%d/%m/%Y")) # 12172
as.numeric(as.Date("30/11/2003", format="%d/%m/%Y")) # 12386
as.numeric(as.Date("30/04/2004", format="%d/%m/%Y")) # 12538
as.numeric(as.Date("30/11/2004", format="%d/%m/%Y")) # 12752
as.numeric(as.Date("30/04/2005", format="%d/%m/%Y")) # 12903
as.numeric(as.Date("30/11/2005", format="%d/%m/%Y")) # 13117
first$dob2 <- as.numeric(first$dob, format="days")
first$firstentry <- as.numeric(min(first$doe))
f.expanded3 <- survSplit(first, 
                         cut=c(11807, 12021, 12172, 12386, 12538, 12572, 12903, 13117), 
                         end="dexit", event="case", start="doe2", id="id", zero="first$entry")
f.expanded3 <- f.expanded3[order(f.expanded3$id),]
f.expanded3$doe2 <- as.numeric(f.expanded3$doe2)
f.expanded3$season <- ""
as.numeric(as.Date("29/06/2004", format="%d/%m/%Y"),format="days") # why .5 days?
f.expanded3$season[f.expanded3$doe2 <= 11807] <- "Winter" # 30/04/2002
f.expanded3$season[f.expanded3$doe2 >= 11807 & f.expanded3$doe2 < 12021] <- "Not winter"
f.expanded3$season[f.expanded3$doe2 >= 12021 & f.expanded3$doe2 < 12172] <- "Winter"
f.expanded3$season[f.expanded3$doe2 >= 12172 & f.expanded3$doe2 < 12386] <- "Not winter"
f.expanded3$season[f.expanded3$doe2 >= 12386 & f.expanded3$doe2 < 12537] <- "Winter"
f.expanded3$season[f.expanded3$doe2 >= 12538 & f.expanded3$doe2 < 12572] <- "Not winter"
f.expanded3$season[f.expanded3$doe2 >= 12572 & f.expanded3$doe2 < 12903] <- "Winter"
f.expanded3$season[f.expanded3$doe2 >= 12903 & f.expanded3$doe2 < 13117] <- "Not winter"
f.expanded3$season[f.expanded3$doe2 >= 13117] <- "Winter"
table(f.expanded3$season)
head(f.expanded3)
f.expanded3[f.expanded3$id==1,c(1,3,4,14,16,17,26)]
f.expanded3$dar <- f.expanded3$dexit-f.expanded3$doe2
season <- ddply(f.expanded3, .(season), summarise, cases=sum(case), tdar=sum(dar))
season$tyar <- season$tdar/365.25
season$rate <- rate_per_k(season$cases,season$tyar)
season 

# temporal trend ####
as.numeric(as.Date("01/01/2003", format="%d/%m/%Y")) # 12053
as.numeric(as.Date("01/01/2004", format="%d/%m/%Y")) # 12418
as.numeric(as.Date("01/01/2005", format="%d/%m/%Y")) # 12784

f.expanded4 <- survSplit(first,
                         cut=c(12053, 12418, 12784), 
                         end="dexit", event="case", start="doe2", id="id", zero="first$entry")
f.expanded4 <- f.expanded4[order(f.expanded4$id)]
head(f.expanded4)
f.expanded4$year <- "2002"
f.expanded4$year[f.expanded4$doe2 >= 12053] <- "2003"
f.expanded4$year[f.expanded4$doe2 >= 12418] <- "2004"
f.expanded4$year[f.expanded4$doe2 >= 12784] <- "2005"
f.expanded4[f.expanded4$id==1,c(1,3,4,14,16,17,26)]
table(f.expanded4$year)
f.expanded4$tdar <- f.expanded4$dexit-f.expanded4$doe2
year <- ddply(f.expanded4, .(year), summarise, n=sum(case), tdar=sum(tdar) )
year$tyar <- year$tdar/365.25
year$rate <- rate_per_k(year$n, year$tyar)
year