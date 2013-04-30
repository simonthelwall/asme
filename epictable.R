library(survival)
library(foreign)
library(plyr)
# functions ####
perc <- function(d,n){
  round((d/n)*100,2)
}

rate_per_k <- function(x,y){
  (x/y)*1000
}

or_wrapper<-function(x){
  cbind(OR=1/exp(coef(x)),exp(confint(x)))
}

se_log_rr <- function(x,y){
  sqrt((1/x)+(1/y))
}

z_rr <- function(x,y){
  log(x)/y
}

p_rr <- function(x){
  2*pnorm(-abs(x))
}

decimal2<-function(x){paste(x,".00",sep="")} # decimal replacer for character strings, 2 decimal places

decimal1<-function(x){paste(x,"0",sep="")} # decimal replacer for character strings, 1 decimal place

simpleCap<-function(x){
  s<-as.character(x)
  s<-paste(toupper(substring(s,1,1)),substring(s,2),sep="")
  #  s<-as.factor(s)
}

# reading data in ####
#,p_val=round(summary(x)$coefficients[,4],6)
first<-read.dta("/home/simon/Documents/MSc_modules/asme/ASMEdata/firstrv.dta")
# can start with very basics. How many children, how many events. Age and sex. 
names(first)
first$lbw <- as.character(first$lbw)
table(is.na(first$lbw))
all <- read.dta("/home/simon/Documents/MSc_modules/asme/ASMEdata/allrv.dta")
head(all)
length(all$id)
table(all$numprevepisode)
447+160+56+14+4+2
all[all$numprevepisode!=0,]
all[all$id==6,]
first[first$id==6,]

# get max number of episodes by id and time at risk
all$dar <- as.numeric(all$endfoll - all$startfoll,format="days")
head(all$dar)
max.episodes <- ddply(all, .(id), summarise, max=max(numprevepisode), tdar=sum(dar))
head(max.episodes)

# Combine max episodes and tdar into first dataframe
length(first$id)
first <- merge(first, max.episodes, all.x=TRUE, by.x="id", by.y="id")
length(first$id)
head(first)
first$case <- 0
first$case[first$exittype == "RV diarrhoea"] <- 1
table(first$case, first$exittype)

# Boring tables ####

# Sex ####
first$obs <- 1
sex <- ddply(first, .(sex), summarise, 
             n=sum(obs), cases=sum(case), episodes=sum(max), tar=sum(tdar)/365.25)
sex$rate <- rate_per_k(sex$episodes,sex$tar)
sex$rr[sex$sex=="M"] <- sex$rate[sex$sex == "M"] / sex$rate[sex$sex == "F"]
sex$rr[sex$sex=="F"] <- sex$rate[sex$sex == "F"] / sex$rate[sex$sex == "F"]
sex$se <- se_log_rr(sex$episodes[sex$sex=="M"], sex$episodes[sex$sex=="F"])
sex$lci <- sex$rr/exp(1.96*sex$se)
sex$uci <- sex$rr*exp(1.96*sex$se)
sex$z <- z_rr(sex$rr, sex$se)
sex$p <- p_rr(sex$z)
sex

first$male <- 0
first$male[first$sex=="M"] <- 1

# neonatal rv ####
neo <- ddply(first, .(neonatalrv) , summarise, 
             n=sum(obs), cases=sum(case), episodes=sum(max), tar=sum(tdar)/365.25)
neo$rate <- rate_per_k(neo$episodes,neo$tar)
neo$rr[neo$neonatalrv == "No"] <- neo$rate[neo$neonatalrv == "No"] / neo$rate[neo$neonatalrv == "No"]
neo$rr[neo$neonatalrv == "Yes"] <- neo$rate[neo$neonatalrv == "Yes"] / neo$rate[neo$neonatalrv == "No"]
neo$se <- se_log_rr(neo$episodes[neo$neonatalrv=="Yes"], neo$episodes[neo$neonatalrv=="No"])
neo$lci <- neo$rr/exp(1.96*neo$se)
neo$uci <- neo$rr*exp(1.96*neo$se)
neo$z <- z_rr(neo$rr, neo$se)
neo$p <- p_rr(neo$z)
neo
neo$var <-"neo"
names(sex)[1] <- "val"
sex$var <- "sex"
names(neo)[1] <- "val"
t1 <- rbind(sex,neo)
t1$val <- as.character(t1$val)
t1$val[t1$val == "No"] <- "No neonatal rotavirus"
t1$val[t1$val == "Yes"] <- "Neonatal rotavirus"
t1$val[t1$val == "M"] <- "Male"
t1$val[t1$val == "F"] <- "Female"
t1
rm(sex)
rm(neo)

# mother's education ####
edumoth <- ddply(first, .(edumoth) , summarise, 
             n=sum(obs), cases=sum(case), episodes=sum(max), tar=sum(tdar)/365.25)
edumoth$rate <- rate_per_k(edumoth$episodes,edumoth$tar)
edumoth$rr[edumoth$edumoth == "Higher secondary & college"] <- edumoth$rate[edumoth$edumoth == "Higher secondary & college"] / edumoth$rate[edumoth$edumoth == "Higher secondary & college"]
edumoth$rr[edumoth$edumoth == "Primary & middle school"] <- edumoth$rate[edumoth$edumoth == "Primary & middle school"] / edumoth$rate[edumoth$edumoth == "Higher secondary & college"]
edumoth$rr[edumoth$edumoth == "None"] <- edumoth$rate[edumoth$edumoth == "None"] / edumoth$rate[edumoth$edumoth == "Higher secondary & college"]

edumoth$se[edumoth$edumoth=="Primary & middle school"] <- se_log_rr(edumoth$episodes[edumoth$edumoth=="Primary & middle school"], edumoth$episodes[edumoth$edumoth=="Higher secondary & college"])
edumoth$se[edumoth$edumoth=="None"] <- se_log_rr(edumoth$episodes[edumoth$edumoth=="None"], edumoth$episodes[edumoth$edumoth=="Higher secondary & college"])
edumoth$lci <- edumoth$rr/exp(1.96*edumoth$se)
edumoth$uci <- edumoth$rr*exp(1.96*edumoth$se)
edumoth$z <- z_rr(edumoth$rr, edumoth$se)
edumoth$p <- p_rr(edumoth$z)
edumoth

edumoth$edumoth <- as.character(edumoth$edumoth)
names(edumoth)[1] <- "val"
edumoth$val <- "Mother's education"
t1 <-rbind(t1, edumoth)
rm(edumoth)

# household size ####
hhsize <- ddply(first, .(hhsize) , summarise, 
                 n=sum(obs), cases=sum(case), episodes=sum(max), tar=sum(tdar)/365.25)
hhsize$rate <- rate_per_k(hhsize$episodes,hhsize$tar)
hhsize$rr[hhsize$hhsize == "<=5"] <- hhsize$rate[hhsize$hhsize == "<=5"] / hhsize$rate[hhsize$hhsize == "<=5"]
hhsize$rr[hhsize$hhsize == ">5"] <- hhsize$rate[hhsize$hhsize == ">5"] / hhsize$rate[hhsize$hhsize == "<=5"]
hhsize$se <- se_log_rr(hhsize$episodes[hhsize$hhsize==">5"], hhsize$episodes[hhsize$hhsize=="<=5"])
hhsize$lci <- hhsize$rr/exp(1.96*hhsize$se)
hhsize$uci <- hhsize$rr*exp(1.96*hhsize$se)
hhsize$z <- z_rr(hhsize$rr, hhsize$se)
hhsize$p <- p_rr(hhsize$z)
hhsize

names(hhsize)[1] <- "val"
hhsize$var <- "Household size"
hhsize$val <- as.character(hhsize$val)
t1 <- rbind(t1, hhsize)
rm(hhsize)

# socioeconomic status ####
ses <- ddply(first, .(ses) , summarise, 
                n=sum(obs), cases=sum(case), episodes=sum(max), tar=round(sum(tdar)/365.25,2))
ses$rate <- rate_per_k(ses$episodes,ses$tar)
ses$rr[ses$ses == "Class II"] <- ses$rate[ses$ses == "Class II"] / ses$rate[ses$ses == "Class II"]
ses$rr[ses$ses == "Class I"] <- ses$rate[ses$ses == "Class I"] / ses$rate[ses$ses == "Class II"]
ses$se <- se_log_rr(ses$episodes[ses$ses=="Class I"], ses$episodes[ses$ses=="Class II"])
ses$lci <- ses$rr/exp(1.96*ses$se)
ses$uci <- ses$rr*exp(1.96*ses$se)
ses$z <- z_rr(ses$rr, ses$se)
ses$p <- p_rr(ses$z)

names(ses)[1] <- "val"
ses$var <- "Socioeconomic status"
ses$val <- as.character(ses$val)
t1 <-rbind(t1, ses)
rm(ses)

# low birth rate ####
lbw <- ddply(first, .(lbw) , summarise, 
             n=sum(obs), cases=sum(case), episodes=sum(max), tar=round(sum(tdar)/365.25,2))
lbw$lbw[is.na(lbw$lbw)==TRUE]<-"Missing"
lbw$rate <- rate_per_k(lbw$episodes,lbw$tar)
lbw$rr[lbw$lbw == ">=2.5"] <- lbw$rate[lbw$lbw == ">=2.5"] / lbw$rate[lbw$lbw == ">=2.5"]
lbw$rr[lbw$lbw == "<2.5"] <- lbw$rate[lbw$lbw == "<2.5"] / lbw$rate[lbw$lbw == ">=2.5"]
lbw$rr[lbw$lbw == "Missing"] <- lbw$rate[lbw$lbw == "Missing"] / lbw$rate[lbw$lbw == ">=2.5"]
lbw$se[lbw$lbw == "<2.5"] <- se_log_rr(lbw$episodes[lbw$lbw=="<2.5"], lbw$episodes[lbw$lbw==">=2.5"])
lbw$se[lbw$lbw == "Missing"] <- se_log_rr(lbw$episodes[lbw$lbw=="Missing"], lbw$episodes[lbw$lbw==">=2.5"])
lbw$lci <- lbw$rr/exp(1.96*lbw$se)
lbw$uci <- lbw$rr*exp(1.96*lbw$se)
lbw$z <- z_rr(lbw$rr, lbw$se)
lbw$p <- p_rr(lbw$z)

names(lbw)[1] <- "var"
lbw$var <- as.character(lbw$var)
t1 <-rbind(t1, lbw)
rm(lbw)

# animal ownership ####
animalown <- ddply(first, .(animalown) , summarise, 
             n=sum(obs), cases=sum(case), episodes=sum(max), tar=round(sum(tdar)/365.25,2))
animalown$rate <- rate_per_k(animalown$episodes,animalown$tar)
animalown$rr[animalown$animalown == "No"] <- animalown$rate[animalown$animalown == "No"] / animalown$rate[animalown$animalown == "No"]
animalown$rr[animalown$animalown == "Yes"] <- animalown$rate[animalown$animalown == "Yes"] / animalown$rate[animalown$animalown == "No"]
animalown$se <- se_log_rr(animalown$episodes[animalown$animalown=="Yes"], animalown$episodes[animalown$animalown=="No"])
animalown$lci <- animalown$rr/exp(1.96*animalown$se)
animalown$uci <- animalown$rr*exp(1.96*animalown$se)
animalown$z <- z_rr(animalown$rr, animalown$se)
animalown$p <- p_rr(animalown$z)

animalown$var <- "Animal ownership"
names(animalown)[1] <- "val"
animalown$val <- as.character(animalown$val)
animalown$val[animalown$var == ""]
t1 <-rbind(t1, animalown)
rm(animalown)

# beediwork ####
beediwork <- ddply(first, .(beediwork) , summarise, 
                   n=sum(obs), cases=sum(case), episodes=sum(max), tar=round(sum(tdar)/365.25,2))
beediwork$rate <- rate_per_k(beediwork$episodes,beediwork$tar)
beediwork$rr[beediwork$beediwork == "No"] <- beediwork$rate[beediwork$beediwork == "No"] / beediwork$rate[beediwork$beediwork == "No"]
beediwork$rr[beediwork$beediwork == "Yes"] <- beediwork$rate[beediwork$beediwork == "Yes"] / beediwork$rate[beediwork$beediwork == "No"]
beediwork$se <- se_log_rr(beediwork$episodes[beediwork$beediwork=="Yes"], beediwork$episodes[beediwork$beediwork=="No"])
beediwork$lci <- beediwork$rr/exp(1.96*beediwork$se)
beediwork$uci <- beediwork$rr*exp(1.96*beediwork$se)
beediwork$z <- z_rr(beediwork$rr, beediwork$se)
beediwork$p <- p_rr(beediwork$z)
names(beediwork)[1] <- "val"
beediwork$var <- "Beedi made in household"
beediwork
t1 <- rbind(t1, beediwork)
rm(beediwork)

# tidy up ####
t1$se <- NULL
t1$z <- NULL
t1<-t1[c(11,1,2,3,4,5,6,7,8,9,10)] # reorder columns
t1$tar <- round(t1$tar,2)
t1$rate <- round(t1$rate,2)
t1$rr <- as.character(round(t1$rr,2))
t1$lci <- as.character(round(t1$lci,2))
t1$uci <- as.character(round(t1$uci,2))
t1$rr[nchar(t1$rr)==1] <- decimal2(t1$rr)[nchar(t1$rr)==1]
t1$rr[nchar(t1$rr)==3] <- decimal1(t1$rr)[nchar(t1$rr)==3]
t1$lci[nchar(t1$lci)==1] <- decimal2(t1$lci)[nchar(t1$lci)==1]
t1$lci[nchar(t1$lci)==3] <- decimal1(t1$lci)[nchar(t1$lci)==3]
t1$uci[nchar(t1$uci)==1] <- decimal2(t1$uci)[nchar(t1$uci)==1]
t1$uci[nchar(t1$uci)==3] <- decimal1(t1$uci)[nchar(t1$uci)==3]
t1$rateratio <- paste(as.character(t1$rr), " (", as.character(t1$lci), " - ", as.character(t1$uci),")", sep="")
t1$rr <- NULL
t1$lci <- NULL
t1$uci <- NULL
t1$p <- round(t1$p, 3)
t1 <- t1[c(1,2,3,4,5,6,7,9,8)]
t1$var[t1$var=="sex" & t1$val=="Male"] <- "Sex"
t1$var[t1$var=="sex" & t1$val=="Female"] <- ""
t1$var[t1$var=="neo" & t1$val=="No neonatal rotavirus"] <- "Sex"
t1$var[t1$var=="sex" & t1$val=="Female"] <- ""