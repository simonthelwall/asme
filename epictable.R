
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
sex$var <-"Sex"
names(sex)[1] <- "val"
#sex
first$male <- 0
first$male[first$sex=="M"] <- 1


# Age ####
first$age.entry <- round(as.numeric(first$doe - first$dob, format="days")/28, 2)
first$age.exit <- round(as.numeric(first$exitdate - first$dob, format="days")/28, 2)
f.expanded <-survSplit(first, cut = c(2, 4, 8, 12, 16, 24), 
                       end = "age.exit", event = "case",
                       start = "age.entry", zero = "dob", id = "id", episode = "age")
f.expanded$obs <- 1
f.expanded$obs[duplicated(f.expanded$id)==TRUE]<-0
f.expanded[f.expanded$id==6,]
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
age_cases <- subset(age, select = c(age, cases))
rm(f.expanded)
all$case <- 0
all$case[all$exittype=="RV diarrhoea"] <- 1
all$age.entry <- round((as.numeric(all$startfoll, format="days") - as.numeric(all$dob, format="days"))/28, 2)
all$age.exit <- round((as.numeric(all$endfoll, format="days") - as.numeric(all$dob, format="days"))/28, 2)
all.expanded <-survSplit(all, cut = c(2, 4, 8, 12, 16, 24), 
                       end = "age.exit", event = "case",
                       start = "age.entry", zero = "dob", id = "id", episode = "age")
all.expanded$doe2 <- all.expanded$startfoll + all.expanded$age.entry*28
all.expanded$dexit <- all.expanded$startfoll + all.expanded$age.exit*28
all.expanded$futime <- as.numeric(all.expanded$dexit - all.expanded$doe2, format="days") #
head(all.expanded)
age <- ddply(all.expanded, .(age), summarise, pdar=sum(futime, na.rm=TRUE), cases=sum(case, na.rm=TRUE))
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
age$pdar <- NULL
names(age) <- c("age", "inc.cases", "pyar", "rate")
age <- merge(age, age_cases, by.x="age", by.y = "age", all.x=TRUE)
age$age <- factor(age$age, 
                     levels = c("0-2 months", "2-4 months", "4-8 months", "8-12 months", "12-16 months", "16-24 months", ">24 months"), 
                     ordered = TRUE)
age <- age[order(age$age),]
age <- age[c(1,5,2,3,4)]
age
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
#neo
neo$var <-"neo"
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

edumoth$edumoth <- as.character(edumoth$edumoth)
names(edumoth)[1] <- "val"
edumoth$var <- "Mother's education"
#edumoth
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
#hhsize

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

lbw$var <- "Birthweight"
names(lbw)[1] <- "val"
lbw$val <- as.character(lbw$val)
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
#beediwork
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
t1$var[t1$var=="neo" & t1$val=="No neonatal rotavirus"] <- "Neonatal rotaviral diarrhoea"
t1$var[t1$var=="sex" & t1$val=="Female"] <- ""
t1
tab <- xtable(t1, caption = "Characteristics of study participants", label = "epic")
digits(tab)[c(4,5,6)] <- 0