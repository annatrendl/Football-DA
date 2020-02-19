rm(list = ls())
library(data.table)
library(lubridate)
library(ggplot2)
library(lmtest)
library(pscl)
library(xtable)
library(RColorBrewer)
library(MASS)
library(AER)
library(stargazer)
library(emmeans)
library(tidyverse)
setwd("C:/Anna/Crimes/wm_supersupernew")
load("C:/Anna/Crimes/wm_supersupernew/all_data.RData")

load("C:/Anna/Crimes/wm_supernew/sixnations.RData")
sixnations[, date := dmy(date)]
all_days_sixnations <- merge(all.days[,1:8], sixnations[England_played == T,], by.x = "when.committed",
                             by.y = "date", all.x = TRUE)
all_days_sixnations[is.na(England_won), England_won := FALSE]
all_days_sixnations[is.na(England_draw), England_draw := FALSE]
all_days_sixnations[is.na(England_lost), England_lost := FALSE]
all_days_sixnations[is.na(England_played), England_played := FALSE]
all_days_sixnations[when.committed %in% sixnations[England_played==TRUE,date +1], After_England := TRUE]
all_days_sixnations[is.na(After_England), After_England := FALSE]
all_days_sixnations[England_won == TRUE,Type.of.day2 := "England win"]
#all_days[England_draw == TRUE,Type.of.day := "England draw"]
all_days_sixnations[England_lost == TRUE,Type.of.day2 := "England lost"]
all_days_sixnations[After_England == TRUE,Type.of.day2 := "After England"]
all_days_sixnations[England_draw == TRUE,Type.of.day2 := "England draw"]
all_days_sixnations[is.na(Type.of.day2),Type.of.day2 := "Nonmatch day"]
sixnations[, year := year(date)]
sixnations[,list(mini = min(date), maxi = max(date)),.(year)]
all_days_sixnations[(when.committed >= ymd("2010-02-06") & when.committed <= ymd("2010-03-20")) | 
                      (when.committed >= ymd("2011-02-04") & when.committed <= ymd("2011-03-19")) |
                      (when.committed >= ymd("2012-02-04") & when.committed <= ymd("2012-03-17")) | 
                      (when.committed >= ymd("2013-02-02") & when.committed <= ymd("2013-03-16")) |
                      (when.committed >= ymd("2014-02-01") & when.committed <= ymd("2014-03-15")) |
                      (when.committed >= ymd("2015-02-06") & when.committed <= ymd("2015-03-21")) | 
                      (when.committed >= ymd("2016-02-06") & when.committed <= ymd("2016-03-19")) | 
                      (when.committed >= ymd("2017-02-04") & when.committed <= ymd("2017-03-18")) | 
                      (when.committed >= ymd("2018-02-03") & when.committed <= ymd("2018-03-17")),
                    Tournament_on := TRUE]
all_days_sixnations[is.na(Tournament_on), Tournament_on := FALSE]
all_days_sixnations[, Type.of.day2 := ifelse(Type.of.day2 == "Nonmatch day" & Tournament_on == TRUE,
                                             "Tournament on",Type.of.day2)]
all_days_sixnations[,Type.of.day2 := as.factor(Type.of.day2)]

all_days_sixnations[,Type.of.day2 := factor(Type.of.day2, levels =c("Nonmatch day","Tournament on", "England win","England draw",
                                                                    "England lost","After England"))]
all_days_sixnations[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                                        "Mon","Tue", "Wed"))]




#create type of day dummies
all.days<- merge(data.table(expand.grid(when.committed = seq(ymd("2010-01-01"), ymd("2019-10-10"), by = "days"),
                                        Alcohol = c("No","Yes"))),
                 data.table(dcast(all_days_sixnations,when.committed ~ Type.of.day2, fun.aggregate = length)),
                 by = "when.committed", all.x = T)

#create year dummies
all.days <- merge(all.days, data.table(dcast(all_days_sixnations,when.committed ~ year, fun.aggregate = length)),
                  by = "when.committed", all.x = T)

#create month dummies
all.days <- merge(all.days, data.table(dcast(all_days_sixnations,when.committed ~ month, fun.aggregate = length)),
                  by = "when.committed", all.x = T)

#create day of week dummies
all.days <- merge(all.days, data.table(dcast(all_days_sixnations,when.committed ~ Day_of_week, fun.aggregate = length)),
                  by = "when.committed", all.x = T)

#add xmas and nye
all.days[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),1,0)]
all.days[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed)==1,1,0)]

all.days[all.days ==2] <- 1


#finally add number of cases
all.days <- merge(all.days, all_days_sixnations[,c("when.committed", "N", "Alcohol")],
                  by = c("when.committed", "Alcohol"), all.x = T)



#make everything a factor
all.days[,(colnames(all.days[, 3:39])):= lapply(.SD, as.factor), .SDcols = colnames(all.days[, 3:39])]


#fix colnames
colnames(all.days) <- gsub(" ", "_", colnames(all.days))

#add year to years
colnames(all.days) <- gsub("20", "year20", colnames(all.days))



############################ RUGBY RESULTS #########################################
form <- "~ Tournament_on+England_win+England_draw+England_lost+After_England+year2011+year2012+year2013+
year2014+year2015+year2016+year2017+year2018+year2019+Apr+Aug+Dec+Feb+Jul+Jun+Mar+May+Nov+Oct+Sep+
Thu+Fri+Sat+Sun+Tue+Wed+XMAS+NYE"

summary(malc <- glm.nb(paste("N", form), data = all.days[Alcohol == "Yes",]))
summary(malcp <- glm(paste("N", form), data  = all.days[Alcohol == "Yes",],
                     family = "poisson"))
dispersiontest(malcp)


summary(mnalc <- glm.nb(paste("N", form), data = all.days[Alcohol == "No",]))
summary(mnalcp <- glm(paste("N", form), data  = all.days[Alcohol == "No",],
                     family = "poisson"))
dispersiontest(malcp)
dispersiontest(mnalcp)


estalc <- exp(cbind(Estimate = coef(malc), confint(malc)))
estnoalc <- exp(cbind(Estimate = coef(mnalc), confint(mnalc)))

p_malc<-drop1(malc, test = "LRT")
p_mnalc<-drop1(mnalc, test = "LRT")

estalc <- cbind(estalc,p_malc$`Pr(>Chi)`)
estnoalc <- cbind(estnoalc,p_mnalc$`Pr(>Chi)`)


stargazer(mnalc,malc, omit = c(names(estalc[,1])[!grepl("_",names(estalc[,1]))], "Constant"),
          column.labels=c('Number of non-alcohol related domestic abuse cases','Number of alcohol-related domestic abuse cases'),
          coef = list(estnoalc[,1][grepl("_",names(estnoalc[,1]))], estalc[,1][grepl("_",names(estalc[,1]))]), 
          p = list(estnoalc[,4][grepl("_",names(estnoalc[,1]))],
                   estalc[,4][grepl("_",names(estalc[,1]))]), 
          ci.custom = list(estnoalc[,2:3][grepl("_",names(estnoalc[,1])),],
                           estalc[,2:3][grepl("_",names(estalc[,1])),]), type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001))








