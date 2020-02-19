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

#create type of day dummies
all.days<- merge(data.table(expand.grid(when.committed = seq(ymd("2010-01-01"), ymd("2019-10-10"), by = "days"),
                                  Alcohol = c("No","Yes"))),
                 data.table(dcast(all.days_original,when.committed ~ Type.of.day, fun.aggregate = length)),
                 by = "when.committed", all.x = T)

#create year dummies
all.days <- merge(all.days, data.table(dcast(all.days_original,when.committed ~ year, fun.aggregate = length)),
                  by = "when.committed", all.x = T)

#create month dummies
all.days <- merge(all.days, data.table(dcast(all.days_original,when.committed ~ month, fun.aggregate = length)),
                  by = "when.committed", all.x = T)

#create day of week dummies
all.days <- merge(all.days, data.table(dcast(all.days_original,when.committed ~ Day_of_week, fun.aggregate = length)),
                  by = "when.committed", all.x = T)

#add xmas and nye
all.days[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),1,0)]
all.days[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed)==1,1,0)]

all.days[all.days ==2] <- 1


#finally add number of cases
all.days <- merge(all.days, all.days_original[,c("when.committed", "N", "Alcohol")],
                  by = c("when.committed", "Alcohol"), all.x = T)



#make everything a factor
all.days[,(colnames(all.days[, 3:39])):= lapply(.SD, as.factor), .SDcols = colnames(all.days[, 3:39])]


#fix colnames
colnames(all.days) <- gsub(" ", "_", colnames(all.days))

#add year to years
colnames(all.days) <- gsub("20", "year20", colnames(all.days))

############################ CORE RESULTS #########################################

#Reg results 1

form <- "N~ Tournament_on+England_win+England_draw+England_lost+After_England+year2011+year2012+year2013+
year2014+year2015+year2016+year2017+year2018+year2019+Apr+Aug+Dec+Feb+Jul+Jun+Mar+May+Nov+Oct+Sep+
Thu+Fri+Sat+Sun+Tue+Wed+XMAS+NYE"

summary(malc <- glm.nb(formula = form, data = all.days[Alcohol == "Yes",]))
summary(malcp <- glm(formula = form, data = all.days[Alcohol == "Yes",],family = "poisson"))
dispersiontest(malcp)

summary(mnalc <- glm.nb(formula = form, data = all.days[Alcohol == "No",]))
summary(mnalcp <- glm(formula = form, data = all.days[Alcohol == "No",],family = "poisson"))
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




