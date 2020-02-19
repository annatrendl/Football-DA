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


check <- crimes_data[, list(Offence_class = Offence_class[1], Offence_subclass = Offence_subclass[1],
                            offence = offence[1], Domestic_abuse = DomesticAbuse[1],date_first_commited = date_first_commited[1],
                            Alcohol = Alcohol[1]),.(crime_number)]
check[, when.committed := ifelse(nchar(date_first_commited) == 13,substring(paste(0, date_first_commited, sep = ""),1,8),
                                 substring(date_first_commited,1,8))]
check[, when.committed := mdy(when.committed)]


other_violence <- check[Domestic_abuse != "Yes" & Offence_class == "Violence Against The Person",list(Otherv =.N),.(when.committed, Alcohol)]
property <- check[Offence_class %in% c("Theft", "Robbery"),list(Property =.N),.(when.committed, Alcohol)]
hate <- check[grepl("HATE|RACIALLY|RACIAL", offence),list(Hate =.N),.(when.committed, Alcohol)]


all.days <- merge(all.days, other_violence, by = c("when.committed", "Alcohol"), all.x = T)
all.days <- merge(all.days, property, by = c("when.committed", "Alcohol"), all.x = T)
all.days <- merge(all.days, hate, by = c("when.committed", "Alcohol"), all.x = T)

all.days[is.na(all.days)] <- 0

############################ CRIME TYPE RESULTS #########################################

#Reg results 2

form <- "~ Tournament_on+England_win+England_draw+England_lost+After_England+year2011+year2012+year2013+
year2014+year2015+year2016+year2017+year2018+year2019+Apr+Aug+Dec+Feb+Jul+Jun+Mar+May+Nov+Oct+Sep+
Thu+Fri+Sat+Sun+Tue+Wed+XMAS+NYE"

m.hatenoalcnb <- glm.nb(paste("Hate", form), data = all.days[Alcohol == "No",])
m.propnoalcnb <- glm.nb(paste("Property", form), data = all.days[Alcohol == "No",])
m.othervnoalcnb <- glm.nb(paste("Otherv", form), data = all.days[Alcohol == "No",])


m.hatenoalcp <- glm(paste("Hate", form), data = all.days[Alcohol == "No",],family = "poisson")
m.propnoalcp <- glm(paste("Property", form), data = all.days[Alcohol == "No",],family = "poisson")
m.othervnoalcp <- glm(paste("Otherv", form), data = all.days[Alcohol == "No",],family = "poisson")

dispersiontest(m.hatenoalcp)
dispersiontest(m.propnoalcp)
dispersiontest(m.othervnoalcp)


m.hatealcnb <- glm.nb(paste("Hate", form), data = all.days[Alcohol == "Yes",])
m.propalcnb <- glm.nb(paste("Property", form), data = all.days[Alcohol == "Yes",])
m.othervalcnb <- glm.nb(paste("Otherv", form), data = all.days[Alcohol == "Yes",])


m.hatealcp <- glm(paste("Hate", form), data = all.days[Alcohol == "Yes",],family = "poisson")
m.propalcp <- glm(paste("Property", form), data = all.days[Alcohol == "Yes",],family = "poisson")
m.othervalcp <- glm(paste("Otherv", form), data = all.days[Alcohol == "Yes",],family = "poisson")

dispersiontest(m.hatealcp)
dispersiontest(m.propalcp)
dispersiontest(m.othervalcp)



estnoalchate <- exp(cbind(Estimate = coef(m.hatenoalcnb), confint(m.hatenoalcnb)))
estnoalcprop <- exp(cbind(Estimate = coef(m.propnoalcnb), confint(m.propnoalcnb)))
estnoalcotherv <- exp(cbind(Estimate = coef(m.othervnoalcnb), confint(m.othervnoalcnb)))

p_noalchate<-drop1(m.hatenoalcnb, test = "LRT")
p_noalcprop<-drop1(m.propnoalcnb, test = "LRT")
p_noalcotherv<-drop1(m.othervnoalcnb, test = "LRT")

estnoalchate <- cbind(estnoalchate,p_noalchate$`Pr(>Chi)`)
estnoalcprop <- cbind(estnoalcprop,p_noalcprop$`Pr(>Chi)`)
estnoalcotherv <- cbind(estnoalcotherv,p_noalcotherv$`Pr(>Chi)`)

estalchate <- exp(cbind(Estimate = coef(m.hatealcnb), confint(m.hatealcnb)))
estalcprop <- exp(cbind(Estimate = coef(m.propalcnb), confint(m.propalcnb)))
estalcotherv <- exp(cbind(Estimate = coef(m.othervalcnb), confint(m.othervalcnb)))

p_alchate<-drop1(m.hatealcnb, test = "LRT")
p_alcprop<-drop1(m.propalcnb, test = "LRT")
p_alcotherv<-drop1(m.othervalcnb, test = "LRT")

estalchate <- cbind(estalchate,p_alchate$`Pr(>Chi)`)
estalcprop <- cbind(estalcprop,p_alcprop$`Pr(>Chi)`)
estalcotherv <- cbind(estalcotherv,p_alcotherv$`Pr(>Chi)`)

stargazer(m.hatenoalcnb,m.propnoalcnb,m.othervnoalcnb,
          m.hatealcnb,m.propalcnb,m.othervalcnb,
          omit = c(names(estalcotherv[,1][!grepl("_",names(estalcotherv[,1]))]), "Constant"),
          #column.labels=c('Number of non-alcohol related domestic abuse cases','Number of alcohol-related domestic abuse cases'),
          coef = list(estnoalchate[,1][grepl("_",names(estnoalchate[,1]))], 
                      estnoalcprop[,1][grepl("_",names(estnoalcprop[,1]))],
                      estnoalcotherv[,1][grepl("_",names(estnoalcotherv[,1]))],
                      estalchate[,1][grepl("_",names(estalchate[,1]))],
                      estalcprop[,1][grepl("_",names(estalcprop[,1]))],
                      estalcotherv[,1][grepl("_",names(estalcotherv[,1]))]), 
          p = list(estnoalchate[,4][grepl("_",names(estnoalchate[,1]))],
                   estnoalcprop[,4][grepl("_",names(estnoalcprop[,1]))],
                   estnoalcotherv[,4][grepl("_",names(estnoalcotherv[,1]))],
                   estalchate[,4][grepl("_",names(estalchate[,1]))],
                   estalcprop[,4][grepl("_",names(estalcprop[,1]))],
                   estalcotherv[,4][grepl("_",names(estalcotherv[,1]))]), 
          ci.custom = list(estnoalchate[,2:3][grepl("_",names(estnoalchate[,1])),],
                           estnoalcprop[,2:3][grepl("_",names(estnoalcprop[,1])),],
                           estnoalcotherv[,2:3][grepl("_",names(estnoalcotherv[,1])),],
                           estalchate[,2:3][grepl("_",names(estalchate[,1])),],
                           estalcprop[,2:3][grepl("_",names(estalcprop[,1])),],
                           estalcotherv[,2:3][grepl("_",names(estalcotherv[,1])),]),type = "latex",
          star.cutoffs = c(0.05, 0.01, 0.001))

