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



check <- crimes_data[DomesticAbuse == "Yes",list(Alcohol = Alcohol[1], no_vict = sum(role == "VICT"),
                                                 no_off = sum(role != "VICT"),
                                                 date_first_commited = date_first_commited[1]),.(crime_number)]

gender_data <- crimes_data[crime_number %in% check[no_vict == 1 & no_off ==1, crime_number],]
gender_data <- gender_data[order(crime_number, role)]
gender_data[, Gender.Type := paste0(gender,collapse = ""), .(crime_number)]
gender_data <- gender_data[order(crime_number, role)]

gender_data <- gender_data[, list(date_first_commited = date_first_commited[1],
                                  Alcohol = Alcohol[1], Gender.Type = Gender.Type[1]), .(crime_number)]

gender_data <- gender_data[!grepl("U|NA", Gender.Type),]
gender_data <- gender_data[,.N,.(Alcohol, Gender.Type, date_first_commited)]

gender_data[, when.committed := ifelse(nchar(date_first_commited) == 13,substring(paste(0, date_first_commited, sep = ""),1,8),
                                       substring(date_first_commited,1,8))]
gender_data[, when.committed := mdy(when.committed)]

all.days <- merge(all.days, dcast(gender_data, Alcohol + when.committed ~ Gender.Type, value.var = "N"), by = c("when.committed", "Alcohol"))

all.days[is.na(all.days)] <- 0

############################ GENDER RESULTS #########################################
form <- "~ Tournament_on+England_win+England_draw+England_lost+After_England+year2011+year2012+year2013+
year2014+year2015+year2016+year2017+year2018+year2019+Apr+Aug+Dec+Feb+Jul+Jun+Mar+May+Nov+Oct+Sep+
Thu+Fri+Sat+Sun+Tue+Wed+XMAS+NYE"

m.mfnoalcnb <- glm.nb(paste("MF", form), data = all.days[Alcohol == "No",])
m.fmnoalcnb <- glm.nb(paste("FM", form), data = all.days[Alcohol == "No",])
m.mfnoalcp <- glm(paste("MF", form), data = all.days[Alcohol == "No",],family = "poisson")
m.fmnoalcp <- glm(paste("FM", form), data = all.days[Alcohol == "No",],family = "poisson")
m.mmnoalcnb <- glm.nb(paste("MM", form), data = all.days[Alcohol == "No",])
m.ffnoalcnb <- glm.nb(paste("FF", form), data = all.days[Alcohol == "No",])
m.mmnoalcp <- glm(paste("MM", form), data = all.days[Alcohol == "No",],family = "poisson")
m.ffnoalcp <- glm(paste("FF", form), data = all.days[Alcohol == "No",],family = "poisson")




dispersiontest(m.mfnoalcp)
dispersiontest(m.fmnoalcp)
dispersiontest(m.mmnoalcp)
dispersiontest(m.ffnoalcp)


m.mfalcnb <- glm.nb(paste("MF", form), data = all.days[Alcohol == "Yes",])
m.fmalcnb <- glm.nb(paste("FM", form), data = all.days[Alcohol == "Yes",])
m.mfalcp <- glm(paste("MF", form), data = all.days[Alcohol == "Yes",],family = "poisson")
m.fmalcp <- glm(paste("FM", form), data = all.days[Alcohol == "Yes",],family = "poisson")
m.mmalcnb <- glm.nb(paste("MM", form), data = all.days[Alcohol == "Yes",])
m.ffalcnb <- glm.nb(paste("FF", form), data = all.days[Alcohol == "Yes",])
m.mmalcp <- glm(paste("MM", form), data = all.days[Alcohol == "Yes",],family = "poisson")
m.ffalcp <- glm(paste("FF", form), data = all.days[Alcohol == "Yes",],family = "poisson")



dispersiontest(m.mfalcp)
dispersiontest(m.fmalcp)
dispersiontest(m.mmalcp)
dispersiontest(m.ffalcp)

#poisson fits better for alcohol - male to male and female to female
estnoalcmf <- exp(cbind(Estimate = coef(m.mfnoalcnb), confint(m.mfnoalcnb)))
estnoalcfm <- exp(cbind(Estimate = coef(m.fmnoalcnb), confint(m.fmnoalcnb)))
estnoalcmm <- exp(cbind(Estimate = coef(m.mmnoalcnb), confint(m.mmnoalcnb)))
estnoalcff <- exp(cbind(Estimate = coef(m.ffnoalcnb), confint(m.ffnoalcnb)))
estalcmf <- exp(cbind(Estimate = coef(m.mfalcnb), confint(m.mfalcnb)))
estalcfm <- exp(cbind(Estimate = coef(m.fmalcnb), confint(m.fmalcnb)))
estalcmm <- exp(cbind(Estimate = coef(m.mmalcp), confint(m.mmalcp)))
estalcff <- exp(cbind(Estimate = coef(m.ffalcp), confint(m.ffalcp)))

p_mfnoalcnb<-drop1(m.mfnoalcnb, test = "LRT")
p_fmnoalcnb<-drop1(m.fmnoalcnb, test = "LRT")
p_mmnoalcnb<-drop1(m.mmnoalcnb, test = "LRT")
p_ffnoalcnb<-drop1(m.ffnoalcnb, test = "LRT")
p_mfalcnb<-drop1(m.mfalcnb, test = "LRT")
p_fmalcnb<-drop1(m.fmalcnb, test = "LRT")
p_mmalcp<-drop1(m.mmalcp, test = "LRT")
p_ffalcp<-drop1(m.ffalcp, test = "LRT")

estnoalcmf <- cbind(estnoalcmf,p_mfnoalcnb$`Pr(>Chi)`)
estnoalcfm <- cbind(estnoalcfm,p_fmnoalcnb$`Pr(>Chi)`)
estnoalcmm <- cbind(estnoalcmm,p_mmnoalcnb$`Pr(>Chi)`)
estnoalcff <- cbind(estnoalcff,p_ffnoalcnb$`Pr(>Chi)`)
estalcmf <- cbind(estalcmf,p_mfalcnb$`Pr(>Chi)`)
estalcfm <- cbind(estalcfm,p_fmalcnb$`Pr(>Chi)`)
estalcmm <- cbind(estalcmm,p_mmalcp$`Pr(>Chi)`)
estalcff <- cbind(estalcff,p_ffalcp$`Pr(>Chi)`)

stargazer(m.mfnoalcnb,m.fmnoalcnb,m.mmnoalcnb,m.ffnoalcnb,
          m.mfalcnb,m.fmalcnb,m.mmalcp,m.ffalcp,
          omit = c(names(estnoalcmf[,1][!grepl("_",names(estnoalcmf[,1]))]), "Constant"),
          #column.labels=c('Number of non-alcohol related domestic abuse cases','Number of alcohol-related domestic abuse cases'),
          coef = list(estnoalcmf[,1][grepl("_",names(estnoalcmf[,1]))],
                      estnoalcfm[,1][grepl("_",names(estnoalcfm[,1]))],
                      estnoalcmm[,1][grepl("_",names(estnoalcmm[,1]))],
                      estnoalcff[,1][grepl("_",names(estnoalcff[,1]))],
                      estalcmf[,1][grepl("_",names(estalcmf[,1]))],
                      estalcfm[,1][grepl("_",names(estalcfm[,1]))],
                      estalcmm[,1][grepl("_",names(estalcmm[,1]))],
                      estalcff[,1][grepl("_",names(estalcff[,1]))]),
          p = list(estnoalcmf[,4][grepl("_",names(estnoalcmf[,1]))],
                   estnoalcfm[,4][grepl("_",names(estnoalcfm[,1]))],
                   estnoalcmm[,4][grepl("_",names(estnoalcmm[,1]))],
                   estnoalcff[,4][grepl("_",names(estnoalcff[,1]))],
                   estalcmf[,4][grepl("_",names(estalcmf[,1]))],
                   estalcfm[,4][grepl("_",names(estalcfm[,1]))],
                   estalcmm[,4][grepl("_",names(estalcmm[,1]))],
                   estalcff[,4][grepl("_",names(estalcff[,1]))]),
          ci.custom = list(estnoalcmf[,2:3][grepl("_",names(estnoalcmf[,1])),],
                           estnoalcfm[,2:3][grepl("_",names(estnoalcfm[,1])),],
                           estnoalcmm[,2:3][grepl("_",names(estnoalcmm[,1])),],
                           estnoalcff[,2:3][grepl("_",names(estnoalcff[,1])),],
                           estalcmf[,2:3][grepl("_",names(estalcmf[,1])),],
                           estalcfm[,2:3][grepl("_",names(estalcfm[,1])),],
                           estalcmm[,2:3][grepl("_",names(estalcmm[,1])),],
                           estalcff[,2:3][grepl("_",names(estalcff[,1])),]
          ), type = "latex", star.cutoffs = c(0.05, 0.01, 0.001))


