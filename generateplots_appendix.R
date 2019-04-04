###############ORIGINAL

rm(list = ls())
#setwd("C:/Users/Anna/Desktop/crimes")
library(data.table)
library(lubridate)
library(ggplot2)
library(stargazer)
library(MASS)
library(lmtest)
library(rgdal)
library(spdep)
library(emmeans)
library(AER)
setwd("C:/Anna/Crimes/wm_supernew")
load("Westmids_supernew.RData")
check <- crimes_cleaned[inc_da == "Yes" | Domestic_abuse != "No", list(no_vict = sum(role == "VICT"),
                                                                       no_off = sum(role != "VICT"),
                                                                       offence = offence[1],
                                                                       Offence_class = Offence_class[1],
                                                                       Offence_subclass = Offence_subclass[1]), .(crime_number)]
#of all incidents, 
#nrow(check[no_vict == 1 & no_off ==1,])/nrow(check)
#92% of domestic abuse incidents have one victim and one offender (either male or female)
#nrow(check)/length(unique(crimes_cleaned$crime_number))
#31% of crime is domestic abuse related
domestic_abuse <- crimes_cleaned[crime_number %in% check[no_vict == 1 & no_off ==1, crime_number],]
domestic_abuse <- domestic_abuse[order(crime_number, role)]
domestic_abuse[, Gender.Type := paste0(gender,collapse = ""), .(crime_number)]
domestic_abuse <- domestic_abuse[order(crime_number, role)]
domestic_abuse <- domestic_abuse[, list(datetime_first_committed = datetime_first_committed[1],
                                        Alcohol = Alcohol_inv[1], Gender.Type = Gender.Type[1],
                                        snap_east= snap_east[1],
                                        snap_north = snap_north[1]), .(crime_number)]

domestic_abuse <- domestic_abuse[!grepl("U|NA", Gender.Type),]
#domestic_abuse <- domestic_abuse[!is.na(Age_difference),]

domestic_abuse[, when.committed := ymd(substring(as.character(datetime_first_committed),1,10))]

load("C:/Anna/Crimes/wm_supernew/worldcupeuro.RData")
worldcupeuro <- worldcupeuro[Team1 == "England" | Team2 == "England",]
worldcupeuro[, Goalsteam1 := as.numeric(Goalsteam1)]
worldcupeuro[, Goalsteam2 := as.numeric(Goalsteam2)]
worldcupeuro[(Team1 == "England" & Goalsteam1 > Goalsteam2) |(Team2 == "England" & Goalsteam1 < Goalsteam2),
             England_win := TRUE]
worldcupeuro[("England" == Team1 | "England" == Team2)& (Goalsteam1 == Goalsteam2),
             England_draw := TRUE]
worldcupeuro[England_draw == TRUE | England_win == TRUE, England_windraw := TRUE]
worldcupeuro[(Team1 == "England" & (Goalsteam1 < Goalsteam2)),
             England_lost := TRUE]
worldcupeuro[(Team2 == "England" & (Goalsteam2 < Goalsteam1)),
             England_lost := TRUE]
#apply(worldcupeuro[,c(8,9,11)], 1, sum, na.rm = TRUE)
worldcupeuro[Team1 == "England" | Team2 == "England" , England_played := TRUE]

worldcupeuro[, date := format(as.Date(Date, format="%d/%m/%Y")), by = 1:nrow(worldcupeuro)]
worldcupeuro[, Date := ymd(date) ]
worldcupeuro <- worldcupeuro[,c(3,6,8:13)]

all.days_new <- data.table(expand.grid(when.committed = seq(ymd("2010-01-01"), ymd("2018-11-06"), by = "days"),
                                       Alcohol = c("No","Yes")))

all.days_new <- merge(all.days_new,  domestic_abuse[,.N,.(when.committed,Alcohol)], 
                      by = c("when.committed","Alcohol"), all.x = TRUE)
all.days_new[is.na(all.days_new)] <- 0
all.days_new <- merge(all.days_new, worldcupeuro, by.x = "when.committed",
                      by.y = "Date", all.x = TRUE)
all.days_new[is.na(England_windraw), England_windraw := FALSE]
all.days_new[is.na(England_win), England_win := FALSE]
all.days_new[is.na(England_draw), England_draw := FALSE]
all.days_new[is.na(England_lost), England_lost := FALSE]
all.days_new[is.na(England_played), England_played := FALSE]
all.days_new[when.committed %in% worldcupeuro[England_played==TRUE,Date +1], After_England := TRUE]
all.days_new[is.na(After_England), After_England := FALSE]


all.days_new[England_win == TRUE,Type.of.day := "England win"]
all.days_new[England_draw == TRUE,Type.of.day := "England draw"]
all.days_new[England_lost == TRUE,Type.of.day := "England lost"]
all.days_new[After_England == TRUE,Type.of.day := "After England"]
all.days_new[is.na(Type.of.day),Type.of.day := "Nonmatch day"]
all.days_new[(when.committed >= ymd("2010-06-11") & when.committed <= ymd("2010-07-11")) | 
               (when.committed >= ymd("2014-06-12") & when.committed <= ymd("2014-07-13")) |
               (when.committed >= ymd("2012-06-08") & when.committed <= ymd("2012-07-01")) | 
               (when.committed >= ymd("2016-06-10") & when.committed <= ymd("2016-07-10")) |
               (when.committed >= ymd("2018-06-14") & when.committed <= ymd("2018-07-15")),
             Tournament_on := TRUE]
all.days_new[is.na(Tournament_on), Tournament_on := FALSE]
all.days_new[, Type.of.day := ifelse(Type.of.day == "Nonmatch day" & Tournament_on == TRUE,
                                     "Tournament on",Type.of.day)]
all.days_new[,Type.of.day := as.factor(Type.of.day)]

all.days_new[,Type.of.day := factor(Type.of.day, levels = c("Nonmatch day","Tournament on", "England win","England draw",
                                                            "England lost","After England"))]
all.days_new[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                                 "Mon","Tue", "Wed"))]

all.days_new[, year := as.factor(year(when.committed))]
all.days_new[, month := factor(as.character(month(when.committed, label = TRUE)))]

all.days_new <- all.days_new[!(when.committed == ymd("2018-11-06") | (when.committed >= ymd("2017-06-01") & when.committed <= ymd("2017-12-31"))),]
all.days_new[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
all.days_new[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed) == 1,T,F)]



####################################################### KIRBY

lancashire_counts <- data.table(read.csv("lancashire_da_counts.csv"))
lancashire_counts[, Date := dmy(Date)]
lancashire_counts[, England_windraw := FALSE]
lancashire_counts[, England_lost := FALSE]
lancashire_counts[, England_win := FALSE]
lancashire_counts[, England_draw := FALSE]
lancashire_counts[Date %in% (c(ymd("2002/06/07"),
                               ymd("2002/06/15"), ymd("2006/06/10"), ymd("2006/06/15"),
                               ymd("2006/06/25"), ymd("2010/06/23"))), England_win := TRUE]
lancashire_counts[Date %in% (c(ymd("2002/06/02"), ymd("2002/06/12"),
                               ymd("2006/06/20"), ymd("2010/06/12"),
                               ymd("2010/06/18"))), England_draw := TRUE]
lancashire_counts[England_draw|England_win,England_windraw := TRUE]

lancashire_counts[Date %in% (c(ymd("2002/06/21"), ymd("2006/07/01"), ymd("2010/06/27"))),
                  England_lost := TRUE]
lancashire_counts[, England_played := ifelse(England_windraw == TRUE | England_lost == TRUE, TRUE, FALSE)]
lancashire_counts[, After_England := shift(England_played, n = 1, fill = NA, type = "lag")]

lancashire_counts[England_windraw == TRUE,Type.of.day := "England windraw"]
lancashire_counts[England_lost == TRUE,Type.of.day := "England lost"]
lancashire_counts[After_England == TRUE,Type.of.day := "After England"]
lancashire_counts[is.na(Type.of.day),Type.of.day := "Nonmatch day"]
lancashire_counts[,Type.of.day := factor(Type.of.day, levels = c("Nonmatch day", "England windraw",
                                                                 "England lost","After England"))]

lancashire_counts[England_win == TRUE,Type.of.day2 := "England win"]
lancashire_counts[England_draw == TRUE,Type.of.day2 := "England draw"]
lancashire_counts[England_lost == TRUE,Type.of.day2 := "England lost"]
lancashire_counts[After_England == TRUE,Type.of.day2 := "After England"]
lancashire_counts[is.na(Type.of.day2),Type.of.day2 := "Nonmatch day"]
#lancashire_counts[, Type.of.day2 := ifelse(Type.of.day2 == "Nonmatch_day",Type.of.day2)]
lancashire_counts[,Type.of.day2 := as.factor(Type.of.day2)]

lancashire_counts[,Type.of.day2 := factor(Type.of.day2, levels = c("Nonmatch day","Tournament on", "England win","England draw",
                                                                   "England lost","After England"))]


lancashire_counts[,Day_of_week := factor(as.character(wday(Date, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                            "Mon","Tue", "Wed"))]
lancashire_counts[, year := as.factor(year(Date))]
lancashire_counts[, week_no := as.factor(week(Date))]
lancashire_counts[, month := month(Date, label = T)]
lancashire_counts[, Year := paste(year, "World Cup"), by = 1:nrow(lancashire_counts)]

summary(kirby1 <- glm.nb(da_count ~ year + Type.of.day + Day_of_week, data = lancashire_counts))
summary(kirby2 <- glm.nb(da_count ~ year + Type.of.day2 + Day_of_week, data = lancashire_counts))
summary(kirby1p <- glm(da_count ~ year + Type.of.day + Day_of_week, data = lancashire_counts, family = "poisson"))
summary(kirby2p <- glm(da_count ~ year + Type.of.day2 + Day_of_week, data = lancashire_counts, family = "poisson"))

lrtest(kirby1p, kirby1)
lrtest(kirby2p, kirby2)

####################################

CI.vectors <- data.table(exp(confint(kirby1)),exp(confint(kirby2)))
Coef.vectors <- data.table(exp(kirby1$coefficients), exp(kirby2$coefficients))
P.vals <- data.table(summary(kirby1)$coefficients[,4],summary(kirby2)$coefficients[,4])

stargazer(kirby1,kirby2,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          column.labels = c("Original Model", "Win/Draw Separate"),
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1)), 
          p =list(P.vals$V1,
                  P.vals$V2),
          keep.stat = c("n", "aic", "bic"),
          add.lines=list(c("BIC",round(BIC(kirby1),3), round(BIC(kirby2),3))))


######################### Why so different


summary(kirby3 <- glm.nb(da_count ~  Type.of.day2 + Day_of_week, data = lancashire_counts[year == 2002,]))
summary(kirby4 <- glm.nb(da_count ~ Type.of.day2 + Day_of_week, data = lancashire_counts[year == 2006,]))
summary(kirby5 <- glm.nb(da_count ~ Type.of.day2 + Day_of_week, data = lancashire_counts[year == 2010,]))

summary(kirby3p <- glm(da_count ~ Type.of.day2 + Day_of_week, data = lancashire_counts[year == 2002,], family = "poisson"))
summary(kirby4p <- glm(da_count ~ Type.of.day2 + Day_of_week, data = lancashire_counts[year == 2006,], family = "poisson"))
summary(kirby5p <- glm(da_count ~ Type.of.day2 + Day_of_week, data = lancashire_counts[year == 2010,], family = "poisson"))

lrtest(kirby3p, kirby3)
lrtest(kirby4p, kirby4)
lrtest(kirby5p, kirby5)


summary(our18 <- glm.nb(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2018,]))
summary(our16 <- glm.nb(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2016,]))
summary(our14 <- glm.nb(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2014,]))
summary(our12 <- glm.nb(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2012,]))
summary(our10 <- glm.nb(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2010,]))

summary(our18p <- glm(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2018,], family = "poisson"))
summary(our16p <- glm(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2016,], family = "poisson"))
summary(our14p <- glm(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2014,], family = "poisson"))
summary(our12p <- glm(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2012,], family = "poisson"))
summary(our10p <- glm(N ~ Type.of.day*Alcohol +  Day_of_week*Alcohol +  month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year == 2010,], family = "poisson"))


lrtest(our18p, our18)
lrtest(our16p, our16)
lrtest(our14p, our14)
lrtest(our12p, our12)
lrtest(our10p, our10)

CI.vectors <- rbind(exp(confint(kirby3),exp(confint(kirby4p)),exp(confint(kirby5p)),
                    exp(confint(our10)),
                    exp(confint(our12)),exp(confint(our14)),
                    exp(confint(our16)),exp(confint(our18))))


names(kirby3$coefficients) <- gsub("Type.of.day2","",names(kirby3$coefficients))
names(kirby4p$coefficients) <- gsub("Type.of.day2","",names(kirby4p$coefficients))
names(kirby5p$coefficients) <- gsub("Type.of.day2","",names(kirby5p$coefficients))
names(our10$coefficients) <- gsub("Type.of.day","",names(our10$coefficients))
names(our12$coefficients) <- gsub("Type.of.day","",names(our12$coefficients))
names(our14$coefficients) <- gsub("Type.of.day","",names(our14$coefficients))
names(our16$coefficients) <- gsub("Type.of.day","",names(our16$coefficients))
names(our18$coefficients) <- gsub("Type.of.day","",names(our18$coefficients))


stargazer(kirby3,kirby4p,kirby5p,our10,our12,our14,our16,our18,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          no.space = TRUE,
         column.labels = c("2002", "2006", "2010", "2010", "2012", "2014", "2016", "2018"),
          coef = list(exp(as.numeric(kirby3$coefficients))-1,
                      exp(as.numeric(kirby4p$coefficients))-1,
                      exp(as.numeric(kirby5p$coefficients))-1,
                      exp(as.numeric(our10$coefficients))-1, 
                      exp(as.numeric(our12$coefficients))-1, 
                      exp(as.numeric(our14$coefficients))-1, 
                      exp(as.numeric(our16$coefficients))-1, 
                      exp(as.numeric(our18$coefficients))-1), 
          p =list(summary(kirby3)$coefficients[,4],
                    summary(kirby4p)$coefficients[,4],
                    summary(kirby5p)$coefficients[,4],
                    summary(our10)$coefficients[,4],
                    summary(our12)$coefficients[,4],
                    summary(our14)$coefficients[,4],
                    summary(our16)$coefficients[,4],
                    summary(our18)$coefficients[,4])
          )


######################################################### ROBUSTNESS

summary(all <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                        month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new))
summary(excl18 <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2018,]))
summary(excl16 <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2016,]))
summary(excl14 <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2014,]))
summary(excl12 <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2012,]))
summary(excl10 <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2010,]))

summary(excl18a <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2018,]))
summary(excl16a <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2016,]))
summary(excl14a <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2014,]))
summary(excl12a <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2012,]))
summary(excl10a <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2010,]))

BIC(excl18)
BIC(excl18a)
BIC(excl16)
BIC(excl16a)
BIC(excl14)
BIC(excl14a)
BIC(excl12)
BIC(excl12a)
BIC(excl10)
BIC(excl10a)




summary(allp <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                        month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new, family = "poisson"))
summary(excl18p <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2018,], family = "poisson"))
summary(excl16p <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2016,], family = "poisson"))
summary(excl14p <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2014,], family = "poisson"))
summary(excl12p <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2012,], family = "poisson"))
summary(excl10p <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[year!=2010,], family = "poisson"))

lrtest(allp, all)
lrtest(excl18p, excl18)
lrtest(excl16p, excl16)
lrtest(excl14p, excl14)
lrtest(excl12p, excl12)
lrtest(excl10p, excl10)

dispersiontest(allp)
dispersiontest(excl18p)
dispersiontest(excl16p)
dispersiontest(excl14p)
dispersiontest(excl12p)
dispersiontest(excl10p)

# CI.vectors <- data.table(exp(confint(all)),exp(confint(excl18)),exp(confint(excl16)),exp(confint(excl14)),
#                          exp(confint(excl12)),exp(confint(excl10)))




Coef.vectors <- data.table(exp(excl18$coefficients),exp(excl16$coefficients),
                           exp(excl14$coefficients), exp(excl12$coefficients),exp(excl10$coefficients))
P.vals <- data.table(summary(excl18)$coefficients[,4],
                     summary(excl16)$coefficients[,4],summary(excl14)$coefficients[,4],
                     summary(excl12)$coefficients[,4],summary(excl10)$coefficients[,4])


names(excl18$coefficients) <- gsub("Type.of.day|Yes", "", names(excl18$coefficients) )
names(excl16$coefficients) <- gsub("Type.of.day|Yes", "", names(excl16$coefficients) )
names(excl14$coefficients) <- gsub("Type.of.day|Yes", "", names(excl14$coefficients) )
names(excl12$coefficients) <- gsub("Type.of.day|Yes", "", names(excl12$coefficients) )
names(excl10$coefficients) <- gsub("Type.of.day|Yes", "", names(excl10$coefficients) )



stargazer(excl18,excl16,excl14,excl12,excl10,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          #covariate.labels = gsub("Type.of.day|Yes","",names(all$coefficients)[grepl("Type|Alcohol",names(all$coefficients) )]),
          column.labels = c("All years", "2018 excluded", "2016 excluded", "2014 excluded",
                            "2012 excluded", "2010 excluded"),
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1),
                      as.numeric(Coef.vectors$V3-1),
                      as.numeric(Coef.vectors$V4-1),
                      as.numeric(Coef.vectors$V5-1)),
                      #as.numeric(Coef.vectors$V6-1)), 
          # ci.custom = list(rbind(CI.vectors[,1:2],
          #                        CI.vectors[,3:4],
          #                        CI.vectors[,5:6],
          #                        CI.vectors[,7:8])),
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3,
                  P.vals$V4,
                  P.vals$V5))



 # # 
###########################ALTERNATIVE DEFINITIONS#############################

all_days <- all.days_new[, c("when.committed", "Alcohol", "N", "year", "Type.of.day",
                             "Day_of_week", "month", "XMAS", "NYE")]
setnames(all_days, "N", "Domestic_Abuse")
offences <- data.table(table(check$offence))
offences <- offences[order(N)]

check <- crimes_cleaned[, list(no_vict = sum(role == "VICT"),
                               no_off = sum(role != "VICT"),
                               offence = offence[1],
                               Offence_class = Offence_class[1],
                               Offence_subclass = Offence_subclass[1],
                               datetime_first_committed = datetime_first_committed[1],
                               Alcohol = Alcohol_inv[1],
                               inc_da = inc_da[1], Domestic_abuse =Domestic_abuse[1]), .(crime_number)]

check[inc_da == "Yes" | Domestic_abuse != "No", DA := T]
check[is.na(DA), DA := F]


 check[DA != T &Offence_class == "Sexual Offences", Type := "Sexual"]
 check[DA != T & grepl("CHILD ABUSE|VULNERABLE|YOUNG", offence), Type := "Other Abuse"]
check[, when.committed := ymd(substring(as.character(datetime_first_committed),1,10))]


all_days <- merge(all_days, check[Type == "Sexual",list(Sexual = .N),.(when.committed, Alcohol)],
                  by = c("when.committed", "Alcohol"), all.x = T)
all_days <- merge(all_days, check[Type == "Other Abuse",list(OtherAbuse = .N),.(when.committed, Alcohol)],
                  by = c("when.committed", "Alcohol"), all.x = T)

all_days[is.na(Sexual), Sexual := 0]
all_days[is.na(OtherAbuse), OtherAbuse := 0]
summary(da <- glm.nb(Domestic_Abuse ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data =all_days))
summary(sexual <- glm.nb(Sexual ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data =all_days))
summary(vulnerable <- glm.nb(OtherAbuse ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data =all_days))

summary(dap <- glm(Domestic_Abuse ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data =all_days, family = "poisson"))
summary(sexualp <- glm(Sexual ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data =all_days, family = "poisson"))
summary(vulnerablep <- glm(OtherAbuse ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data =all_days, family = "poisson"))

lrtest(dap,da)
lrtest(sexualp,sexual)
lrtest(vulnerablep,vulnerable)

dispersiontest(sexualp)
dispersiontest(vulnerablep)

#CI.vectors <- data.table(exp(confint(da)),exp(confint(sexual)),exp(confint(vulnerable)))
Coef.vectors <- data.table(exp(sexual$coefficients),
                           exp(vulnerable$coefficients))
P.vals <- data.table(summary(sexual)$coefficients[,4],summary(vulnerable)$coefficients[,4])

names(sexual$coefficients) <- gsub("Type.of.day|Yes", "", names(sexual$coefficients) )
names(vulnerable$coefficients) <- gsub("Type.of.day|Yes", "", names(vulnerable$coefficients) )


stargazer(sexual,vulnerable,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          #column.labels = c("Domestic Abuse", "Child/Vulnerable Adult Abuse", "Sexual abuse"),
          coef = list(#as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1)), 
          p =list(#P.vals$V1,
                  P.vals$V1,
                  P.vals$V2))

# m3 <- rbind(data.table(summary(emmeans(sexual, ~ Type.of.day*Alcohol))),
#             data.table(summary(emmeans(vulnerable, ~ Type.of.day*Alcohol))))
# m3[, Type := rep(c("Sexual", "Vulnerable"), each = 12)]
# 
# ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
#   facet_wrap(~Type, scales = "free", ncol = 2) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Number of cases ~ Type.of.day*Alcohol, separate regression for each crime type")
# ggsave("help_emmeans_dominance.pdf")

#################################################  Rugby & football


load("C:/Anna/Crimes/wm_supernew/sixnations.RData")
sixnations[, date := dmy(date)]
all_days <- merge(all_days, sixnations[England_played == T,], by.x = "when.committed",
                  by.y = "date", all.x = TRUE)
all_days[is.na(England_won), England_won := FALSE]
all_days[is.na(England_draw), England_draw := FALSE]
all_days[is.na(England_lost), England_lost := FALSE]
all_days[is.na(England_played), England_played := FALSE]
all_days[when.committed %in% sixnations[England_played==TRUE,date +1], After_England := TRUE]
all_days[is.na(After_England), After_England := FALSE]
all_days[England_won == TRUE,Type.of.day2 := "England win"]
#all_days[England_draw == TRUE,Type.of.day := "England draw"]
all_days[England_lost == TRUE,Type.of.day2 := "England lost"]
all_days[After_England == TRUE,Type.of.day2 := "After England"]
all_days[is.na(Type.of.day2),Type.of.day2 := "Nonmatch day"]
sixnations[, year := year(date)]
sixnations[,list(mini = min(date), maxi = max(date)),.(year)]
all_days[(when.committed >= ymd("2010-02-06") & when.committed <= ymd("2010-03-20")) | 
           (when.committed >= ymd("2011-02-04") & when.committed <= ymd("2011-03-19")) |
           (when.committed >= ymd("2012-02-04") & when.committed <= ymd("2012-03-17")) | 
           (when.committed >= ymd("2013-02-02") & when.committed <= ymd("2013-03-16")) |
           (when.committed >= ymd("2014-02-01") & when.committed <= ymd("2014-03-15")) |
           (when.committed >= ymd("2015-02-06") & when.committed <= ymd("2015-03-21")) | 
           (when.committed >= ymd("2016-02-06") & when.committed <= ymd("2016-03-19")) | 
           (when.committed >= ymd("2017-02-04") & when.committed <= ymd("2017-03-18")) | 
           (when.committed >= ymd("2018-02-03") & when.committed <= ymd("2018-03-17")),
         Tournament_on := TRUE]
all_days[is.na(Tournament_on), Tournament_on := FALSE]
all_days[, Type.of.day2 := ifelse(Type.of.day2 == "Nonmatch day" & Tournament_on == TRUE,
                                 "Tournament on",Type.of.day2)]
all_days[,Type.of.day2 := as.factor(Type.of.day2)]

all_days[,Type.of.day2 := factor(Type.of.day2, levels = c("Nonmatch day","Tournament on", "England win",
                                                        "England lost","After England"))]
all_days[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                             "Mon","Tue", "Wed"))]

all_days[, year := as.factor(year(when.committed))]
all_days[, month := factor(as.character(month(when.committed, label = TRUE)))]
all_days <- all_days[!(when.committed == ymd("2018-11-06") | (when.committed >= ymd("2017-06-01") & when.committed <= ymd("2017-12-31"))),]
all_days[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
all_days[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed) == 1,T,F)]

#all_days <- all_days[,c(1:2,4,15,17:21)]



# DP.nbfootballalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
# DP.nbfootballnalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])
# 
# DP.nbrugbyalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day2+ Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
# DP.nbrugbynalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day2 + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])

summary(DP.nbfootball <- glm.nb(Domestic_Abuse ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol +
                          XMAS*Alcohol + NYE*Alcohol, data = all_days))
summary(DP.nbrugby <- glm.nb(Domestic_Abuse ~ year*Alcohol + Type.of.day2*Alcohol + Day_of_week*Alcohol + month*Alcohol +
                          XMAS*Alcohol + NYE*Alcohol, data = all_days))


# CI.vectorsf <- data.table(exp(confint(DP.nbfootballalc)),exp(confint(DP.nbfootballnalc)))
# CI.vectorsr <- data.table(exp(confint(DP.nbrugbyalc)),exp(confint(DP.nbrugbynalc)))
Coef.vectorsf <- data.table(exp(DP.nbfootball$coefficients))
Coef.vectorsr <- data.table(exp(DP.nbrugby$coefficients))
P.valsf <- data.table(summary(DP.nbfootball)$coefficients[,4])
P.valsr <- data.table(summary(DP.nbrugby)$coefficients[,4])
# 
# m3 <- rbind(data.table(summary(emmeans(DP.nbrugbyalc, ~ Type.of.day2))),
#             data.table(summary(emmeans(DP.nbrugbynalc, ~ Type.of.day2))))
# setnames(m3, "Type.of.day2", "Type.of.day")
# m3[, Alcohol := rep(c("Yes", "No"), each = 5)]
#             
# m3 <- rbind(m3, data.table(summary(emmeans(DP.nbfootballalc, ~ Type.of.day)), Alcohol = "Yes"),
#             data.table(summary(emmeans(DP.nbfootballnalc, ~ Type.of.day)), Alcohol = "No"))
# m3[, Type := c(rep("Rugby", 10), rep("Football", 12))]

# ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
#   facet_wrap(~Type, scales = "free", ncol = 2) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "Number of cases ~ Type.of.day, separate regression for alcohol and non-alc related incients")
# ggsave("help_emmeans_rugbysep.pdf")

names(DP.nbfootball$coefficients) <- gsub("Type.of.day|Yes","",names(DP.nbfootball$coefficients))
names(DP.nbrugby$coefficients) <- gsub("Type.of.day2|Yes","",names(DP.nbrugby$coefficients))


stargazer(DP.nbfootball,DP.nbrugby,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          #covariate.labels = gsub("Type.of.day","",names(DP.nbfootballnalc$coefficients)[grepl("Type",names(DP.nbfootballnalc$coefficients) )]),
          column.labels = c("Football", "Rugby"),
          coef = list(as.numeric(Coef.vectorsf$V1-1),
                      as.numeric(Coef.vectorsr$V1-1)), 
          p =list(P.valsf$V1,
                  P.valsr$V1))

# 
# 
# 
# football <- glm.nb(Domestic_Abuse ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days)
# rugby <- glm.nb(Domestic_Abuse ~ year + Type.of.day2*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days)
# 
# m3 <- rbind(data.table(summary(emmeans(rugby, ~ Type.of.day2*Alcohol))))
# 
# ggplot(data = m3, aes(Type.of.day2, emmean, colour = Alcohol)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(title = "DA ~ Type.of.day*Alcohol, Rugby")
# ggsave("help_emmeans_rugbyint.pdf")
# 
# 
# CI.vectorsf <- data.table(exp(confint(football)))
# Coef.vectorsf <- data.table(exp(football$coefficients))
# P.valsf <- data.table(summary(football)$coefficients[,4])
# CI.vectorsr <- data.table(exp(confint(rugby)))
# Coef.vectorsr <- data.table(exp(rugby$coefficients))
# P.valsr <- data.table(summary(rugby)$coefficients[,4])
# 
# names(football$coefficients) <- gsub("Type.of.day|Yes","",names(football$coefficients))
# names(rugby$coefficients) <- gsub("Type.of.day2|Yes","",names(rugby$coefficients))
# 
# 
# stargazer(football,rugby,type = "latex",
#           omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
#           title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
#           no.space = TRUE,
#           #column.labels = c("All years", "2018 excluded", "2016 excluded", "2014 excluded"),
#           coef = list(as.numeric(Coef.vectorsf$V1-1),
#                       as.numeric(Coef.vectorsr$V1-1)), 
#           p =list(P.valsf$V1,
#                   P.valsr$V1))

################################DEPRIVATION###########################
# wgs84 = "+init=epsg:4326"
# bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
# +ellps=airy +datum=OSGB36 +units=m +no_defs'
# 
# ConvertCoordinates <- function(easting,northing) {
#   if (!is.na(easting) & !is.na(northing)) {
#   out = cbind(easting,northing)
#   mask = !is.na(easting)
#   sp <-  sp::spTransform(sp::SpatialPoints(list(easting[mask],northing[mask]),proj4string=sp::CRS(bng)),sp::CRS(wgs84))
#   out[mask,]=sp@coords
#   } else {
#     out <- c(NA, NA)
#   }
#   out
# }
# 
# domestic_abuse[, LatLong := list(list(ConvertCoordinates(snap_east, snap_north))), by = 1:nrow(domestic_abuse)]
# 
# rm(list=ls()[! ls() %in% c("all_days","all.days_new", "check","domestic_abuse", "crimes_cleaned",
#                            "worldcupeuro")])
# save.image("C:/Anna/Crimes/wm_supernew/da_location.RData")

# 
# rm(list=ls())
# load("C:/Anna/Crimes/wm_supernew/da_location.RData")
# domestic_abuse[, Latitude := unlist(domestic_abuse$LatLong)[seq(1,2*nrow(domestic_abuse),2)]]
# domestic_abuse[, Longitude := unlist(domestic_abuse$LatLong)[seq(2,2*nrow(domestic_abuse),2)]]
# LSOAs_2011 <- readOGR("LSOA_shapefiles", layer = "england_lsoa_2011")
# LSOAs_2011 <- spTransform(LSOAs_2011, CRS("+init=epsg:4326"))
# domestic_abuse <- domestic_abuse[!is.na(Latitude) | !is.na(Longitude),]
# points <- SpatialPoints(cbind(domestic_abuse$Latitude, domestic_abuse$Longitude))
# proj4string(points) <- proj4string(LSOAs_2011)
# domestic_abuse <- cbind(domestic_abuse,"LSOAs_2011" = as.factor(over(points, LSOAs_2011)$code))
# 
# lsoachanges <- data.table(read.csv("LSOA_changes.csv"))
# domestic_abuse <- merge(domestic_abuse, lsoachanges[,c("LSOA01CD", "LSOA11CD")],
#                by.x = "LSOAs_2011", by.y = "LSOA11CD", all.x = T)
# 
# 
# imd_2010 <- data.table(read.csv("imd2010.csv", sep = ","))
# imd_2015 <- data.table(read.csv("imd2015.csv", sep = ","))
# imd_2010 <- merge(imd_2010[,colnames(imd_2010)[grepl("rank|lsoa", colnames(imd_2010))], with = F],
#                   lsoachanges[, c("LSOA01CD", "LSOA11CD")],
#                   by.x = "lsoacode", by.y = "LSOA01CD", all.x = T)
# 
# imd_2010 <- imd_2010[LSOA11CD %in% unique(lsoachanges[LAD11NM %in% c("Coventry", "Dudley", "Birmingham", "Wolverhampton", "Sandwell", "Solihull", "Walsall")][, LSOA11CD]),]
# #take average by 2011 LSOAs
# imd_2010 <- imd_2010[,list(imd_rank = mean(imd_rank), employ_rank = mean(employ_rank),
#                            income_rank = mean(income_rank), healthdd_rank = mean(healthdd_rank),
#                            edust_rank = mean(edust_rank), housesb_rank = mean(housesb_rank),
#                            environl_rank = mean(environl_rank), crimed_rank = mean(crimed_rank)),.(LSOA11CD)]
# 
# colnames(imd_2015) <- tolower(colnames(imd_2015))
# imd_2015 <- imd_2015[,colnames(imd_2015)[grepl("rank|code", colnames(imd_2015))], with = F]
# imd_2015 <- imd_2015[,c(1,3:10)]
# colnames(imd_2015) <- c("LSOA11CD","imd_rank", "income_rank", "employ_rank","edust_rank",
#                         "healthdd_rank", "crimed_rank", "housesb_rank", "environl_rank")
# 
# 
# imd_2010[, to_match := 2010]
# imd_2015 <- imd_2015[LSOA11CD %in% unique(lsoachanges[LAD11NM %in% c("Coventry", "Dudley", "Birmingham", "Wolverhampton", "Sandwell", "Solihull", "Walsall")][, LSOA11CD]),]
# imd_2015[, to_match := 2015]
# imd <- rbind(imd_2010, imd_2015)
# domestic_abuse[, to_match := ifelse(year(when.committed) < 2014, 2010, 2015)]
# domestic_abuse <- merge(domestic_abuse, imd, by.x= c("LSOAs_2011", "to_match"), 
#                by.y= c("LSOA11CD", "to_match"), 
#                all.x = T)
# 
# domestic_abuse <- domestic_abuse[!is.na(LSOAs_2011),]
# domestic_abuse <- merge(domestic_abuse, crimes_cleaned[, list(location_type_general = location_type_general[1]) ,.(crime_number)],
#                         by = "crime_number", all.x = T)
# domestic_abuse[,Inside := grepl("DWELLING",location_type_general)]
# 
# domestic_abuse[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
#                                                                                                  "Mon","Tue", "Wed"))]
# 
# domestic_abuse[, year := as.factor(year(when.committed))]
# domestic_abuse[, month := factor(as.character(month(when.committed, label = TRUE)))]
# domestic_abuse[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
# domestic_abuse[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed) == 1,T,F)]
# 
# domestic_abuse[, year2 := as.numeric(levels(domestic_abuse$year)[as.numeric(domestic_abuse$year)])]
# domestic_abuse <- merge(domestic_abuse[year2 >= 2010,], unique(all_days[, c("Type.of.day", "when.committed")]),
#                         by = "when.committed", all.x = T)
# domestic_abuse <- domestic_abuse[!is.na(Type.of.day),]
# domestic_abuse[, Alcohol := factor(Alcohol)]
# domestic_abuse <- domestic_abuse[!is.na(when.committed),]
# #I can't run a spatial regression, because there are not enough incidents per day from each LSOA
# #so I am running a series of logistic regressions
# #one last try for the logistic then check for spatial autocorr
# 
# depriv <- data.table(expand.grid(when.committed = unique(all_days$when.committed),
#                        Alcohol = c("No", "Yes"), LSOAs_2011 = unique(domestic_abuse$LSOAs_2011)))
# 
# depriv <- merge(depriv, domestic_abuse[Inside == T,.N,.(when.committed,Alcohol,LSOAs_2011)],
#                 by = c("when.committed","Alcohol","LSOAs_2011"), all.x = T)
# 
# depriv <- merge(depriv, all_days[,c("when.committed", "Alcohol", "Type.of.day", "Day_of_week", "month","year", "XMAS", "NYE")],
#                 by= c("when.committed", "Alcohol"), all.x = T)
# 
# depriv <- merge(depriv, unique(domestic_abuse[,c("LSOAs_2011", "year", "employ_rank","income_rank")]),
#                 by = c("LSOAs_2011", "year"), all.x = T)
# 
# depriv[is.na(N), N:=0]
# depriv[, da:= factor(ifelse(N==0,"No", "Yes"))]
# 
# depriv[, year2 := as.numeric(levels(depriv$year)[as.numeric(depriv$year)])]
# depriv[year2 <= 2014, Employ_group := cut(employ_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                           include.lowest = T,
#                                           labels = c("Lowest", "Lower", "Higher", "Highest"))]
# depriv[year2 > 2014, Employ_group := cut(employ_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                          include.lowest = T,
#                                          labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# depriv[year2 <= 2014, Income_group := cut(income_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                           include.lowest = T,
#                                           labels = c("Lowest", "Lower", "Higher", "Highest"))]
# depriv[year2 > 2014, Income_group := cut(income_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                          include.lowest = T,
#                                          labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# # 
# # neighbours.all <- poly2nb(LSOAs_2011)
# # spatial_weights.all <- nb2listw(neighbours.all)
# # moran.test(residuals.glm(check), spatial_weights.all)
# 
# #in 2010, there were 32,482 lsoas, and in 2015, there were 32,844
# #divide each LSOA into 4 categories, based on each of the measures
# #employment, income, health, educ, house, environment
# 
# 
# 
# # domestic_abuse[year2 <= 2014, Health_group := cut(healthdd_rank, breaks = c(1, 8121, 16241,24543,32482),
# #                                                   include.lowest = T,
# #                                                   labels = c("Lowest", "Lower", "Higher", "Highest"))]
# # domestic_abuse[year2 > 2014, Health_group := cut(healthdd_rank, breaks = c(1, 8211, 16422,24543,32844),
# #                                                  include.lowest = T,
# #                                                  labels = c("Lowest", "Lower", "Higher", "Highest"))]
# # 
# # domestic_abuse[year2 <= 2014, Educ_group := cut(edust_rank, breaks = c(1, 8121, 16241,24543,32482),
# #                                                   include.lowest = T,
# #                                                   labels = c("Lowest", "Lower", "Higher", "Highest"))]
# # domestic_abuse[year2 > 2014, Educ_group := cut(edust_rank, breaks = c(1, 8211, 16422,24543,32844),
# #                                                  include.lowest = T,
# #                                                  labels = c("Lowest", "Lower", "Higher", "Highest"))]
# # 
# # domestic_abuse[year2 <= 2014, Housing_group := cut(housesb_rank, breaks = c(1, 8121, 16241,24543,32482),
# #                                                 include.lowest = T,
# #                                                 labels = c("Lowest", "Lower", "Higher", "Highest"))]
# # domestic_abuse[year2 > 2014, Housing_group := cut(housesb_rank, breaks = c(1, 8211, 16422,24543,32844),
# #                                                include.lowest = T,
# #                                                labels = c("Lowest", "Lower", "Higher", "Highest"))]
# # 
# # 
# # domestic_abuse[year2 <= 2014, Environment_group := cut(environl_rank, breaks = c(1, 8121, 16241,24543,32482),
# #                                                    include.lowest = T,
# #                                                    labels = c("Lowest", "Lower", "Higher", "Highest"))]
# # domestic_abuse[year2 > 2014, Environment_group := cut(environl_rank, breaks = c(1, 8211, 16422,24543,32844),
# #                                                   include.lowest = T,
# #                                                   labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# domestic_abuse[, year2 := as.numeric(levels(domestic_abuse$year)[as.numeric(domestic_abuse$year)])]
# domestic_abuse[year2 <= 2014, Employ_group := cut(employ_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                           include.lowest = T,
#                                           labels = c("Lowest", "Lower", "Higher", "Highest"))]
# domestic_abuse[year2 > 2014, Employ_group := cut(employ_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                          include.lowest = T,
#                                          labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# domestic_abuse[year2 <= 2014, Income_group := cut(income_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                           include.lowest = T,
#                                           labels = c("Lowest", "Lower", "Higher", "Highest"))]
# domestic_abuse[year2 > 2014, Income_group := cut(income_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                          include.lowest = T,
#                                          labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# all.days_new <- data.table(expand.grid(when.committed = unique(all_days$when.committed),
#                             Alcohol = c("No", "Yes"),
#                             Group = c("Lowest", "Lower", "Higher", "Highest")))
# 
# all.days_new <- merge(all.days_new, domestic_abuse[Inside == T, .N, .(Alcohol, when.committed, Employ_group)],
#                       by.x = c("when.committed", "Alcohol", "Group"),
#                       by.y = c("when.committed", "Alcohol", "Employ_group"),
#                       all.x = T)
# all.days_new[is.na(N), N:= 0]
# setnames(all.days_new, "N", "Employment_no")
# 
# all.days_new <- merge(all.days_new, domestic_abuse[Inside == T, .N, .(Alcohol, when.committed, Income_group)],
#                       by.x = c("when.committed", "Alcohol", "Group"),
#                       by.y = c("when.committed", "Alcohol", "Income_group"),
#                       all.x = T)
# all.days_new[is.na(N), N:= 0]
# setnames(all.days_new, "N", "Income_no")
# 
# 
# all.days_new <- merge(all.days_new, unique(all_days[,c("when.committed", "Type.of.day",
#                                                        "year", "Day_of_week", "month",
#                                                        "XMAS", "NYE")]))
# 
# all.days_new[, Alcohol := as.factor(Alcohol)]
# 
# summary(inc <- glm.nb(Income_no ~ year + Type.of.day*Alcohol*Group + Day_of_week + month + XMAS + NYE,
#                        data = all.days_new))
# 
# m3 <- data.table(summary(emmeans(inc, ~ Type.of.day*Alcohol*Group)))
# ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
#   facet_wrap(~Group, scales = "free", ncol = 2) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   labs(title = "Number of cases ~ Type.of.day*Alcohol*Neighbourhoodtype")
# ggsave("help_emmeans_incomenegbin.pdf")
# 
# 
# 
# summary(inc1 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Lowest",]))
# summary(inc2 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Lower",]))
# summary(inc3 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Higher",]))
# summary(inc4 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Highest",]))
# summary(inc1p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                        data = all.days_new[Group == "Lowest",], family = "poisson"))
# summary(inc2p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                        data = all.days_new[Group == "Lower",], family = "poisson"))
# summary(inc3p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                        data = all.days_new[Group == "Higher",], family = "poisson"))
# summary(inc4p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                        data = all.days_new[Group == "Highest",], family = "poisson"))
# 
# lrtest(inc1p, inc1)
# lrtest(inc2p, inc2)
# lrtest(inc3p, inc3)
# lrtest(inc4p, inc4)
# 
# summary(emp1 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Lowest",]))
# summary(emp2 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Lower",]))
# summary(emp3 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Higher",]))
# summary(emp4 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                data = all.days_new[Group == "Highest",]))
# 
# summary(emp1p <- glm(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                      data = all.days_new[Group == "Lowest",], family = "poisson"))
# summary(emp2p <- glm(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                      data = all.days_new[Group == "Lower",], family = "poisson"))
# summary(emp3p <- glm(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                      data = all.days_new[Group == "Higher",], family = "poisson"))
# summary(emp4p <- glm(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
#                      data = all.days_new[Group == "Highest",], family = "poisson"))
# 
# lrtest(emp1p, emp1)
# lrtest(emp2p, emp2)
# lrtest(emp3p, emp3)
# lrtest(emp4p, emp4)
# 
# summary(emp <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol*Group + Day_of_week + month + XMAS + NYE,
#                       data = all.days_new))
# 
# m3 <- data.table(summary(emmeans(emp, ~ Type.of.day*Alcohol*Group)))
# ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
#   facet_wrap(~Group, scales = "free", ncol = 2) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   labs(title = "Number of cases ~ Type.of.day*Alcohol*Neighbourhoodtype")
# ggsave("help_emmeans_employnegbin.pdf")
# 
# 
# CI.vectorsinc <- data.table(exp(confint(inc1)),exp(confint(inc2)),exp(confint(inc3)),exp(confint(inc4)))
# CI.vectorsemp <- data.table(exp(confint(emp1)),exp(confint(emp2)),exp(confint(emp3)),exp(confint(emp4)))
# 
# Coef.vectorsinc <- data.table(exp(inc1$coefficients), exp(inc2$coefficients),
#                             exp(inc3$coefficients), exp(inc4$coefficients))
# Coef.vectorsemp <- data.table(exp(emp1$coefficients), exp(emp2$coefficients),
#                               exp(emp3$coefficients), exp(emp4$coefficients))
# P.valsinc <- data.table(summary(inc1)$coefficients[,4],summary(inc2)$coefficients[,4],
#                         summary(inc3)$coefficients[,4],summary(inc4)$coefficients[,4])
# P.valsemp <- data.table(summary(emp1)$coefficients[,4],summary(emp2)$coefficients[,4],
#                                      summary(emp3)$coefficients[,4],summary(emp4)$coefficients[,4])
# 
# 
# stargazer(inc1,inc2,inc3,inc4,type = "latex",
#           omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
#           title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
#           no.space = TRUE,
#           coef = list(as.numeric(Coef.vectorsinc$V1-1),
#                       as.numeric(Coef.vectorsinc$V2-1),
#                       as.numeric(Coef.vectorsinc$V3-1),
#                       as.numeric(Coef.vectorsinc$V4-1)), 
#           p =list(P.valsinc$V1,
#                   P.valsinc$V2,
#                   P.valsinc$V3,
#                   P.valsinc$V4))
# 
# 
# stargazer(emp1,emp2,emp3,emp4,type = "latex",
#           omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
#           title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
#           no.space = TRUE,
#           coef = list(as.numeric(Coef.vectorsemp$V1-1),
#                       as.numeric(Coef.vectorsemp$V2-1),
#                       as.numeric(Coef.vectorsemp$V3-1),
#                       as.numeric(Coef.vectorsemp$V4-1)), 
#           p =list(P.valsemp$V1,
#                   P.valsemp$V2,
#                   P.valsemp$V3,
#                   P.valsemp$V4))
# 
# #same results from logit?
# summary(income <- glm(Alcohol ~ Type.of.day*Income_group + month + Day_of_week +
#                         XMAS + NYE + year, data = domestic_abuse, family = binomial(link = "logit")))
# summary(emp <- glm(Alcohol ~ Type.of.day*Employ_group + month + Day_of_week +
#                         XMAS + NYE + year, data = domestic_abuse, family = binomial(link = "logit")))
# 
# 
# m3 <- data.table(summary(emmeans(income, ~ Type.of.day*Income_group)))
# ggplot(data = m3, aes(Type.of.day, emmean, colour = Income_group)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   facet_wrap(~Income_group)+
#   labs(title = "Alcohol ~ Type.of.day*Neighbourhoodtype, (income group, logistic regression)")
# ggsave("help_emmeans_incomelogistic.pdf")
# 
# 
# m3 <- data.table(summary(emmeans(emp, ~ Type.of.day*Employ_group)))
# ggplot(data = m3, aes(Type.of.day, emmean, colour = Employ_group)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   facet_wrap(~Employ_group)+
#   labs(title = "Alcohol ~ Type.of.day*Neighbourhoodtype, (emp group, logistic regression)")
# ggsave("help_emmeans_emplogistic.pdf")
# 
# 

#check if it is men who are violent
rm(list = ls())
#setwd("C:/Users/Anna/Desktop/crimes")
setwd("C:/Anna/Crimes/wm_supernew")
load("Westmids_supernew.RData")

check <- crimes_cleaned[, list(no_vict = sum(role == "VICT"),inc_da = inc_da[1],Domestic_abuse = Domestic_abuse[1],
                                                                       no_off = sum(role != "VICT"),
                                                                       offence = offence[1],
                                                                       Offence_class = Offence_class[1],
                                                                       Offence_subclass = Offence_subclass[1]), .(crime_number)]

check[inc_da == "Yes" | Domestic_abuse != "No", AmongstDA := T]
check[is.na(AmongstDA), AmongstDA := F]
other_violence <- check[AmongstDA == F & Offence_class == "Violence Against The Person",]






#of all incidents, 
#nrow(check[no_vict == 1 & no_off ==1,])/nrow(check)
#92% of domestic abuse incidents have one victim and one offender (either male or female)
#nrow(check)/length(unique(crimes_cleaned$crime_number))
#31% of crime is domestic abuse related
other_violence <- crimes_cleaned[crime_number %in% check[no_vict == 1 & no_off ==1, crime_number],]
other_violence <- other_violence[order(crime_number, role)]
other_violence[, Gender.Type := paste0(gender,collapse = ""), .(crime_number)]
other_violence <- other_violence[order(crime_number, role)]

other_violence <- other_violence[, list(datetime_first_committed = datetime_first_committed[1],
                                        Alcohol = Alcohol_inv[1], Gender.Type = Gender.Type[1]), .(crime_number)]

other_violence <- other_violence[!grepl("U|NA", Gender.Type),]
#domestic_abuse <- domestic_abuse[!is.na(Age_difference),]


other_violence[, when.committed := ymd(substring(as.character(datetime_first_committed),1,10))]


other_violence <- other_violence[,.N,.(Alcohol, Gender.Type, when.committed)]

all.days_new <- data.table(expand.grid(when.committed = seq(ymd("2010-01-01"), ymd("2018-11-06"), by = "days"),
                                       Alcohol = c("No","Yes"), Gender.Type = c("FM", "MF", "MM", "FF", "All")))

all.days_new <- merge(all.days_new, other_violence, 
                      by = c("when.committed","Alcohol", "Gender.Type"), all.x = TRUE)
all.days_new[is.na(all.days_new)] <- 0
all.days_new[, year := year(when.committed)]

load("C:/Anna/Crimes/wm_supernew/worldcupeuro.RData")
worldcupeuro <- worldcupeuro[Team1 == "England" | Team2 == "England",]
worldcupeuro[, Goalsteam1 := as.numeric(Goalsteam1)]
worldcupeuro[, Goalsteam2 := as.numeric(Goalsteam2)]
worldcupeuro[(Team1 == "England" & Goalsteam1 > Goalsteam2) |(Team2 == "England" & Goalsteam1 < Goalsteam2),
             England_win := TRUE]
worldcupeuro[("England" == Team1 | "England" == Team2)& (Goalsteam1 == Goalsteam2),
             England_draw := TRUE]
worldcupeuro[England_draw == TRUE | England_win == TRUE, England_windraw := TRUE]
worldcupeuro[(Team1 == "England" & (Goalsteam1 < Goalsteam2)),
             England_lost := TRUE]
worldcupeuro[(Team2 == "England" & (Goalsteam2 < Goalsteam1)),
             England_lost := TRUE]
#apply(worldcupeuro[,c(8,9,11)], 1, sum, na.rm = TRUE)
worldcupeuro[Team1 == "England" | Team2 == "England" , England_played := TRUE]

worldcupeuro[, date := format(as.Date(Date, format="%d/%m/%Y")), by = 1:nrow(worldcupeuro)]
worldcupeuro[, Date := ymd(date) ]
worldcupeuro <- worldcupeuro[,c(3,6,8:13)]

all.days_new <- merge(all.days_new, worldcupeuro, by.x = "when.committed",
                      by.y = "Date", all.x = TRUE)
all.days_new[is.na(England_windraw), England_windraw := FALSE]
all.days_new[is.na(England_win), England_win := FALSE]
all.days_new[is.na(England_draw), England_draw := FALSE]
all.days_new[is.na(England_lost), England_lost := FALSE]
all.days_new[is.na(England_played), England_played := FALSE]
all.days_new[when.committed %in% worldcupeuro[England_played==TRUE,Date +1], After_England := TRUE]
all.days_new[is.na(After_England), After_England := FALSE]


all.days_new[England_win == TRUE,Type.of.day := "England win"]
all.days_new[England_draw == TRUE,Type.of.day := "England draw"]
all.days_new[England_lost == TRUE,Type.of.day := "England lost"]
all.days_new[After_England == TRUE,Type.of.day := "After England"]
all.days_new[is.na(Type.of.day),Type.of.day := "Nonmatch day"]

# all.days_new[when.committed %in% worldcupeuro[England_win==TRUE,Date +1], After_Englandw := TRUE]
# all.days_new[when.committed %in% worldcupeuro[England_draw==TRUE,Date +1], After_Englandd := TRUE]
# all.days_new[when.committed %in% worldcupeuro[England_lost==TRUE,Date +1], After_Englandl := TRUE]
# all.days_new[is.na(After_Englandw), After_Englandw := FALSE]
# all.days_new[is.na(After_Englandd), After_Englandd := FALSE]
# all.days_new[is.na(After_Englandl), After_Englandl := FALSE]
# all.days_new[After_Englandw == TRUE,Type.of.day := "After_Englandw"]
# all.days_new[After_Englandl == TRUE,Type.of.day := "After_Englandl"]
# all.days_new[After_Englandd == TRUE,Type.of.day := "After_Englandd"]



all.days_new[(when.committed >= ymd("2010-06-11") & when.committed <= ymd("2010-07-11")) | 
               (when.committed >= ymd("2014-06-12") & when.committed <= ymd("2014-07-13")) |
               (when.committed >= ymd("2012-06-08") & when.committed <= ymd("2012-07-01")) | 
               (when.committed >= ymd("2016-06-10") & when.committed <= ymd("2016-07-10")) |
               (when.committed >= ymd("2018-06-14") & when.committed <= ymd("2018-07-15")),
             Tournament_on := TRUE]
all.days_new[is.na(Tournament_on), Tournament_on := FALSE]
all.days_new[, Type.of.day := ifelse(Type.of.day == "Nonmatch day" & Tournament_on == TRUE,
                                     "Tournament on",Type.of.day)]
all.days_new[,Type.of.day := as.factor(Type.of.day)]

all.days_new[,Type.of.day := factor(Type.of.day, levels = c("Nonmatch day","Tournament on", "England win","England draw",
                                                            "England lost","After England"))]
all.days_new[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                                 "Mon","Tue", "Wed"))]

all.days_new[, year := as.factor(year(when.committed))]
all.days_new[, month := factor(as.character(month(when.committed, label = TRUE)))]

#all.days_new[, All := FF + FM + MF + MM]

#get rid of 2018 11 06 and second half of 2017
all.days_new <- all.days_new[!(when.committed == ymd("2018-11-06") | (when.committed >= ymd("2017-06-01") & when.committed <= ymd("2017-12-31"))),]
all.days_new[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
all.days_new[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed)==1,T,F)]

#all.days_new[, All := sum(N), .(when.committed, Alcohol)]
#all.days_new[, Allall := sum(N), .(when.committed, Alcohol)]

all.days_new[Gender.Type == "All", N := 0]
all.days_new[Gender.Type == "All", N := sum(N),.(when.committed, Alcohol)]

all.days_new <- merge(all.days_new, all.days_new[, list(All = sum(N)),.(when.committed, Alcohol)],
by = c("when.committed", "Alcohol"), all.x=T)

m.mmnb <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "MM",])
m.mfnb <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "MF",])
m.fmnb <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "FM",])
m.ffnb <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "FF",])
m.allnb <- glm.nb(All ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "All",])

m.mmp <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "MM",], family = "poisson")
m.mfp <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "MF",], family = "poisson")
m.fmp <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "FM",], family = "poisson")
m.ffp <- glm(N ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "FF",], family = "poisson")
m.allp <- glm(All ~ year*Alcohol + Type.of.day*Alcohol + Day_of_week*Alcohol + month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new[Gender.Type == "All",], family = "poisson")

lrtest(m.mmp, m.mmnb)
lrtest(m.fmp, m.fmnb)
lrtest(m.ffp, m.ffnb)
lrtest(m.mfp, m.mfnb)
lrtest(m.allp, m.allnb)

dispersiontest(m.mmp)
dispersiontest(m.fmp)
dispersiontest(m.ffp)
dispersiontest(m.mfp)
dispersiontest(m.allp)


Coef.vectors <- data.table(exp(m.mmnb$coefficients),
                           exp(m.mfnb$coefficients), exp(m.fmnb$coefficients),
                           exp(m.ffnb$coefficients))
P.vals <- data.table(summary(m.mmnb)$coefficients[,4],
                     summary(m.mfnb)$coefficients[,4],summary(m.fmnb)$coefficients[,4],
                     summary(m.ffnb)$coefficients[,4])
names(m.mmnb$coefficients) <- gsub("Type.of.day|Yes", "", names(m.mmnb$coefficients) )
names(m.mfnb$coefficients) <- gsub("Type.of.day|Yes", "", names(m.mfnb$coefficients) )
names(m.ffnb$coefficients) <- gsub("Type.of.day|Yes", "", names(m.ffnb$coefficients) )
names(m.fmnb$coefficients) <- gsub("Type.of.day|Yes", "", names(m.fmnb$coefficients) )

stargazer(m.mmnb,m.mfnb,m.ffnb,m.fmnb,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          no.space = TRUE,
          #covariate.labels = gsub("Type.of.day|Yes","",names(m.fmnb$coefficients)[grepl("Type|Alcohol",names(m.fmnb$coefficients) )]),
          #covariate.labels = gsub("Yes|Type.of.day", "", names(m.fmnb$coefficients)[grepl("Type",names(m.fmnb$coefficients))]),
          column.labels = c("Male to Male", "Male to Female", "Female to Female", "Female to Male"),
          coef = list(as.numeric(Coef.vectors$V1)-1,
                      as.numeric(Coef.vectors$V2)-1,
                      as.numeric(Coef.vectors$V3)-1,
                      as.numeric(Coef.vectors$V4)-1), 
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3,
                  P.vals$V4))


