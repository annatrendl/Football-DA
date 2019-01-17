rm(list=ls())
library(data.table)
library(lubridate)
library(stargazer)
setwd("C:/Anna/Crimes/wm_new/football_revisited")

load("C:/Anna/Crimes/wm_new/exploratory/DA.Rdata")
domestic_abuse[is.na(Injury), Injury := "no info"]
domestic_abuse <- domestic_abuse[DA == TRUE,]

domestic_abuse <- domestic_abuse[!is.na(PID),]
check <- domestic_abuse[,list(no_off = sum(role != "VICT"), no_vict = sum(role == "VICT")), .(Crime_ID)]
check[, overall := no_off+no_vict]
nrow(check[overall == 2,])/nrow(check)
#91% has a victim and an offender
#keep only those that have at least one victim and one offender
domestic_abuse <- domestic_abuse[Crime_ID %in% check[no_vict > 0 & no_off >0, Crime_ID],]

#first I need to order the incidents by offender: first, second, etc
#then retrieve characteristics of this incident
#then find out if there were further incidents by the same offender
#first put it in chronological order by offender
offenders <- domestic_abuse[role != "VICT", ]
offenders <- offenders[order(PID, time_first_committed)]
#create which incident number
offenders[, inc.no := 1:.N, .(PID)]

#find out if there are further incidents
inc_per_row <- merge(domestic_abuse[role != "VICT", ],
                     domestic_abuse[role == "VICT",], by = "Crime_ID")
first6 <- inc_per_row[, .N,.(Class.y)][order(-N)][1:6,Class.y]
inc_per_row[, how.many := 1:.N, .(Crime_ID)]

inc_per_row <- inc_per_row[order(PID.x, time_first_committed.x)]
inc_per_row[, inc.no := 1:.N, .(PID.x)]

inc_per_row[, Class := ifelse(Class.y %in% first6, Class.y,"Other")]

#for all days, find out how many M-M, F-F, F-M, M-F
inc_per_row <- inc_per_row[!is.na(gender.x) & !is.na(gender.y),]
inc_per_row <- inc_per_row[gender.x != "U" & gender.y != "U",]
inc_per_row[, Genders := paste(gender.x, gender.y), by = 1:nrow(inc_per_row)]

inc_per_row[, Date := as.Date(time_first_committed.x)]

inc_per_row <- inc_per_row[!(is.na(age.x) | is.na(age.y)),]

inc_per_row[, Age_diff := ifelse(abs(age.x- age.y) >= 15, ">=15", "<15")]
inc_per_row[, Age_diff := factor(Age_diff)]
# crimes <- inc_per_row[!is.na(Date),list(N = .N, Sexual_Offences = sum(Class == "Sexual Offences"),
#                        Alcohol = sum(Alcohol.x == "Yes"), 
#                        DA_incident = sum(Class == "DA incidents"),
#                        Arson_and_Criminal_Damage = sum(Class == "Arson and Criminal Damage"),
#                        Violence_ATP =  sum(Class == "Violence Against The Person"),
#                        Breach_of_AO=  sum(Class == "Breach of AO"),
#                        Threatening_behaviour =  sum(Class == "Threatening Behaviour"),
#                        other =  sum(Class == "Other")),
#                  .(Date, Genders)]

inc_per_row[, Alcohol := ifelse(Alcohol.x == "Yes"| Under_influence.x == "Yes", "Yes", "No")]
crimes <- inc_per_row[, list(N = .N), .(Date, Genders, Age_diff, Alcohol)]
all <- crimes[,list(N = sum(N)),.(Date, Age_diff, Alcohol)]
all[, Genders := "All"]
crimes <- rbind(crimes, all)
crimes[, Age_diff := as.character(Age_diff)]

# all.days <- data.table(Date = rep(seq(ymd("2010-01-01"), ymd("2016-12-31"),by='days'), each = 20),
#                        Genders = rep(unique(crimes$Genders), 4*length(seq(ymd("2010-01-01"),ymd("2016-12-31"),by='days'))),
#                        Age_diff = rep(rep(c("<15", ">=15"), each = 10), 2*length(seq(ymd("2010-01-01"),ymd("2016-12-31"),by='days'))),
#                        Alcohol = rep(rep(c("Yes", "No"), each = 20), length(seq(ymd("2010-01-01"),ymd("2016-12-31"),by='days'))))

all.days <- data.table(expand.grid(Date = seq(ymd("2010-01-01"), ymd("2016-12-31"),by='days'),
            Genders = unique(crimes$Genders), Age_diff = c("<15", ">=15"),
            Alcohol = c("Yes", "No")))

all.days <- merge(all.days, crimes, by = c("Date", "Genders", "Age_diff", "Alcohol"), all.x = TRUE)

load("worldcupeuro.RData")
worldcupeuro[, Goalsteam1 := as.numeric(Goalsteam1)]
worldcupeuro[, Goalsteam2 := as.numeric(Goalsteam2)]

worldcupeuro[(Team1 == "England" & Goalsteam1 > Goalsteam2) |(Team2 == "England" & Goalsteam1 < Goalsteam2),
             England_win := TRUE]
worldcupeuro[("England" == Team1 | "England" == Team2)& (Goalsteam1 == Goalsteam2),
             England_draw := TRUE]
# worldcupeuro[(Team1 == "England" & Goalsteam1 >= Goalsteam2) | (Team2 == "England" & Goalsteam2 <= Goalsteam2),
#              England_windraw := TRUE]
worldcupeuro[England_draw == TRUE | England_win == TRUE, England_windraw := TRUE]
worldcupeuro[(Team1 == "England" & (Goalsteam1 < Goalsteam2)),
             England_lost := TRUE]
worldcupeuro[(Team2 == "England" & (Goalsteam2 < Goalsteam1)),
             England_lost := TRUE]

worldcupeuro[Team1 == "England" | Team2 == "England" , England_played := TRUE]

worldcupeuro[, date := format(as.Date(Date, format="%d/%m/%Y")), by = 1:nrow(worldcupeuro)]


worldcupeuro <- worldcupeuro[,list(Type = Type[1], England_windraw = sort(England_windraw, decreasing = TRUE)[1],
                                   England_lost = sort(England_lost, decreasing = TRUE)[1],England_draw = sort(England_draw, decreasing = TRUE)[1],
                                   England_win = sort(England_win, decreasing = TRUE)[1],
                                   England_played = sort(England_played, decreasing = TRUE)[1],
                                   Datetime = Date[1]),by = date]
worldcupeuro[, Date := ymd(date) ]
#worldcupeuro[Date %in% worldcupeuro[England_played==TRUE,Date +1], After_England := TRUE]


all.days <- merge(all.days, worldcupeuro[,-c("date")], by = "Date", all.x = TRUE)

#all.days[, After_England := shift(England_played, n = 1, fill = NA, type = "lag")]


all.days[is.na(England_windraw), England_windraw := FALSE]
all.days[is.na(England_win), England_win := FALSE]
all.days[is.na(England_draw), England_draw := FALSE]
all.days[is.na(England_lost), England_lost := FALSE]
all.days[is.na(England_played), England_played := FALSE]
all.days[Date %in% worldcupeuro[England_played==TRUE,Date +1], After_England := TRUE]
all.days[is.na(After_England), After_England := FALSE]

#create type of day variable

all.days[England_windraw == TRUE,Type.of.day := "England windraw"]
all.days[England_lost == TRUE,Type.of.day := "England lost"]
all.days[After_England == TRUE,Type.of.day := "After England"]
all.days[is.na(Type.of.day),Type.of.day := "Nonmatch day"]
all.days[England_win == TRUE,Type.of.day2 := "England win"]
all.days[England_draw == TRUE,Type.of.day2 := "England draw"]
all.days[England_lost == TRUE,Type.of.day2 := "England lost"]
all.days[After_England == TRUE,Type.of.day2 := "After England"]
all.days[is.na(Type.of.day2),Type.of.day2 := "Nonmatch day"]
all.days[(Date >= ymd("2010-06-11") & Date <= ymd("2010-07-11")) | 
           (Date >= ymd("2014-06-12") & Date <= ymd("2014-07-13")) |
           (Date >= ymd("2012-06-08") & Date <= ymd("2012-07-01")) | 
           (Date >= ymd("2016-06-10") & Date <= ymd("2016-07-10")), Tournament_on := TRUE]
all.days[is.na(Tournament_on), Tournament_on := FALSE]
all.days[, Type.of.day := ifelse(Type.of.day == "Nonmatch day" & Tournament_on == TRUE,
                                 "Tournament on",Type.of.day)]
all.days[,Type.of.day := as.factor(Type.of.day)]

all.days[,Type.of.day := factor(Type.of.day, levels = c("Nonmatch day","Tournament on", "England windraw",
                                                        "England lost","After England"))]

all.days[, Type.of.day2 := ifelse(Type.of.day2 == "Nonmatch day" & Tournament_on == TRUE,
                                  "Tournament on",Type.of.day2)]
all.days[,Type.of.day2 := as.factor(Type.of.day2)]

all.days[,Type.of.day2 := factor(Type.of.day2, levels = c("Nonmatch day","Tournament on", "England win","England draw",
                                                          "England lost","After England"))]


all.days[,Day_of_week := factor(as.character(wday(Date, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                   "Mon","Tue", "Wed"))]

all.days[, year := as.factor(year(Date))]
all.days[, month := factor(as.character(month(Date, label = TRUE)))]

all.days_restricted <- all.days[(Date >= ymd("2010-06-11") & Date <= ymd("2010-07-11")) |
                                  (Date >= ymd("2014-06-12") & Date <= ymd("2014-07-13")) |
                                  (Date >= ymd("2012-06-08") & Date <= ymd("2012-07-01")) |
                                  (Date >= ymd("2016-06-10") & Date <= ymd("2016-07-10")),]
# 
# 
# library(MASS)
# 
# all.days_restricted[Type.of.day2== "Tournament on", Type.of.day2 := "Nonmatch day"]
all.days[, Month := month(Date, label = TRUE)]
all.days[is.na(N), N := 0]


all.days[Type.of.day2 == "Tournament on", Type.of.day2 := "Nonmatch day"]
all.days[,Type.of.day2 := factor(Type.of.day2, levels = c("Nonmatch day", "England win","England draw",
                                                          "England lost","After England"))]

all.days[, Age_diff := factor(Age_diff)]
all.days[, Football_on := ifelse(Type.of.day2 %in% c("Nonmatch day", "After England"), "No", "Yes")]
all.days[, Alcohol := factor(Alcohol)]
all.days[, Football_on := factor(Football_on)]


summary(m.allp <- glm(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "All",],
                     family = "poisson"))
summary(m.mmp <- glm(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "M M",],
                     family = "poisson"))
summary(m.mfp <- glm(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "M F",],
                     family = "poisson"))
summary(m.ffp <- glm(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "F F",],
                     family = "poisson"))
summary(m.fmp <- glm(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "F M",],
                     family = "poisson"))
library(MASS)
summary(m.allnb <- glm.nb(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "All",]))
summary(m.mmnb <- glm.nb(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "M M",]))
summary(m.mfnb <- glm.nb(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "M F",]))
summary(m.ffnb <- glm.nb(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "F F",]))
summary(m.fmnb <- glm.nb(formula = N ~ Type.of.day2*Alcohol + Day_of_week + Age_diff,all.days[Genders == "F M",]))
library(lmtest)
lrtest(m.allnb, m.allp)
lrtest(m.mmp, m.mmnb)
lrtest(m.mfp, m.mfnb)
lrtest(m.ffp, m.ffnb)
lrtest(m.fmp, m.fmnb)



stargazer(m.allnb,m.mmnb,m.mfnb,m.ffnb,m.fmnb,
          font.size = "tiny",
          type = "latex",ci=TRUE, ci.level=0.95,column.sep.width = "1pt",
          omit = names(regressions_all[[1]]$coefficients)[13:29],
          out="C:/Anna/Crimes/wm_new/football_revisited/.tex")



#marginal effects
library(emmeans)
m1_margs <- emmeans(m1, ~Football_on*Alcohol)
emmip(m1,  Alcohol ~ Football_on)

emmeans(m1, pairwise ~ Football_on | Alcohol)

toplot <- emmeans(m1, pairwise ~ Football_on | Alcohol)

