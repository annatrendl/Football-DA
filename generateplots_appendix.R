###############ORIGINAL

rm(list = ls())
#setwd("C:/Users/Anna/Desktop/crimes")
library(data.table)
library(lubridate)
library(ggplot2)
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
               (when.committed >= ymd("2016-06-10") & when.committed <= ymd("2016-07-10")), Tournament_on := TRUE]
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
all.days_new[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) > 23,T,F)]
all.days_new[, NYE := ifelse(month(when.committed) == 11 & mday(when.committed) == 1,T,F)]



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
summary(kirby3 <- glm.nb(da_count ~ year + Type.of.day2 + Day_of_week + month, data = lancashire_counts))


names(kirby3$coefficients) <- gsub("day2","day",names(kirby3$coefficients))
names(kirby2$coefficients) <- gsub("day2","day",names(kirby2$coefficients))

CI.vectors <- data.table(exp(confint(kirby1)),exp(confint(kirby2)),exp(confint(kirby3)))
Coef.vectors <- data.table(exp(kirby1$coefficients), exp(kirby2$coefficients),exp(kirby3$coefficients))
P.vals <- data.table(summary(kirby1)$coefficients[,4],summary(kirby2)$coefficients[,4],
                     summary(kirby3)$coefficients[,4])

stargazer(kirby1,kirby2,kirby3,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          column.labels = c("Original Model", "Win/Draw Separate", "Month added"),
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1),
                      as.numeric(Coef.vectors$V3-1)), 
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3))

######################################################### ROBUSTNESS

summary(all <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                        month + XMAS + NYE, data = all.days_new))
summary(excl18 <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2018,]))
summary(excl16 <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2016,]))
summary(excl14 <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2014,]))
summary(excl12 <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2012,]))
summary(excl10 <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week +
                           month + XMAS + NYE, data = all.days_new[year!=2010,]))


CI.vectors <- data.table(exp(confint(all)),exp(confint(excl18)),exp(confint(excl16)),exp(confint(excl14)),
                         exp(confint(excl12)),exp(confint(excl10)))
Coef.vectors <- data.table(exp(all$coefficients), exp(excl18$coefficients),exp(excl16$coefficients),
                           exp(excl14$coefficients), exp(excl12$coefficients),exp(excl10$coefficients))
P.vals <- data.table(summary(all)$coefficients[,4],summary(excl18)$coefficients[,4],
                     summary(excl16)$coefficients[,4],summary(excl14)$coefficients[,4],
                     summary(excl12)$coefficients[,4],summary(excl10)$coefficients[,4])


stargazer(all,excl18,excl16,excl14,excl12,excl10,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,covariate.labels = gsub("Type.of.day|Yes","",names(all$coefficients)[grepl("Type|Alcohol",names(all$coefficients) )]),
          column.labels = c("All years", "2018 excluded", "2016 excluded", "2014 excluded",
                            "2012 excluded", "2010 excluded"),
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1),
                      as.numeric(Coef.vectors$V3-1),
                      as.numeric(Coef.vectors$V4-1),
                      as.numeric(Coef.vectors$V5-1),
                      as.numeric(Coef.vectors$V6-1)), 
          # ci.custom = list(rbind(CI.vectors[,1:2],
          #                        CI.vectors[,3:4],
          #                        CI.vectors[,5:6],
          #                        CI.vectors[,7:8])),
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3,
                  P.vals$V4,
                  P.vals$V5,P.vals$V6))

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
summary(da <- glm.nb(Domestic_Abuse ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data =all_days))
summary(sexual <- glm.nb(Sexual ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data =all_days))
summary(vulnerable <- glm.nb(OtherAbuse ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data =all_days))

summary(dap <- glm(Domestic_Abuse ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data =all_days, family = "poisson"))
summary(sexualp <- glm(Sexual ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data =all_days, family = "poisson"))
summary(vulnerablep <- glm(OtherAbuse ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data =all_days, family = "poisson"))

lrtest(dap,da)
lrtest(sexualp,sexual)
lrtest(vulnerablep,vulnerable)

CI.vectors <- data.table(exp(confint(da)),exp(confint(sexual)),exp(confint(vulnerable)))
Coef.vectors <- data.table(exp(da$coefficients), exp(sexual$coefficients),
                           exp(vulnerable$coefficients))
P.vals <- data.table(summary(da)$coefficients[,4],summary(sexual)$coefficients[,4],
                     summary(vulnerable)$coefficients[,4])

stargazer(da,sexual,vulnerable,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          #column.labels = c("Domestic Abuse", "Child/Vulnerable Adult Abuse", "Sexual abuse"),
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1),
                      as.numeric(Coef.vectors$V3-1)), 
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3))


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
all_days[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) > 23,T,F)]
all_days[, NYE := ifelse(month(when.committed) == 11 & mday(when.committed) == 1,T,F)]

#all_days <- all_days[,c(1:2,4,15,17:21)]



DP.nbfootballalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
DP.nbfootballnalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])

DP.nbrugbyalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day2+ Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
DP.nbrugbynalc <- glm.nb(Domestic_Abuse ~ year + Type.of.day2 + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])

CI.vectorsf <- data.table(exp(confint(DP.nbfootballalc)),exp(confint(DP.nbfootballnalc)))
CI.vectorsr <- data.table(exp(confint(DP.nbrugbyalc)),exp(confint(DP.nbrugbynalc)))
Coef.vectorsf <- data.table(exp(DP.nbfootballalc$coefficients), exp(DP.nbfootballnalc$coefficients))
Coef.vectorsr <- data.table(exp(DP.nbrugbyalc$coefficients), exp(DP.nbrugbynalc$coefficients))
P.valsf <- data.table(summary(DP.nbfootballalc)$coefficients[,4],summary(DP.nbfootballnalc)$coefficients[,4])
P.valsr <- data.table(summary(DP.nbrugbyalc)$coefficients[,4],summary(DP.nbrugbynalc)$coefficients[,4])



names(DP.nbrugbyalc$coefficients) <- gsub("2","",names(DP.nbrugbyalc$coefficients))
names(DP.nbrugbynalc$coefficients) <- gsub("2","",names(DP.nbrugbynalc$coefficients))


stargazer(DP.nbfootballnalc,DP.nbfootballalc,DP.nbrugbynalc,DP.nbrugbyalc,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,covariate.labels = gsub("Type.of.day","",names(DP.nbfootballnalc$coefficients)[grepl("Type",names(DP.nbfootballnalc$coefficients) )]),
          column.labels = c("All years", "2018 excluded", "2016 excluded", "2014 excluded"),
          coef = list(as.numeric(Coef.vectorsf$V1-1),
                      as.numeric(Coef.vectorsf$V2-1),
                      as.numeric(Coef.vectorsr$V1-1),
                      as.numeric(Coef.vectorsr$V2-1)), 
          p =list(P.valsf$V1,
                  P.valsf$V2,
                  P.valsr$V1,
                  P.valsr$V2))

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


rm(list=ls())
library(rgdal)
library(spdep)
load("C:/Anna/Crimes/wm_supernew/da_location.RData")
domestic_abuse[, Latitude := unlist(domestic_abuse$LatLong)[seq(1,2*nrow(domestic_abuse),2)]]
domestic_abuse[, Longitude := unlist(domestic_abuse$LatLong)[seq(2,2*nrow(domestic_abuse),2)]]
LSOAs_2011 <- readOGR("LSOA_shapefiles", layer = "england_lsoa_2011")
LSOAs_2011 <- spTransform(LSOAs_2011, CRS("+init=epsg:4326"))
domestic_abuse <- domestic_abuse[!is.na(Latitude) | !is.na(Longitude),]
points <- SpatialPoints(cbind(domestic_abuse$Latitude, domestic_abuse$Longitude))
proj4string(points) <- proj4string(LSOAs_2011)
domestic_abuse <- cbind(domestic_abuse,"LSOAs_2011" = as.factor(over(points, LSOAs_2011)$code))

lsoachanges <- data.table(read.csv("LSOA_changes.csv"))
domestic_abuse <- merge(domestic_abuse, lsoachanges[,c("LSOA01CD", "LSOA11CD")],
               by.x = "LSOAs_2011", by.y = "LSOA11CD", all.x = T)


imd_2010 <- data.table(read.csv("imd2010.csv", sep = ","))
imd_2015 <- data.table(read.csv("imd2015.csv", sep = ","))
imd_2010 <- merge(imd_2010[,colnames(imd_2010)[grepl("rank|lsoa", colnames(imd_2010))], with = F],
                  lsoachanges[, c("LSOA01CD", "LSOA11CD")],
                  by.x = "lsoacode", by.y = "LSOA01CD", all.x = T)

imd_2010 <- imd_2010[LSOA11CD %in% unique(lsoachanges[LAD11NM %in% c("Coventry", "Dudley", "Birmingham", "Wolverhampton", "Sandwell", "Solihull", "Walsall")][, LSOA11CD]),]
#take average by 2011 LSOAs
imd_2010 <- imd_2010[,list(imd_rank = mean(imd_rank), employ_rank = mean(employ_rank),
                           income_rank = mean(income_rank), healthdd_rank = mean(healthdd_rank),
                           edust_rank = mean(edust_rank), housesb_rank = mean(housesb_rank),
                           environl_rank = mean(environl_rank), crimed_rank = mean(crimed_rank)),.(LSOA11CD)]

colnames(imd_2015) <- tolower(colnames(imd_2015))
imd_2015 <- imd_2015[,colnames(imd_2015)[grepl("rank|code", colnames(imd_2015))], with = F]
imd_2015 <- imd_2015[,c(1,3:10)]
colnames(imd_2015) <- c("LSOA11CD","imd_rank", "income_rank", "employ_rank","edust_rank",
                        "healthdd_rank", "crimed_rank", "housesb_rank", "environl_rank")


imd_2010[, to_match := 2010]
imd_2015 <- imd_2015[LSOA11CD %in% unique(lsoachanges[LAD11NM %in% c("Coventry", "Dudley", "Birmingham", "Wolverhampton", "Sandwell", "Solihull", "Walsall")][, LSOA11CD]),]
imd_2015[, to_match := 2015]
imd <- rbind(imd_2010, imd_2015)
domestic_abuse[, to_match := ifelse(year(when.committed) < 2014, 2010, 2015)]
domestic_abuse <- merge(domestic_abuse, imd, by.x= c("LSOAs_2011", "to_match"), 
               by.y= c("LSOA11CD", "to_match"), 
               all.x = T)

domestic_abuse <- domestic_abuse[!is.na(LSOAs_2011),]
domestic_abuse <- merge(domestic_abuse, crimes_cleaned[, list(location_type_general = location_type_general[1]) ,.(crime_number)],
                        by = "crime_number", all.x = T)
domestic_abuse[,Inside := grepl("DWELLING",location_type_general)]

domestic_abuse[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                                 "Mon","Tue", "Wed"))]

domestic_abuse[, year := as.factor(year(when.committed))]
domestic_abuse[, month := factor(as.character(month(when.committed, label = TRUE)))]
domestic_abuse[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) > 23,T,F)]
domestic_abuse[, NYE := ifelse(month(when.committed) == 11 & mday(when.committed) == 1,T,F)]

domestic_abuse[, year2 := as.numeric(levels(domestic_abuse$year)[as.numeric(domestic_abuse$year)])]
domestic_abuse <- merge(domestic_abuse[year2 >= 2010,], unique(all_days[, c("Type.of.day", "when.committed")]),
                        by = "when.committed", all.x = T)
domestic_abuse <- domestic_abuse[!is.na(Type.of.day),]
domestic_abuse[, Alcohol := factor(Alcohol)]
domestic_abuse <- domestic_abuse[!is.na(when.committed),]
#I can't run a spatial regression, because there are not enough incidents per day from each LSOA
#so I am running a series of logistic regressions


#in 2010, there were 32,482 lsoas, and in 2015, there were 32,844
#divide each LSOA into 4 categories, based on each of the measures
#employment, income, health, educ, house, environment

domestic_abuse[year2 <= 2014, Employ_group := cut(employ_rank, breaks = c(1, 8121, 16241,24543,32482),
              include.lowest = T,
              labels = c("Lowest", "Lower", "Higher", "Highest"))]
domestic_abuse[year2 > 2014, Employ_group := cut(employ_rank, breaks = c(1, 8211, 16422,24543,32844),
              include.lowest = T,
              labels = c("Lowest", "Lower", "Higher", "Highest"))]

domestic_abuse[year2 <= 2014, Income_group := cut(income_rank, breaks = c(1, 8121, 16241,24543,32482),
                                                  include.lowest = T,
                                                  labels = c("Lowest", "Lower", "Higher", "Highest"))]
domestic_abuse[year2 > 2014, Income_group := cut(income_rank, breaks = c(1, 8211, 16422,24543,32844),
                                                 include.lowest = T,
                                                 labels = c("Lowest", "Lower", "Higher", "Highest"))]

# domestic_abuse[year2 <= 2014, Health_group := cut(healthdd_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                                   include.lowest = T,
#                                                   labels = c("Lowest", "Lower", "Higher", "Highest"))]
# domestic_abuse[year2 > 2014, Health_group := cut(healthdd_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                                  include.lowest = T,
#                                                  labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# domestic_abuse[year2 <= 2014, Educ_group := cut(edust_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                                   include.lowest = T,
#                                                   labels = c("Lowest", "Lower", "Higher", "Highest"))]
# domestic_abuse[year2 > 2014, Educ_group := cut(edust_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                                  include.lowest = T,
#                                                  labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# domestic_abuse[year2 <= 2014, Housing_group := cut(housesb_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                                 include.lowest = T,
#                                                 labels = c("Lowest", "Lower", "Higher", "Highest"))]
# domestic_abuse[year2 > 2014, Housing_group := cut(housesb_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                                include.lowest = T,
#                                                labels = c("Lowest", "Lower", "Higher", "Highest"))]
# 
# 
# domestic_abuse[year2 <= 2014, Environment_group := cut(environl_rank, breaks = c(1, 8121, 16241,24543,32482),
#                                                    include.lowest = T,
#                                                    labels = c("Lowest", "Lower", "Higher", "Highest"))]
# domestic_abuse[year2 > 2014, Environment_group := cut(environl_rank, breaks = c(1, 8211, 16422,24543,32844),
#                                                   include.lowest = T,
#                                                   labels = c("Lowest", "Lower", "Higher", "Highest"))]



all.days_new <- data.table(expand.grid(when.committed = unique(all_days$when.committed),
                            Alcohol = c("No", "Yes"),
                            Group = c("Lowest", "Lower", "Higher", "Highest")))

all.days_new <- merge(all.days_new, domestic_abuse[Inside == T, .N, .(Alcohol, when.committed, Employ_group)],
                      by.x = c("when.committed", "Alcohol", "Group"),
                      by.y = c("when.committed", "Alcohol", "Employ_group"),
                      all.x = T)
all.days_new[is.na(N), N:= 0]
setnames(all.days_new, "N", "Employment_no")

all.days_new <- merge(all.days_new, domestic_abuse[Inside == T, .N, .(Alcohol, when.committed, Income_group)],
                      by.x = c("when.committed", "Alcohol", "Group"),
                      by.y = c("when.committed", "Alcohol", "Income_group"),
                      all.x = T)
all.days_new[is.na(N), N:= 0]
setnames(all.days_new, "N", "Income_no")


all.days_new <- merge(all.days_new, unique(all_days[,c("when.committed", "Type.of.day",
                                                       "year", "Day_of_week", "month",
                                                       "XMAS", "NYE")]))
all.days_new[, Alcohol := as.factor(Alcohol)]




summary(inc1 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Lowest",]))
summary(inc2 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Lower",]))
summary(inc3 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Higher",]))
summary(inc4 <- glm.nb(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Highest",]))
summary(inc1p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                       data = all.days_new[Group == "Lowest",], family = "poisson"))
summary(inc2p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                       data = all.days_new[Group == "Lower",], family = "poisson"))
summary(inc3p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                       data = all.days_new[Group == "Higher",], family = "poisson"))
summary(inc4p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                       data = all.days_new[Group == "Highest",], family = "poisson"))

lrtest(inc1p, inc1)
lrtest(inc2p, inc2)
lrtest(inc3p, inc3)
lrtest(inc4p, inc4)

summary(emp1 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Lowest",]))
summary(emp2 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Lower",]))
summary(emp3 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Higher",]))
summary(emp4 <- glm.nb(Employment_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
               data = all.days_new[Group == "Highest",]))

summary(emp1p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                     data = all.days_new[Group == "Lowest",], family = "poisson"))
summary(emp2p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                     data = all.days_new[Group == "Lower",], family = "poisson"))
summary(emp3p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                     data = all.days_new[Group == "Higher",], family = "poisson"))
summary(emp4p <- glm(Income_no ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE,
                     data = all.days_new[Group == "Highest",], family = "poisson"))

lrtest(emp1p, emp1)
lrtest(emp2p, emp2)
lrtest(emp3p, emp3)
lrtest(emp4p, emp4)


CI.vectorsinc <- data.table(exp(confint(inc1)),exp(confint(inc2)),exp(confint(inc3)),exp(confint(inc4)))
CI.vectorsemp <- data.table(exp(confint(emp1)),exp(confint(emp2)),exp(confint(emp3)),exp(confint(emp4)))

Coef.vectorsinc <- data.table(exp(inc1$coefficients), exp(inc2$coefficients),
                            exp(inc3$coefficients), exp(inc4$coefficients))
Coef.vectorsemp <- data.table(exp(emp1$coefficients), exp(emp2$coefficients),
                              exp(emp3$coefficients), exp(emp4$coefficients))
P.valsinc <- data.table(summary(inc1)$coefficients[,4],summary(inc2)$coefficients[,4],
                        summary(inc3)$coefficients[,4],summary(inc4)$coefficients[,4])
P.valsemp <- data.table(summary(emp1)$coefficients[,4],summary(emp2)$coefficients[,4],
                                     summary(emp3)$coefficients[,4],summary(emp4)$coefficients[,4])


stargazer(inc1,inc2,inc3,inc4,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          coef = list(as.numeric(Coef.vectorsinc$V1-1),
                      as.numeric(Coef.vectorsinc$V2-1),
                      as.numeric(Coef.vectorsinc$V3-1),
                      as.numeric(Coef.vectorsinc$V4-1)), 
          p =list(P.valsinc$V1,
                  P.valsinc$V2,
                  P.valsinc$V3,
                  P.valsinc$V4))


stargazer(emp1,emp2,emp3,emp4,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          coef = list(as.numeric(Coef.vectorsemp$V1-1),
                      as.numeric(Coef.vectorsemp$V2-1),
                      as.numeric(Coef.vectorsemp$V3-1),
                      as.numeric(Coef.vectorsemp$V4-1)), 
          p =list(P.valsemp$V1,
                  P.valsemp$V2,
                  P.valsemp$V3,
                  P.valsemp$V4))


# summary(glm(Alcohol ~ Type.of.day*Employ_group + as.factor(year) + month +
#               Day_of_week + XMAS + NYE, family = binomial(link = "logit"), 
#             data = domestic_abuse[Inside==T,]))
# summary(glm(Alcohol ~ Type.of.day*Health_group + as.factor(year) + month +
#               Day_of_week + XMAS + NYE, family = binomial(link = "logit"), 
#             data = domestic_abuse[Inside==T,]))
# summary(glm(Alcohol ~ Type.of.day*Educ_group + as.factor(year) + month +
#               Day_of_week + XMAS + NYE, family = binomial(link = "logit"), 
#             data = domestic_abuse[Inside==T,]))
# summary(glm(Alcohol ~ Type.of.day*Housing_group + as.factor(year) + month +
#               Day_of_week + XMAS + NYE, family = binomial(link = "logit"), 
#             data = domestic_abuse[Inside==T,]))
# summary(glm(Alcohol ~ Type.of.day*Environment_group + as.factor(year) + month +
#               Day_of_week + XMAS + NYE, family = binomial(link = "logit"), 
#             data = domestic_abuse[Inside==T,]))

