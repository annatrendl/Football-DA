###############ORIGINAL

rm(list = ls())
#setwd("C:/Users/Anna/Desktop/crimes")
library(data.table)
library(lubridate)
library(ggplot2)
library(emmeans)
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
all.days_new <- rbind(all.days_new, all.days_new)
all.days_new <- all.days_new[order(when.committed, Alcohol)]

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


###############################CHARACTERISTICS#################################

load("DA_ids.RData")
check <- unique(domestic_abuse[, c("crime_number", "ids", "datetime_first_committed")])
#problem: sometimes they would be charged with two crimes at once
#get first crime of each time
check <- check[,list(crime_number = crime_number[1]), .(datetime_first_committed, ids)]
check <- check[order(datetime_first_committed)]
check <- check[order(ids, datetime_first_committed)]
check[, one.only := max(.N), .(ids)]
check[one.only > 1, how.many.before := 1:.N,.(ids)]
check[, how.many.before := how.many.before-1]
check[is.na(how.many.before), how.many.before := 0]
check[, delay_before_next := 0]
check[, year := year(datetime_first_committed)]
#check_only <- check[year >= 2010 & !is.na(year),]
#create will there be another one variable
check[, month := month(datetime_first_committed, label = T)]
check[, day := day(datetime_first_committed)]
check[, when.committed := ymd(paste(year, month, day))]
check[, Day_of_week := wday(when.committed, label = T)]
check[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
check[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed) == 1,T,F)]



check[when.committed %in% unique( all.days_new[England_win == T,when.committed]), Type := "England_win"]
check[when.committed %in% unique( all.days_new[England_lost == T,when.committed]), Type := "England_lost"]
check[when.committed %in% unique( all.days_new[England_draw == T,when.committed]), Type := "England_draw"]
check[when.committed %in% unique( all.days_new[After_England == T,when.committed]), Type := "After_England"]
check[when.committed %in% unique( all.days_new[Tournament_on == T,when.committed]) & is.na(Type), Type := "Tournament_on"]
check[is.na(Type), Type := "Nonmatch_day"]

check[,Type := factor(Type, levels = c("Nonmatch_day","Tournament_on", "England_win","England_draw",
                                       "England_lost","After_England"))]
check <- merge(check,crimes_cleaned[crime_number %in% check$crime_number & role == "VICT",
                                    c("crime_number", "offence", "Offence_class", "Alcohol_inv",
                                      "Domestic_abuse", "inc_da", "DA", "location_type_general", "Injury.code",
                                      "Injury_class")],
               by = "crime_number", all.x = T)


check[, Locationpublic := !grepl("DWELLING", location_type_general)]
check[, Newlyreported := how.many.before == 0]
check[, Alcohol := as.factor(Alcohol_inv)]
check[, Violent := grepl("ASSAULT|MALICIOUS|GBH|WOUND|RAPE|MURDER|MANSLAUGHTER", offence)]
check[, Serious := ifelse(!(Injury_class %in% c("no injury", "threat")),T,F)]
check[, year2 := factor(year)]
check$Day_of_week <- factor(check$Day_of_week, ordered = F)
check$month <- factor(check$month, ordered = F)

library(rms)
fitnew=lrm(Newlyreported ~year2*Alcohol + Type*Alcohol + Day_of_week*Alcohol +
          month*Alcohol + XMAS*Alcohol + NYE*Alcohol, x=T, y=T, data=check[year > 2010,])
resnew1<- robcov(fitnew, cluster=check[year > 2010,ids])
resnew2<- bootcov(fitnew,cluster=check[year > 2010,ids])

fitloc=lrm(Locationpublic ~year2*Alcohol + Type*Alcohol + Day_of_week*Alcohol +
             month*Alcohol + XMAS*Alcohol + NYE*Alcohol, x=T, y=T, data=check[year > 2010,])
resloc1<- robcov(fitloc, cluster=check[year > 2010,ids])
resloc2<- bootcov(fitloc,cluster=check[year > 2010,ids])

fitser=lrm(Serious ~year2*Alcohol + Type*Alcohol + Day_of_week*Alcohol +
             month*Alcohol + XMAS*Alcohol + NYE*Alcohol, x=T, y=T, data=check[year > 2010,])
resser1<- robcov(fitser, cluster=check[year > 2010,ids])
resser2<- bootcov(fitser,cluster=check[year > 2010,ids])






summary(newlylev <- glmer(Newlyreported ~as.factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol +
                         month*Alcohol + XMAS*Alcohol + NYE*Alcohol+ (1|ids), data = check[year > 2010,], family = binomial(link = "logit")))
summary(loclev <- glmer(Locationpublic ~as.factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol+ (1|ids), data = check[year >= 2010,], family = binomial(link = "logit")))
summary(seriouslev <- glmer(Serious ~as.factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol +
                           month*Alcohol + XMAS*Alcohol + NYE*Alcohol+ (1|ids), data = check[year >= 2010,], family = binomial(link = "logit")))


# m1 <- data.table(summary(emmeans(loc, ~ Type*Alcohol)))
# ggplot(data = m1, aes(Type, emmean, colour = Alcohol)) +
#    geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +  
#   #facet_wrap(~Locationpublic, scales = "free")+
#    labs(title = "DA ~ Type.of.day*Alcohol*Locationpublic")+
#    theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# 
# m1 <- data.table(summary(emmeans(serious, ~ Type*Alcohol)))
# ggplot(data = m1, aes(Type, emmean, colour = Alcohol)) +
#   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +  
#   #facet_wrap(~Locationpublic, scales = "free")+
#   labs(title = "DA ~ Type.of.day*Alcohol*Locationpublic")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# setnames(all.days_new, "N", "All")
# ##################Location
# all.days_new[, Locationpublic := rep(c(T,F), length(unique(all.days_new$when.committed))*2)]
# all.days_new <- merge(all.days_new,check[,.N,.(Alcohol, when.committed,Locationpublic)],
#                       by = c("when.committed", "Locationpublic", "Alcohol"), all.x = T)
# all.days_new[is.na(N),N:=0]
# 
# 
# summary(m3 <- glm.nb(N ~ year*Alcohol + Type.of.day*Alcohol*Locationpublic + Day_of_week*Alcohol +
#                         month*Alcohol + XMAS*Alcohol + NYE*Alcohol, data = all.days_new))
# m3 <- data.table(summary(emmeans(m3, ~Type.of.day*Alcohol*Locationpublic)))
# # 
# # ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
# #   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
# #   facet_wrap(~Locationpublic, scales = "free")+
# #   labs(title = "DA ~ Type.of.day*Alcohol*Locationpublic")+
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # ggsave("help_emmeans_locationtypeint.pdf")
# 
# summary(locnalc <- glm.nb(N ~ year + Type.of.day*Locationpublic + Day_of_week +
#                            month + XMAS + NYE, data = all.days_new[Alcohol == "No",]))
# summary(localc <- glm.nb(N ~ year + Type.of.day*Locationpublic + Day_of_week +
#                             month + XMAS + NYE, data = all.days_new[Alcohol == "Yes",]))
# all.days_new <- all.days_new[, -c("Locationpublic", "N")]
# ##################Newly reported
# all.days_new <- all.days_new[order(when.committed, Alcohol)]
# all.days_new[, Newlyreported := rep(c(T,F), length(unique(all.days_new$when.committed))*2)]
# all.days_new <- merge(all.days_new,check[,.N,.(Alcohol, when.committed,Newlyreported)],
#                       by = c("when.committed", "Newlyreported", "Alcohol"), all.x = T)
# all.days_new[is.na(N),N:=0]
# 
# # summary(m3 <- glm.nb(N ~ year + Type.of.day*Alcohol*Newlyreported + Day_of_week +
# #                        month + XMAS + NYE, data = all.days_new[year != 2010,]))
# # m3 <- data.table(summary(emmeans(m3, ~Type.of.day*Alcohol*Newlyreported)))
# # 
# # ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
# #   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
# #   facet_wrap(~Newlyreported, scales = "free")+
# #   labs(title = "DA ~ Type.of.day*Alcohol*Newlyreported")+ 
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # ggsave("help_emmeans_newlyrepint.pdf")
# 
# 
# summary(newlyrepalc <- glm.nb(N ~ year + Type.of.day*Newlyreported + Day_of_week +
#                              month + XMAS + NYE, data = all.days_new[year != 2010& Alcohol == "Yes",]))
# summary(newlyrepnalc <- glm.nb(N ~ year + Type.of.day*Newlyreported + Day_of_week +
#                              month + XMAS + NYE, data = all.days_new[year != 2010 &Alcohol == "No",]))
# all.days_new <- all.days_new[, -c("Newlyreported", "N")]
# 
# ##################Serious
# all.days_new <- all.days_new[order(when.committed, Alcohol)]
# all.days_new[, Serious := rep(c(T,F), length(unique(all.days_new$when.committed))*2)]
# all.days_new <- merge(all.days_new,check[,.N,.(Alcohol, when.committed,Serious)],
#                       by = c("when.committed", "Serious", "Alcohol"), all.x = T)
# all.days_new[is.na(N),N:=0]
# 
# 
# # summary(m3 <- glm.nb(N ~ year + Type.of.day*Alcohol*Serious + Day_of_week +
# #                        month + XMAS + NYE, data = all.days_new[year != 2010,]))
# # m3 <- data.table(summary(emmeans(m3, ~Type.of.day*Alcohol*Serious)))
# # 
# # ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
# #   geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
# #   facet_wrap(~Serious, scales = "free")+
# #   labs(title = "DA ~ Type.of.day*Alcohol*Serious")+ 
# #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# # ggsave("help_emmeans_seriousint.pdf")
# # 
# 
# summary(seriousalc <- glm.nb(N ~ year + Type.of.day*Serious + Day_of_week +
#                                 month + XMAS + NYE, data = all.days_new[Alcohol == "Yes",]))
# summary(seriousnalc <- glm.nb(N ~ year + Type.of.day*Serious + Day_of_week +
#                                  month + XMAS + NYE, data = all.days_new[Alcohol == "No",]))
# all.days_new <- all.days_new[, -c("Serious", "N")]

#######################################################################################

# CI.vectors <- data.table(exp(confint(localc)),exp(confint(locnalc)),
#                          exp(confint(newlyrepalc)),exp(confint(newlyrepnalc)),
#                          exp(confint(seriousalc)),exp(confint(seriousnalc)))
# Coef.vectors <- data.table(exp(localc$coefficients), exp(locnalc$coefficients),
#                            exp(newlyrepalc$coefficients),exp(newlyrepnalc$coefficients),
#                            exp(seriousalc$coefficients),exp(seriousnalc$coefficients))
# P.vals <- data.table(summary(localc)$coefficients[,4],summary(locnalc)$coefficients[,4],
#                      summary(newlyrepalc)$coefficients[,4],summary(newlyrepnalc)$coefficients[,4],
#                      summary(seriousalc)$coefficients[,4],summary(seriousnalc)$coefficients[,4])
# 
# names(localc$coefficients) <- gsub("Type.of.day","", names(localc$coefficients))
# names(seriousalc$coefficients) <- gsub("Type.of.day","", names(seriousalc$coefficients))
# names(newlyrepalc$coefficients) <- gsub("Type.of.day","", names(newlyrepalc$coefficients))
# names(locnalc$coefficients) <- gsub("Type.of.day","", names(locnalc$coefficients))
# names(seriousnalc$coefficients) <- gsub("Type.of.day","", names(seriousnalc$coefficients))
# names(newlyrepnalc$coefficients) <- gsub("Type.of.day","", names(newlyrepnalc$coefficients))
# 
# stargazer(localc,locnalc,
#           newlyrepalc,newlyrepnalc,
#           seriousalc, seriousnalc,type = "latex",
#           omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
#           title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
#           no.space = TRUE,
#           coef = list(as.numeric(Coef.vectors$V1-1),
#                       as.numeric(Coef.vectors$V2-1),
#                       as.numeric(Coef.vectors$V3-1)), 
#           p =list(P.vals$V1,
#                   P.vals$V2,
#                   P.vals$V3))

names(serious$coefficients) <- gsub("Type|Yes","", names(serious$coefficients))
names(loc$coefficients) <- gsub("Type|Yes","", names(loc$coefficients))
names(new$coefficients) <- gsub("Type|Yes","", names(new$coefficients))

# CI.vectors <- data.table(exp(confint(loc)),exp(confint(new)),
#                       exp(confint(serious)))
 Coef.vectors <- data.table(exp(loc$coefficients), exp(new$coefficients),
                            exp(serious$coefficients))
 P.vals <- data.table(summary(loc)$coefficients[,4],summary(new)$coefficients[,4],
                      summary(serious)$coefficients[,4])                 

 stargazer(loc, new, serious,type = "latex",
           omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
           title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
           no.space = TRUE,
           coef = list(as.numeric(Coef.vectors$V1),
                       as.numeric(Coef.vectors$V2),
                       as.numeric(Coef.vectors$V3)), 
           p =list(P.vals$V1,
                   P.vals$V2,
                   P.vals$V3))
 

 # library(lme4)
 # multilev <- glmer.nb(Daystilnext_round ~ as.factor(year) + Type*Alcohol + Day_of_week +
 #            month + XMAS + NYE + (1|ids), data = check[year >=2010,], verbose = TRUE)
 
 
 

################################## TIME DELAY BETWEEN INCIDENTS##################################

check <- check[order(ids, datetime_first_committed)]
check[, Daystilnext := c(as.numeric(difftime(tail(datetime_first_committed, -1),
                                             head(datetime_first_committed, -1), units ="days")), NA), .(ids)]
check[, Dayssincelast :=c(NA, as.numeric(difftime(tail(datetime_first_committed, -1),
                                                  head(datetime_first_committed, -1), units ="days"))), .(ids)]

check[, first_occurred := year[1],.(ids)]


check[, Repeated := ifelse(length(how.many.before) == 1, "No", "Yes"),.(ids)]
check[, Daystilnext_round := round(Daystilnext)]
check[, Dayssincelast_round := round(Dayssincelast)]



summary(nbtilnext.m <- glm.nb(Daystilnext_round ~ factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol + month*Alcohol + 
                              XMAS*Alcohol + NYE*Alcohol, data = check[year >=2010 & first_occurred >=2010 &
                              Daystilnext_round < as.numeric(quantile(check$Daystilnext_round,0.975,na.rm = T)),]))
summary(ptilnext.m <- glm(Daystilnext_round ~ year*Alcohol + Type*Alcohol + Day_of_week*Alcohol + month*Alcohol  + 
                          XMAS*Alcohol + NYE*Alcohol, data = check[year >=2010 & first_occurred >=2010 &
                          Daystilnext_round < as.numeric(quantile(check$Daystilnext_round,0.975,na.rm = T)),],
                          family = "poisson"))

library(lmtest)
lrtest(ptilnext.m,nbtilnext.m)
# 
# m3 <- data.table(summary(emmeans(nbtilnext.m, ~ Type*Alcohol)))
# ggplot(data = m3, aes(Type, emmean, colour = Alcohol)) +
#   geom_point(position=position_dodge(width=0.6)) + 
#   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.6)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   labs(title = "Daystilnext ~ Type.of.day*Alcohol")
# ggsave("help_emmeans_daystilnext.pdf")



summary(nbsincelast.m <- glm.nb(Dayssincelast_round  ~ factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol + month*Alcohol + 
                                  XMAS*Alcohol + NYE*Alcohol, data = check[year >=2010 & first_occurred >=2010 &
                                Dayssincelast_round < as.numeric(quantile(check$Dayssincelast_round,0.975,na.rm = T)),]))
summary(psincelast.m <- glm(Dayssincelast_round  ~ factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol + month*Alcohol + 
                              XMAS*Alcohol + NYE*Alcohol, data = check[year >=2010 & first_occurred >=2010 &
                            Dayssincelast_round < as.numeric(quantile(check$Dayssincelast_round,0.975,na.rm = T)),], family = "poisson"))
library(lmtest)
lrtest(psincelast.m,nbsincelast.m)


# m3 <- data.table(summary(emmeans(nbsincelast.m, ~ Type*Alcohol)))
# ggplot(data = m3, aes(Type, emmean, colour = Alcohol)) +
#   geom_point(position=position_dodge(width=0.6)) + 
#   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.6)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   labs(title = "Dayssincelast ~ Type.of.day*Alcohol")
# ggsave("help_emmeans_dayssincelast.pdf")



################################## TIME DELAY REPORTING##################################

check <- merge(check, unique(crimes_cleaned[, c("crime_number", "datetime_reported")]),
               by = "crime_number", all.x = T)
check[, Report_delay :=  round(as.numeric(difftime(datetime_reported, datetime_first_committed,
                                                   units="hours")))]
summary(reportdelaynb <- glm.nb(Report_delay ~ factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol + month*Alcohol + 
                                  XMAS*Alcohol + NYE*Alcohol, data = check[year >=2010 & 
   Report_delay < as.numeric(quantile(check$Report_delay,0.975,na.rm = T)) &
   Report_delay >= 0 & first_occurred >=2010,]))
summary(reportdelayp <- glm(Report_delay ~ factor(year)*Alcohol + Type*Alcohol + Day_of_week*Alcohol + month*Alcohol + 
                              XMAS*Alcohol + NYE*Alcohol,
                            data = check[year >=2010 & 
                            Report_delay < as.numeric(quantile(check$Report_delay,0.975,na.rm = T)) &
                            Report_delay >= 0 & first_occurred >=2010,],family = "poisson"))

lrtest(reportdelayp,reportdelaynb)

# m3 <- data.table(summary(emmeans(reportdelaynb, ~ Type*Alcohol)))
# ggplot(data = m3, aes(Type, emmean, colour = Alcohol)) +
#   geom_point(position=position_dodge(width=0.6)) + 
#   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.6)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   labs(title = "Hourselapsedtilreport ~ Type.of.day*Alcohol")
# ggsave("help_emmeans_hourstilreport.pdf")




#CI.vectors <- data.table(exp(confint(nbtilnext.m)),exp(confint(nbsincelast.m)), exp(confint(reportdelaynb)))
Coef.vectors <- data.table(exp(nbtilnext.m$coefficients),exp(nbsincelast.m$coefficients),
                           exp(reportdelaynb$coefficients))
P.vals <- data.table(c(summary(nbtilnext.m)$coefficients[,4],
                       summary(nbsincelast.m)$coefficients[,4],
                       summary(reportdelaynb)$coefficients[,4]))

names(nbtilnext.m$coefficients) <- gsub("Type|Yes","", names(nbtilnext.m$coefficients))
names(nbsincelast.m$coefficients) <- gsub("Type|Yes","", names(nbsincelast.m$coefficients))
names(reportdelaynb$coefficients) <- gsub("Type|Yes","", names(reportdelaynb$coefficients))


stargazer(nbtilnext.m,nbsincelast.m,reportdelaynb,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1),
                      as.numeric(Coef.vectors$V3-1)), 
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3))



##################################ALCOHOL TRANSITION##########################

check <- check[order(ids, datetime_first_committed)]
check[,Previous_alc := shift(Alcohol, type = "lag", fill = NA), .(ids)]
check <- check[year > 2009 & first_occurred >=2010,]


summary(transition <- glm(Alcohol ~ Type*Previous_alc + Day_of_week+ month+ XMAS + NYE+as.factor(year),
                      data = check, family = "binomial"))
#CI.vectors <- data.table(exp(confint(transition)))
Coef.vectors <- data.table(exp(transition$coefficients))
P.vals <- data.table(c(summary(transition)$coefficients[,4]))


# m3 <- data.table(summary(emmeans(transition, ~ Type*Previous_alc)))
# ggplot(data = m3, aes(Type, emmean, colour = Previous_alc)) +
#   geom_point(position=position_dodge(width=0.6)) + 
#   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.6)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
#   labs(title = "Alcohol ~ Type.of.day*Previous_alc")
# ggsave("help_emmeans_alctrans.pdf")

names(transition$coefficients) <- gsub("Type|Yes","", names(reportdelaynb$coefficients))

stargazer(transition,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          coef = list(as.numeric(Coef.vectors$V1-1)), 
          p =list(P.vals$V1))






