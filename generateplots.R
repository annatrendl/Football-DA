############################### GENDER-TYPE COMPARISON

rm(list = ls())
#setwd("C:/Users/Anna/Desktop/crimes")
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
domestic_abuse[, Agediff := diff(age),.(crime_number)]

domestic_abuse[, Age_difference := ifelse(abs(Agediff) > 15, "More than 15", "Less than 15")]

domestic_abuse <- domestic_abuse[, list(datetime_first_committed = datetime_first_committed[1],
                                        Alcohol = Alcohol_inv[1], Gender.Type = Gender.Type[1]), .(crime_number)]

domestic_abuse <- domestic_abuse[!grepl("U|NA", Gender.Type),]
#domestic_abuse <- domestic_abuse[!is.na(Age_difference),]


domestic_abuse[, when.committed := ymd(substring(as.character(datetime_first_committed),1,10))]

allda <- domestic_abuse

domestic_abuse <- domestic_abuse[,.N,.(Alcohol, Gender.Type, when.committed)]

all <- domestic_abuse[,sum(N),.(Alcohol,when.committed)]
setnames(all, "V1", "N")
all[, Gender.Type := "All"]
domestic_abuse <- rbind(domestic_abuse, all)

domestic_abuse[, year := year(when.committed)]



all.days_new <- data.table(expand.grid(when.committed = seq(ymd("2010-01-01"), ymd("2018-11-06"), by = "days"),
                                       Alcohol = c("No","Yes"), Gender.Type = c("FM", "MF", "MM", "FF", "All")))

all.days_new <- merge(all.days_new, domestic_abuse[, -c("year")], 
                      by = c("when.committed","Alcohol", "Gender.Type"), all.x = TRUE)
all.days_new[is.na(all.days_new)] <- 0
all.days_new[, year := year(when.committed)]

descriptive <- all.days_new[Gender.Type == "All" & year >= 2010,
                              list(No_days = .N, DA_cases = sum(N)), .(year, Alcohol)]

pop <- data.table(read.csv("LSOA_pop20102017.csv", sep = ",", stringsAsFactors = F))
pop[, all := as.numeric(gsub(",","",all_ages))]
descriptive <- merge(descriptive,
                     pop[grepl("Sandwell|Birmingham|Coventry|Walsall|Wolverhampton|Dudley|Solihull", LAD11NM),sum(all),.(year)],
                     by = "year", all.x = T)
setnames(descriptive, "V1", "Population")
descriptive[, Rate := ((DA_cases/No_days)/Population)*100000]

xtable(descriptive, type = "latex")


ggplot(descriptive, aes(year, Rate, colour = Alcohol)) + geom_line()




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

#all.days_new[, All := FF + FM + MF + MM]

#get rid of 2018 11 06 and second half of 2017
all.days_new <- all.days_new[!(when.committed == ymd("2018-11-06") | (when.committed >= ymd("2017-06-01") & when.committed <= ymd("2017-12-31"))),]
all.days_new[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
all.days_new[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed)==1,T,F)]

#all.days_new[, All := sum(N), .(when.committed, Alcohol)]
#all.days_new[, Allall := sum(N), .(when.committed, Alcohol)]


summary(m1 <- glm.nb(N ~ year + Alcohol +  Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",]))
summary(m2 <- glm.nb(N ~ year + Alcohol + Type.of.day +  Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",]))
summary(m3 <- glm.nb(N ~ year + Alcohol*Type.of.day +  Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",]))

summary(m1p <- glm(N ~ year + Alcohol +  Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",], family = "poisson"))
summary(m2p <- glm(N ~ year + Alcohol + Type.of.day +  Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",], family = "poisson"))
summary(m3p <- glm(N ~ year + Alcohol*Type.of.day +  Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",], family = "poisson"))

dispersiontest(m1p)
dispersiontest(m2p)
dispersiontest(m3p)

CI.vectors1 <- data.table(exp(confint(m1)))
CI.vectors2 <- data.table(exp(confint(m2)))
CI.vectors3 <- data.table(exp(confint(m3)))

Coef.vectors1 <- data.table(exp(m1$coefficients))
Coef.vectors2 <- data.table(exp(m2$coefficients))
Coef.vectors3 <- data.table(exp(m3$coefficients))  

Pval1 <- data.table(summary(m1)$coefficients[,4])
Pval2 <- data.table(summary(m2)$coefficients[,4])
Pval3 <- data.table(summary(m3)$coefficients[,4]) 

stargazer(m1,m2,m3,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,         column.labels = c("All", "Male to Male", "Male to Female", "Female to Female", "Female to Male"),
          coef = list(as.numeric(Coef.vectors1$V1)-1,
                      as.numeric(Coef.vectors2$V1)-1,
                      as.numeric(Coef.vectors3$V1)-1), 
          p =list(Pval1$V1,
                  Pval2$V1,
                  Pval3$V1))    

m3 <- data.table(summary(emmeans(m3, ~ Type.of.day*Alcohol)))
ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
  geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  labs(title = "DA ~ Type.of.day*Alcohol")
ggsave("help_emmeans_mainres.pdf")

contrast_winlose <- emmeans(m3, ~Type.of.day*Alcohol)
m1 <- emmeans(contrast_winlose, pairwise ~ Type.of.day|Alcohol)
m1 <- summary(m1)
xtable(m1$contrasts, type = "latex")

################################### GENDER#########################
m.mmnb <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "MM",])
m.mfnb <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "MF",])
m.fmnb <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "FM",])
m.ffnb <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "FF",])
m.allnb <- glm.nb(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",])


m.mmp <- glm(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "MM",], family = "poisson")
m.mfp <- glm(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "MF",], family = "poisson")
m.fmp <- glm(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "FM",], family = "poisson")
m.ffp <- glm(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "FF",], family = "poisson")
m.allp <- glm(N ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all.days_new[Gender.Type == "All",], family = "poisson")


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


CI.vectors <- data.table(exp(confint(m.mmnb)),exp(confint(m.mfnb)),
                         exp(confint(m.fmnb)),exp(confint(m.ffnb)))
Coef.vectors <- data.table(exp(m.mmnb$coefficients),
                           exp(m.mfnb$coefficients), exp(m.fmnb$coefficients),
                           exp(m.ffnb$coefficients))
P.vals <- data.table(summary(m.mmnb)$coefficients[,4],
                     summary(m.mfnb)$coefficients[,4],summary(m.fmnb)$coefficients[,4],
                     summary(m.ffnb)$coefficients[,4])

toplot <- data.table("Variable" = rep(names(m.fmnb$coefficients)[grepl("Type", names(m.fmnb$coefficients))],4),
           "Coeff" = c(as.numeric(Coef.vectors$V1)[grepl("Type", names(m.fmnb$coefficients))],
                       as.numeric(Coef.vectors$V2)[grepl("Type", names(m.fmnb$coefficients))],
                       as.numeric(Coef.vectors$V3)[grepl("Type", names(m.fmnb$coefficients))],
                       as.numeric(Coef.vectors$V4)[grepl("Type", names(m.fmnb$coefficients))]))
toplot <- cbind(toplot, rbind(CI.vectors[grepl("Type", names(m.fmnb$coefficients)),1:2],
                              CI.vectors[grepl("Type", names(m.fmnb$coefficients)),3:4],
                              CI.vectors[grepl("Type", names(m.fmnb$coefficients)),5:6],
                              CI.vectors[grepl("Type", names(m.fmnb$coefficients)),7:8]))
 toplot[, Model := rep(c("Male to Male", "Male to Female", "Female to Male", "Female to Female"), each = 10)]
 toplot[, Variable := gsub("Type.of.day","",Variable)]
 setnames(toplot, c("2.5 %", "97.5 %"), c("Lower", "Upper"))
 toplot[, Variable := factor(Variable, levels = rev(unique(toplot$Variable)))]


ggplot(toplot, aes(Variable,Coeff, group = factor(Model), colour = factor(Model))) +
  geom_hline(aes(yintercept = 1), linetype = 4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.7), size = 1.5) +
  geom_point(position=position_dodge(width=0.7), size = 1.5, colour = "black") +
  coord_flip() + theme_classic()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=19))+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(text = element_text(size=25),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20)) +
  theme(legend.position = "bottom")+
  labs(x = "", y= "Coefficient")+
  scale_color_manual(values=brewer.pal(n = 8, name = "Set1"))+
  guides(colour=guide_legend(nrow=2,byrow=TRUE))



stargazer(m.mmnb,m.mfnb,m.ffnb,m.fmnb,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,
          covariate.labels = gsub("Type.of.day|Yes","",names(m.fmnb$coefficients)[grepl("Type|Alcohol",names(m.fmnb$coefficients) )]),
          column.labels = c("Male to Male", "Male to Female", "Female to Female", "Female to Male"),
          coef = list(as.numeric(Coef.vectors$V1)-1,
                      as.numeric(Coef.vectors$V2)-1,
                      as.numeric(Coef.vectors$V3)-1,
                      as.numeric(Coef.vectors$V4)-1), 
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3,
                  P.vals$V4))


m3 <- rbind(data.table(summary(emmeans(m.mmnb, ~ Type.of.day*Alcohol))),
      data.table(summary(emmeans(m.mfnb, ~ Type.of.day*Alcohol))),
      data.table(summary(emmeans(m.fmnb, ~ Type.of.day*Alcohol))),
      data.table(summary(emmeans(m.ffnb, ~ Type.of.day*Alcohol))))
m3[, Type := rep(c("MM", "MF", "FM", "FF"), each = 12)]


ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
  geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_wrap(~Type, scales = "free", ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "DA ~ Type.of.day*Alcohol, separate regression for each gender group")
ggsave("help_emmeans_gender.pdf")

#pairs(contrast_winlose, ~Type.of.day*Alcohol)
##############################################COMPARISON WITH OTHER INCIDENT TYPES
rm(list = ls())
#setwd("C:/Users/Anna/Desktop/crimes")
setwd("C:/Anna/Crimes/wm_supernew")
load("Westmids_supernew.RData")

check <- crimes_cleaned[, list(Offence_class = Offence_class[1], Offence_subclass = Offence_subclass[1],
                               offence = offence[1], Domestic_abuse = Domestic_abuse[1],
                               DA = DA[1], inc_da = inc_da[1],Alcohol_inv = Alcohol_inv[1],
                               datetime_first_committed = datetime_first_committed[1]),.(crime_number)]
check[, when.committed := ymd(substring(as.character(datetime_first_committed),1,10))]
check[inc_da == "Yes" | Domestic_abuse != "No", AmongstDA := T]
check[is.na(AmongstDA), AmongstDA := F]

other_violence <- check[AmongstDA == F & Offence_class == "Violence Against The Person",list(Otherv =.N),.(when.committed, Alcohol_inv)]
property <- check[Offence_class %in% c("Theft", "Burglary", "Robbery"),list(Property =.N),.(when.committed, Alcohol_inv)]
poo <- check[Offence_class == "Public Order Offences",list(Poo =.N),.(when.committed, Alcohol_inv)]
hate <- check[grepl("HATE|RACIALLY", offence),list(Hate =.N),.(when.committed, Alcohol_inv)]



all_days <- data.table(when.committed = rep(seq(ymd("2010-01-01"), ymd("2018-11-06"), by = "days"), each = 2),
                       Alcohol_inv = rep(c("No","Yes"), length(seq(ymd("2010-01-01"), ymd("2018-11-06"), by = "days"))))

all_days <- merge(all_days, other_violence, by = c("when.committed","Alcohol_inv"), all.x = TRUE)
all_days <- merge(all_days, poo, by = c("when.committed","Alcohol_inv"), all.x = TRUE)
all_days <- merge(all_days, hate, by = c("when.committed","Alcohol_inv"), all.x = TRUE)
all_days <- merge(all_days, property, by = c("when.committed","Alcohol_inv"), all.x = TRUE)
all_days[is.na(all_days)] <- 0

setnames(all_days, c("Alcohol_inv"), "Alcohol")

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



all_days <- merge(all_days, worldcupeuro, by.x = "when.committed",
                  by.y = "Date", all.x = TRUE)
all_days[is.na(England_windraw), England_windraw := FALSE]
all_days[is.na(England_win), England_win := FALSE]
all_days[is.na(England_draw), England_draw := FALSE]
all_days[is.na(England_lost), England_lost := FALSE]
all_days[is.na(England_played), England_played := FALSE]
all_days[when.committed %in% worldcupeuro[England_played==TRUE,Date +1], After_England := TRUE]
all_days[is.na(After_England), After_England := FALSE]


all_days[England_win == TRUE,Type.of.day := "England win"]
all_days[England_draw == TRUE,Type.of.day := "England draw"]
all_days[England_lost == TRUE,Type.of.day := "England lost"]
all_days[After_England == TRUE,Type.of.day := "After England"]
all_days[is.na(Type.of.day),Type.of.day := "Nonmatch day"]
all_days[(when.committed >= ymd("2010-06-11") & when.committed <= ymd("2010-07-11")) | 
           (when.committed >= ymd("2014-06-12") & when.committed <= ymd("2014-07-13")) |
           (when.committed >= ymd("2012-06-08") & when.committed <= ymd("2012-07-01")) | 
           (when.committed >= ymd("2016-06-10") & when.committed <= ymd("2016-07-10")), Tournament_on := TRUE]
all_days[is.na(Tournament_on), Tournament_on := FALSE]
all_days[, Type.of.day := ifelse(Type.of.day == "Nonmatch day" & Tournament_on == TRUE,
                                 "Tournament on",Type.of.day)]
all_days[,Type.of.day := as.factor(Type.of.day)]

all_days[,Type.of.day := factor(Type.of.day, levels = c("Nonmatch day","Tournament on", "England win","England draw",
                                                        "England lost","After England"))]
all_days[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                             "Mon","Tue", "Wed"))]

all_days[, year := as.factor(year(when.committed))]
all_days[, month := factor(as.character(month(when.committed, label = TRUE)))]

all_days <- all_days[!(when.committed == ymd("2018-11-06") | (when.committed >= ymd("2017-06-01") & when.committed <= ymd("2017-12-31"))),]

all_days[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
all_days[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed)==1,T,F)]

Hate.nb <- glm.nb(Hate ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days)
PR.nb <- glm.nb(Property ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days)
POO.nb <- glm.nb(Poo ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days)
VATP.nb <- glm.nb(Otherv ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days)

Hatep <- glm(Hate ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days, family = "poisson")
PRp <- glm(Property ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days, family = "poisson")
POOp <- glm(Poo ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days, family = "poisson")
VATPp <- glm(Otherv ~ year + Type.of.day*Alcohol + Day_of_week + month + XMAS + NYE, data = all_days, family = "poisson")

lrtest(Hatep, Hate.nb)
lrtest(PRp, PR.nb)
lrtest(POOp, POO.nb)
lrtest(VATPp, VATP.nb)

dispersiontest(Hatep)
dispersiontest(PRp)
dispersiontest(POOp)
dispersiontest(VATPp)

m3 <- rbind(data.table(summary(emmeans(Hate.nb, ~ Type.of.day*Alcohol))),
            data.table(summary(emmeans(PR.nb, ~ Type.of.day*Alcohol))),
            data.table(summary(emmeans(POO.nb, ~ Type.of.day*Alcohol))),
            data.table(summary(emmeans(VATP.nb, ~ Type.of.day*Alcohol))))
m3[, Type := rep(c("Hate", "Property", "Public Order Offences", "Other violence"), each = 12)]


ggplot(data = m3, aes(Type.of.day, emmean, colour = Alcohol)) +
  geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL)) +
  facet_wrap(~Type, scales = "free", ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of cases ~ Type.of.day*Alcohol, separate regression for each crime type")
ggsave("help_emmeans_crimetype.pdf")



contrast_winlose <- emmeans(VATP.nb, ~Type.of.day*Alcohol)
m1 <- emmeans(contrast_winlose, pairwise ~ Type.of.day|Alcohol)
m1 <- summary(m1)

CI.vectors <- data.table(exp(confint(PR.nb)),exp(confint(POO.nb)),exp(confint(Hate.nb)),
                         exp(confint(VATP.nb)))
Coef.vectors <- data.table(exp(PR.nb$coefficients), exp(POO.nb$coefficients),
                           exp(Hate.nb$coefficients), exp(VATP.nb$coefficients))
P.vals <- data.table(summary(PR.nb)$coefficients[,4],summary(POO.nb)$coefficients[,4],
                     summary(Hate.nb)$coefficients[,4],summary(VATP.nb)$coefficients[,4])



library(stargazer)
stargazer(PR.nb,POO.nb,Hate.nb,VATP.nb,type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,covariate.labels = gsub("Type.of.day|Yes","",names(PR.nb$coefficients)[grepl("Type|Alcohol",names(PR.nb$coefficients) )]),
          column.labels = c("Property-related", "Public Order Offences", "Hate incidents", "Other violence"),
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1),
                      as.numeric(Coef.vectors$V3-1),
                      as.numeric(Coef.vectors$V4-1)), 
          # ci.custom = list(rbind(CI.vectors[,1:2],
          #                        CI.vectors[,3:4],
          #                        CI.vectors[,5:6],
          #                        CI.vectors[,7:8])),
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3,
                  P.vals$V4))


#alternative
Hate.y <- glm.nb(Hate ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
Hate.n <- glm.nb(Hate ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])
poo.y <- glm.nb(Poo ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
poo.n <- glm.nb(Poo ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])
prop.y <- glm.nb(Property ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
prop.n <- glm.nb(Property ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])
other.y <- glm.nb(Otherv ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "Yes",])
other.n <- glm.nb(Otherv ~ year + Type.of.day + Day_of_week + month + XMAS + NYE, data = all_days[Alcohol == "No",])

CI.vectors <- data.table(exp(confint(prop.y)),exp(confint(prop.n)),exp(confint(poo.y)),
                         exp(confint(poo.n)),exp(confint(Hate.y)),exp(confint(Hate.n)),
                         exp(confint(other.y)),exp(confint(other.n)))
Coef.vectors <- data.table(exp(prop.y$coefficients), exp(prop.n$coefficients),
                           exp(poo.y$coefficients), exp(poo.n$coefficients),
                           exp(Hate.y$coefficients), exp(Hate.n$coefficients),
                           exp(other.y$coefficients), exp(other.n$coefficients))
P.vals <- data.table(summary(prop.y)$coefficients[,4],summary(prop.n)$coefficients[,4],
                     summary(poo.y)$coefficients[,4],summary(poo.n)$coefficients[,4],
                     summary(Hate.y)$coefficients[,4],summary(Hate.n)$coefficients[,4],
                     summary(other.y)$coefficients[,4],summary(other.n)$coefficients[,4])

stargazer(prop.y,prop.n,poo.y,poo.n,Hate.y,Hate.n,other.y,other.n,
          type = "latex",
          omit = c("month", "year","Day_of_week", "XMAS","NYE", "Constant"), 
          title = "Exponentiated coefficients and 95% CIs from a series of negative binomial regresssions predicting daily counts of reported DA incidents (other controls not included here: month, year, xmas/nye)",
          no.space = TRUE,covariate.labels = gsub("Type.of.day|Yes","",names(Hate.y$coefficients)[grepl("Type|Alcohol",names(Hate.y$coefficients) )]),
          coef = list(as.numeric(Coef.vectors$V1-1),
                      as.numeric(Coef.vectors$V2-1),
                      as.numeric(Coef.vectors$V3-1),
                      as.numeric(Coef.vectors$V4-1),
                      as.numeric(Coef.vectors$V5-1),
                      as.numeric(Coef.vectors$V6-1),
                      as.numeric(Coef.vectors$V7-1),
                      as.numeric(Coef.vectors$V8-1)), 
          p =list(P.vals$V1,
                  P.vals$V2,
                  P.vals$V3,
                  P.vals$V4,
                  P.vals$V5,
                  P.vals$V6,
                  P.vals$V7,
                  P.vals$V8))



######################################THREE HOUR PLOT####################################

rm(list = ls())
setwd("C:/Anna/Crimes/wm_supernew")
load("Westmids_supernew.RData")
check <- crimes_cleaned[inc_da == "Yes" | Domestic_abuse != "No", list(no_vict = sum(role == "VICT"),
                                                                       no_off = sum(role != "VICT"),
                                                                       offence = offence[1],
                                                                       Offence_class = Offence_class[1],
                                                                       Offence_subclass = Offence_subclass[1]), .(crime_number)]


domestic_abuse <- crimes_cleaned[crime_number %in% check[no_vict == 1 & no_off ==1, crime_number],]
domestic_abuse[, Time_frame := cut(datetime_first_committed, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2018-11-05 00:00:00"),by='3 hours'))]
domestic_abuse <- domestic_abuse[order(crime_number, role)]
domestic_abuse[, Type := paste0(gender,collapse = ""), .(crime_number)]

domestic_abuse <- domestic_abuse[, list(Time_frame = Time_frame[1],
                                        Alcohol = Alcohol_inv[1], Type = Type[1]), .(crime_number)]
domestic_abuse <- domestic_abuse[!grepl("U", Type),]
domestic_abuse <- domestic_abuse[,list(.N),.(Time_frame,Type,Alcohol)]
three.hours <- dcast(domestic_abuse, Time_frame + Alcohol ~ Type, value.var = "N")
three.hours <- three.hours[!is.na(Time_frame),]
three.hours[, Time_frame := ymd_hms(Time_frame)]
load("C:/Anna/Crimes/wm_supernew/worldcupeuro.RData")
worldcupeuro <- worldcupeuro[Team1 == "England" | Team2 == "England",]
worldcupeuro[, Goalsteam1 := as.numeric(Goalsteam1)]
worldcupeuro[, Goalsteam2 := as.numeric(Goalsteam2)]

worldcupeuro[(Team1 == "England" & Goalsteam1 > Goalsteam2) |(Team2 == "England" & Goalsteam1 < Goalsteam2),
             England_win := TRUE]
worldcupeuro[("England" == Team1 | "England" == Team2)& (Goalsteam1 == Goalsteam2),
             England_draw := TRUE]
worldcupeuro[(Team1 == "England" & (Goalsteam1 < Goalsteam2)),
             England_lost := TRUE]
worldcupeuro[(Team2 == "England" & (Goalsteam2 < Goalsteam1)),
             England_lost := TRUE]

worldcupeuro[Team1 == "England" | Team2 == "England" , England_played := TRUE]

#calculate when the game ends
worldcupeuro[Penalties == "", End_of_game := Date + 90*60]
worldcupeuro[Penalties == "extra", End_of_game := Date + 135*60]
worldcupeuro[Penalties == "penalties", End_of_game := Date + 150*60]
worldcupeuro[, how.long := (End_of_game-Date)]


#get the three hour period when the game ends
worldcupeuro[, EndTime_frame := cut(End_of_game, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2018-11-05 00:00:00"),by='3 hours'))]
worldcupeuro[, EndTime_frame := ymd_hms(EndTime_frame)]
#and the three hour oeriod when it starts
worldcupeuro[, StartTime_frame := cut(Date, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2018-11-05 00:00:00"),by='3 hours'))]
worldcupeuro[, StartTime_frame := ymd_hms(StartTime_frame)]
#for comparability, let's choose the three hour time period where most of the match falls
#calculate how many hours of the match is in both 3 hours periods 
#if the start and end is in one 3 hour period, there's no decision to be made
#otherwise look at how much of the match falls in the first three hour period
worldcupeuro[StartTime_frame!=EndTime_frame,first_half := as.numeric(((EndTime_frame-Date))/as.numeric(how.long*60))]
#ifelse wont give back a date object
worldcupeuro[StartTime_frame==EndTime_frame | first_half >0.5, Time_frame := StartTime_frame]
worldcupeuro[is.na(Time_frame), Time_frame := EndTime_frame]


all.days <- data.table(Time_frame = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2018-11-05 00:00:00"),by='3 hours'))

all.days <- merge(all.days, worldcupeuro, by = "Time_frame", all.x = TRUE)
all.days[, year := year(Time_frame)]
all.days[, month := month(Time_frame, label = TRUE)]
all.days[, day := day(Time_frame)]

#create when it ended dummy var
#then follow the next: 0-3, 3-6, 6-9, 9-12
#with result vars: win, lost, lost

all.days[, During := England_played == TRUE ]
all.days[, Threeafter := shift(England_played, 1, type = "lag") ]
all.days[, Sixafter := shift(England_played, 2, type = "lag") ]
all.days[, Threebefore := shift(England_played, 1, type = "lead") ]
all.days[, Sixbefore := shift(England_played, 2, type = "lead") ]
all.days[shift(England_played, 3, type = "lag") | shift(England_played, 4, type = "lag"),
         Twelveafter := TRUE]
all.days[shift(England_played, 5, type = "lag") | shift(England_played, 6, type = "lag") |
           shift(England_played, 7, type = "lag") | shift(England_played, 8, type = "lag"),
         Twentyfourafter := TRUE]

all.days[England_win | shift(England_win, 1, type = "lag") | shift(England_win, 2, type = "lag") |
           shift(England_win, 3, type = "lag") | shift(England_win, 4, type = "lag") |
           shift(England_win, 5, type = "lag") | shift(England_win, 6, type = "lag") |
           shift(England_win, 7, type = "lag") | shift(England_win, 8, type = "lag")|
           shift(England_win, 1, type = "lead") | shift(England_win, 2, type = "lead"),
         England_win := TRUE]


all.days[England_lost | shift(England_lost, 1, type = "lag") | shift(England_lost, 2, type = "lag") |
           shift(England_lost, 3, type = "lag") | shift(England_lost, 4, type = "lag") |
           shift(England_lost, 5, type = "lag") | shift(England_lost, 6, type = "lag") |
           shift(England_lost, 7, type = "lag") | shift(England_lost, 8, type = "lag") |
           shift(England_lost, 1, type = "lead") | shift(England_lost, 2, type = "lead"),
         England_lost := TRUE]


all.days[England_draw | shift(England_draw, 1, type = "lag") | shift(England_draw, 2, type = "lag") |
           shift(England_draw, 3, type = "lag") | shift(England_draw, 4, type = "lag") |
           shift(England_draw, 5, type = "lag") | shift(England_draw, 6, type = "lag") |
           shift(England_draw, 7, type = "lag") | shift(England_draw, 8, type = "lag")|
           shift(England_draw, 1, type = "lead") | shift(England_draw, 2, type = "lead"),
         England_draw := TRUE]

all.days <- merge(data.table(Time_frame = rep(seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2018-11-05 00:00:00"),by='3 hours'), each = 2),
                             Alcohol = rep(c("Yes", "No"), length(seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2018-11-05 00:00:00"),by='3 hours')))),
                  all.days, by = "Time_frame", all.x = TRUE)

all.days <- merge(all.days, three.hours, by = c("Time_frame", "Alcohol"), all.x = TRUE)

all.days[is.na(FF), FF := 0]
all.days[is.na(MF), MF := 0]
all.days[is.na(MM), MM := 0]
all.days[is.na(FM), FM := 0]
all.days[, All := MM + FF + MF + FM]

all.days[, Six_before_win := ifelse(Sixbefore == T & !is.na(Sixbefore)& England_win == TRUE & !is.na(England_win),1,0)]
all.days[, Three_before_win := ifelse(Threebefore == T & !is.na(Threebefore)& England_win == TRUE & !is.na(England_win),1,0)]
all.days[, Six_before_lost := ifelse(Sixbefore == T & !is.na(Sixbefore)& England_lost == TRUE & !is.na(England_lost),1,0)]
all.days[, Three_before_lost := ifelse(Threebefore == T & !is.na(Threebefore)& England_lost == TRUE & !is.na(England_lost),1,0)]
all.days[, Six_before_draw := ifelse(Sixbefore == T & !is.na(Sixbefore)& England_draw == TRUE & !is.na(England_draw),1,0)]
all.days[, Three_before_draw := ifelse(Threebefore == T & !is.na(Threebefore)& England_draw == TRUE & !is.na(England_draw),1,0)]
all.days[, Six_before_win := factor(Six_before_win)]
all.days[, Three_before_win := factor(Three_before_win)]
all.days[, Three_before_lost := factor(Three_before_lost)]
all.days[, Six_before_lost := factor(Six_before_lost)]
all.days[, Six_before_draw := factor(Six_before_draw)]
all.days[, Three_before_draw := factor(Three_before_draw)]




all.days[, Three_win := ifelse(!is.na(England_win) & !is.na(Threeafter) & England_win == TRUE &
                                 Threeafter == TRUE, 1,0)]
all.days[, Three_win := factor(Three_win)]
all.days[, Six_win := ifelse(!is.na(England_win) & !is.na(Sixafter) & England_win == TRUE &
                               Sixafter == TRUE, 1,0)]
all.days[, Six_win := factor(Six_win)]
all.days[, Twelve_win := ifelse(!is.na(England_win) & !is.na(Twelveafter) & England_win == TRUE &
                                  Twelveafter == TRUE, 1,0)]
all.days[, Twelve_win := factor(Twelve_win)]
all.days[, Twentyfour_win := ifelse(!is.na(England_win) & !is.na(Twentyfourafter) & England_win == TRUE &
                                      Twentyfourafter == TRUE, 1,0)]
all.days[, Twentyfour_win := factor(Twentyfour_win)]


all.days[, Three_lost := ifelse(!is.na(England_lost) & !is.na(Threeafter) & England_lost == TRUE &
                                  Threeafter == TRUE, 1,0)]
all.days[, Three_lost := factor(Three_lost)]
all.days[, Six_lost := ifelse(!is.na(England_lost) & !is.na(Sixafter) & England_lost == TRUE &
                                Sixafter == TRUE, 1,0)]
all.days[, Six_lost := factor(Six_lost)]
all.days[, Twelve_lost := ifelse(!is.na(England_lost) & !is.na(Twelveafter) & England_lost == TRUE &
                                   Twelveafter == TRUE, 1,0)]
all.days[, Twelve_lost := factor(Twelve_lost)]
all.days[, Twentyfour_lost := ifelse(!is.na(England_lost) & !is.na(Twentyfourafter) & England_lost == TRUE &
                                       Twentyfourafter == TRUE, 1,0)]
all.days[, Twentyfour_lost := factor(Twentyfour_lost)]


all.days[, Three_draw := ifelse(!is.na(England_draw) & !is.na(Threeafter) & England_draw == TRUE &
                                  Threeafter == TRUE, 1,0)]
all.days[, Three_draw := factor(Three_draw)]
all.days[, Six_draw := ifelse(!is.na(England_draw) & !is.na(Sixafter) & England_draw == TRUE &
                                Sixafter == TRUE, 1,0)]
all.days[, Six_draw := factor(Six_draw)]
all.days[, Twelve_draw := ifelse(!is.na(England_draw) & !is.na(Twelveafter) & England_draw == TRUE &
                                   Twelveafter == TRUE, 1,0)]
all.days[, Twelve_draw := factor(Twelve_draw)]
all.days[, Twentyfour_draw := ifelse(!is.na(England_draw) & !is.na(Twentyfourafter) & England_draw == TRUE &
                                       Twentyfourafter == TRUE, 1,0)]
all.days[, Twentyfour_draw := factor(Twentyfour_draw)]



all.days[, During_win := ifelse(!is.na(During) & During == TRUE & !is.na(England_win) & England_win == TRUE, 1,0)]
all.days[, During_win := factor(During_win)]
all.days[, During_draw := ifelse(!is.na(During) & During == TRUE & !is.na(England_draw) & England_draw == TRUE, 1,0)]
all.days[, During_draw := factor(During_draw)]
all.days[, During_lost := ifelse(!is.na(During) & During == TRUE & !is.na(England_lost) & England_lost == TRUE, 1,0)]
all.days[, During_lost := factor(During_lost)]



all.days[,Day_of_week := factor(as.character(wday(Time_frame, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun","Mon","Tue", "Wed"))]
all.days[,year := factor(year)]
all.days[,month := month(Time_frame, label = TRUE)]
all.days[, month := as.character(month)]
all.days[, month := factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                             "Jun", "Jul", "Aug", "Sep", "Oct",
                                             "Nov", "Dec"))]

#get rid of 2018 11 06 and second half of 2017
all.days <- all.days[!(year == 2017 & month %in% c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),]

all.days[, XMAS := ifelse(month == "Dec" & day %in% c(24,25,26),T,F)]
all.days[, NYE := ifelse(month == "Jan" & day == 1,T,F)]

all.days[, Period := factor(hour(Time_frame))]

#negative binomial fits better for both types of incidents
summary(three_nonalc <- glm.nb(All~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                                 Six_before_draw + Three_before_draw+
                                 During_draw+ During_win +
                                 During_lost+Three_draw+ Three_win + Three_lost +
                                 Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                                 Twentyfour_draw + Twentyfour_win +
                                 Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "No",]))

summary(three_alc <- glm.nb(All~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                                 Six_before_draw + Three_before_draw+
                                 During_draw+ During_win +
                                 During_lost+Three_draw+ Three_win + Three_lost +
                                 Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                                 Twentyfour_draw + Twentyfour_win +
                                 Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "Yes",]))


summary(three_nonalcp <- glm(All~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                                 Six_before_draw + Three_before_draw+
                                 During_draw+ During_win +
                                 During_lost+Three_draw+ Three_win + Three_lost +
                                 Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                                 Twentyfour_draw + Twentyfour_win +
                                 Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "No",], family = "poisson"))

summary(three_alcp <- glm(All~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                              Six_before_draw + Three_before_draw+
                              During_draw+ During_win +
                              During_lost+Three_draw+ Three_win + Three_lost +
                              Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                              Twentyfour_draw + Twentyfour_win +
                              Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "Yes",], family = "poisson"))

lrtest(three_alcp, three_alc)
lrtest(three_nonalcp, three_nonalc)

dispersiontest(three_nonalcp)
dispersiontest(three_alcp)

#ok, save regression results 
alc_ci <- confint(three_alc)
nonalc_ci <- confint(three_nonalc)
toplot <- data.table("Variable" = rep(names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))], 2),
                     "Coeff" = c(exp(three_alc$coefficients)[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))]],
                                 exp(three_nonalc$coefficients)[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))]]))
toplot[,  Alcohol := rep(c("Alcohol", "No Alcohol"), each =21)]
toplot <- cbind(toplot, rbind(exp(alc_ci[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))],]),
                              exp(nonalc_ci[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))],])))
setnames(toplot, c("2.5 %", "97.5 %"), c("Lower", "Upper"))
toplot[, Variable := gsub("1", "", Variable)]
# toplot[,Variable := factor(Variable, levels = rev(c("Six_before_win", "Three_before", "During_draw", "Three_draw","Six_draw", "Twelve_draw",
#                                                     "Twentyfour_draw", "During_lost", "Three_lost","Six_lost", "Twelve_lost", "Twentyfour_lost",
#                                                     "During_win", "Three_win","Six_win", "Twelve_win", "Twentyfour_win")))]
# toplot[, variable := as.character(Variable)]

toplot[, Variable := gsub("_before", "before", Variable)]

toplot[, Type :=unlist(strsplit(Variable,split = "_"))[1], by = 1:nrow(toplot)]
toplot[, Result :=unlist(strsplit(Variable,split = "_"))[2], by = 1:nrow(toplot)]


toplot[, Type := factor(Type, levels = c("Sixbefore", "Threebefore", "During", "Three","Six", 
                                                 "Twelve","Twentyfour"))]
levels(toplot$Type) <- c("Six\nHours\nBefore", "Three\nHours\nBefore", "During\nMatch", 
                         "Three\nHours\nAfter", "Six\nHours\nAfter", "Twelve\nHours\nAfter",
                         "Twenty-four\nHours\nAfter")

toplot[, Result := ifelse(Result == "win", "Win", ifelse(Result == "lost", "Lost", "Draw"))]

toplot[, Alcohol := ifelse(Alcohol == "No Alcohol", "No", "Yes")]

ggplot(toplot, aes(Type,Coeff, group = factor(Alcohol), fill = factor(Alcohol))) +
  geom_hline(aes(yintercept = 1), linetype = 4) +
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.7), size = 1.5, width =0.5) + 
  facet_wrap(~Result, ncol = 1) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha= 0.5) + 
  geom_line(size = 1, aes(colour = Alcohol)) + 
  theme_classic()+
  labs(x = "", y = "Coefficient", fill = "Alcohol") +theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=19))+ 
  scale_fill_manual(values=c("cornflowerblue", "darkorange")) +
  scale_colour_manual(values=c("cornflowerblue", "darkorange")) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))+
  #geom_point(position=position_dodge(width=0.7), size = 1.5, colour = "black")+
  ylim(c(-0.5,4))


# 
# ggplot(toplot, aes(Variable,Coeff, group = factor(Alcohol), colour = factor(Alcohol))) +
#   geom_hline(aes(yintercept = 1), linetype = 4) +
#   geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.7), size = 1.5) +
#   geom_point(position=position_dodge(width=0.7), size = 1.5, colour = "black") + 
#   coord_flip() + theme_classic()+
#   theme(legend.title=element_blank(),
#         legend.text = element_text(size=19))+ 
#   theme(strip.background = element_blank(),
#         strip.text.x = element_blank()) +
#   theme(text = element_text(size=25),
#         axis.text.x = element_text(size = 20),
#         axis.text.y = element_text(size = 20)) + 
#   theme(legend.position = "bottom")+
#   labs(x = "", y= "Coefficient")+ 
#   scale_color_manual(values=c("darkorange", "cornflowerblue"))

ggsave("C:\\Anna\\Crimes\\Football-DA\\Threehours.pdf", height = 10, width = 10)

