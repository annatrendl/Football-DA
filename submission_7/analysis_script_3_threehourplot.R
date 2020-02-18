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

load("C:/Anna/Crimes/wm_supersupernew/all_data.RData")
crimes_data[,.N,.(Alcohol,DomesticAbuse)]

############################### THREE HOUR PLOT ############################
options(scipen = 999)
da_data <- crimes_data[DomesticAbuse == "Yes",list(Alcohol = Alcohol[1], 
          date_first_commited = date_first_commited[1], time_first_commited = time_first_commited[1]),.(crime_number) ]
da_data <- da_data[,.N,.(Alcohol, date_first_commited, time_first_commited)]

da_data[, date_first_committed := ifelse(nchar(date_first_commited) == 13,substring(paste(0, date_first_commited, sep = ""),1,8),
                                             substring(date_first_commited,1,8))]
da_data[, date_first_committed := mdy(date_first_committed)]

create.time <- function(date, time) {
  paste0(c(as.character(date), as.character(time)), collapse = " ")
}
#1) new time variables: time first committed, time last committed, time reported, time recorded
da_data[, datetime_first_committed := mapply(create.time, date = date_first_committed, time = time_first_commited)]
da_data[, datetime_first_committed := ymd_hms(datetime_first_committed, tz = "Europe/London")]


da_data[, Time_frame := cut(datetime_first_committed, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2019-10-10 00:00:00"),by='3 hours'))]
three.hours <- da_data[!is.na(Time_frame),]
three.hours[, Time_frame := ymd_hms(Time_frame)]
three.hours <- three.hours[,list(N = sum(N)),.(Time_frame,Alcohol)]

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
worldcupeuro[, EndTime_frame := cut(End_of_game, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2019-10-10 00:00:00"),by='3 hours'))]
worldcupeuro[, EndTime_frame := ymd_hms(EndTime_frame)]
#and the three hour oeriod when it starts
worldcupeuro[, StartTime_frame := cut(Date, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2019-10-10 00:00:00"),by='3 hours'))]
worldcupeuro[, StartTime_frame := ymd_hms(StartTime_frame)]
#for comparability, let's choose the three hour time period where most of the match falls
#calculate how many hours of the match is in both 3 hours periods 
#if the start and end is in one 3 hour period, there's no decision to be made
#otherwise look at how much of the match falls in the first three hour period
worldcupeuro[StartTime_frame!=EndTime_frame,first_half := as.numeric(((EndTime_frame-Date))/as.numeric(how.long*60))]
#ifelse wont give back a date object
worldcupeuro[StartTime_frame==EndTime_frame | first_half >0.5, Time_frame := StartTime_frame]
worldcupeuro[is.na(Time_frame), Time_frame := EndTime_frame]


all.days <- data.table(Time_frame = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2019-10-10 00:00:00"),by='3 hours'))

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

all.days <- merge(data.table(Time_frame = rep(seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2019-10-10 00:00:00"),by='3 hours'), each = 2),
                             Alcohol = rep(c("Yes", "No"), length(seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2019-10-10 00:00:00"),by='3 hours')))),
                  all.days, by = "Time_frame", all.x = TRUE)

all.days <- merge(all.days, three.hours, by = c("Time_frame", "Alcohol"), all.x = TRUE)


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




all.days[, XMAS := ifelse(month == "Dec" & day %in% c(24,25,26),T,F)]
all.days[, NYE := ifelse(month == "Jan" & day == 1,T,F)]

all.days[, Period := factor(hour(Time_frame))]

#negative binomial fits better for both types of incidents
summary(three_nonalc <- glm.nb(N~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                                 Six_before_draw + Three_before_draw+
                                 During_draw+ During_win +
                                 During_lost+Three_draw+ Three_win + Three_lost +
                                 Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                                 Twentyfour_draw + Twentyfour_win +
                                 Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "No",]))

summary(three_alc <- glm.nb(N~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                              Six_before_draw + Three_before_draw+
                              During_draw+ During_win +
                              During_lost+Three_draw+ Three_win + Three_lost +
                              Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                              Twentyfour_draw + Twentyfour_win +
                              Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "Yes",]))



summary(three_nonalcp <- glm(N~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                               Six_before_draw + Three_before_draw+
                               During_draw+ During_win +
                               During_lost+Three_draw+ Three_win + Three_lost +
                               Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                               Twentyfour_draw + Twentyfour_win +
                               Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "No",], family = "poisson"))

summary(three_alcp <- glm(N~ Six_before_win + Three_before_win + Six_before_lost + Three_before_lost +
                            Six_before_draw + Three_before_draw+
                            During_draw+ During_win +
                            During_lost+Three_draw+ Three_win + Three_lost +
                            Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
                            Twentyfour_draw + Twentyfour_win +
                            Twentyfour_lost + year + Day_of_week + month + Period + XMAS + NYE, data = all.days[Alcohol == "Yes",], family = "poisson"))

lrtest(three_alcp, three_alc)
lrtest(three_nonalcp, three_nonalc)
#lrtest(threehourp,threehournb)

dispersiontest(three_nonalcp)
dispersiontest(three_alcp)


alc_ci <- confint(three_alc)
nonalc_ci <- confint(three_nonalc)
toplot <- data.table("Variable" = rep(names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))], 2),
                     "Coeff" = c(exp(three_alc$coefficients)[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))]],
                                 exp(three_nonalc$coefficients)[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))]]))
toplot[,  Alcohol := rep(c("Yes", "No"), each =21)]
toplot <- cbind(toplot, rbind(exp(alc_ci[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))],]),
                              exp(nonalc_ci[names(three_alc$coefficients)[grepl("draw|win|lost|before", names(three_alc$coefficients))],])))
setnames(toplot, c("2.5 %", "97.5 %"), c("Lower", "Upper"))
toplot[, Variable := gsub("1", "", Variable)]



toplot[, Variable := gsub("_before", "before", Variable)]

toplot[, Type :=unlist(strsplit(Variable,split = "_"))[1], by = 1:nrow(toplot)]
toplot[, Result :=unlist(strsplit(Variable,split = "_"))[2], by = 1:nrow(toplot)]


toplot[, Type := factor(Type, levels = c("Sixbefore", "Threebefore", "During", "Three","Six", 
                                         "Twelve","Twentyfour"))]
levels(toplot$Type) <- c("Six\nHours\nBefore", "Three\nHours\nBefore", "During\nMatch", 
                         "Three\nHours\nAfter", "Six\nHours\nAfter", "Twelve\nHours\nAfter",
                         "Twenty-four\nHours\nAfter")

toplot[, Result := ifelse(Result == "win", "Win", ifelse(Result == "lost", "Loss", "Draw"))]

toplot[, Alcohol := ifelse(Alcohol == "No", "Not Alcohol-related", "Alcohol-related")]
toplot[, Alcohol := factor(Alcohol, levels = c("Not Alcohol-related",  "Alcohol-related"))]

toplot[, Coeff := (Coeff-1)*100]
toplot[, Lower := (Lower-1)*100]
toplot[, Upper := (Upper-1)*100]

ggplot(toplot, aes(Type,Coeff, group = factor(Alcohol), fill = factor(Alcohol))) +
  geom_hline(aes(yintercept = 1), linetype = 4) +
  #geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.7), size = 1.5, width =0.5) + 
  facet_wrap(~Result, ncol = 1) + 
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha= 0.5) + 
  geom_line(size = 1, aes(colour = Alcohol)) + 
  theme_classic()+
  labs(x = "", y = "Increase in domestic abuse (%)", fill = "Alcohol") +theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=19))+ 
  scale_fill_manual(values=c("cornflowerblue", "darkorange")) +
  scale_colour_manual(values=c("cornflowerblue", "darkorange")) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))



ggsave("C:\\Anna\\Crimes\\Football-DA\\submission_7\\Threehours.pdf", height = 10, width = 10)