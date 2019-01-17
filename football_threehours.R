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

inc_per_row <- inc_per_row[!is.na(time_first_committed.y),]
inc_per_row <- inc_per_row[year(time_first_committed.x) <2017,]
inc_per_row[, Time_frame := cut(time_first_committed.x, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2017-01-01 00:00:00"),by='3 hours'))]

# crimes <- inc_per_row[, list(N = .N, Alcohol = sum(Alcohol.x == "Yes"), 
#                              Physical_injury = sum(Injury.y %in% c("serious", "minor")),
#                              DA_incidents = sum(Class == "DA incidents")),
#                       .(Time_frame)]

inc_per_row[grepl("- DWELLING", location_type_general.x), Outside := "No"]
inc_per_row[is.na(Outside), Outside := "Yes"]


crimes <- inc_per_row[, list(N = .N, Alcohol = sum(Alcohol.x == "Yes" | Under_influence.x == "Yes"), 
                             Physical_injury = sum(Injury.y %in% c("serious", "minor")),
                             Outside = sum(Outside == "Yes")),
                      .(Time_frame)]


crimes[, Time_frame := ymd_hms(Time_frame)]

all.days <- data.table(Time_frame = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2017-01-01 00:00:00"),by='3 hours'))

all.days <- merge(all.days, crimes, by = "Time_frame", all.x = TRUE)

load("worldcupeuro.RData")
worldcupeuro[, Goalsteam1 := as.numeric(Goalsteam1)]
worldcupeuro[, Goalsteam2 := as.numeric(Goalsteam2)]

worldcupeuro[(Team1 == "England" & Goalsteam1 > Goalsteam2) |(Team2 == "England" & Goalsteam1 < Goalsteam2),
             England_win := TRUE]
worldcupeuro[("England" == Team1 | "England" == Team2)& (Goalsteam1 == Goalsteam2),
             England_draw := TRUE]
# worldcupeuro[(Team1 == "England" & Goalsteam1 >= Goalsteam2) | (Team2 == "England" & Goalsteam2 <= Goalsteam2),
#              England_winlost := TRUE]
worldcupeuro[England_draw == TRUE | England_win == TRUE, England_windraw := TRUE]
worldcupeuro[(Team1 == "England" & (Goalsteam1 < Goalsteam2)),
             England_lost := TRUE]
worldcupeuro[(Team2 == "England" & (Goalsteam2 < Goalsteam1)),
             England_lost := TRUE]

worldcupeuro[Team1 == "England" | Team2 == "England" , England_played := TRUE]

worldcupeuro[, date := format(as.Date(Date, format="%d/%m/%Y")), by = 1:nrow(worldcupeuro)]


# worldcupeuro <- worldcupeuro[,list(Type = Type[1], England_winlost = sort(England_winlost, decreasing = TRUE)[1],
#                                    England_lost = sort(England_lost, decreasing = TRUE)[1],England_lost = sort(England_lost, decreasing = TRUE)[1],
#                                    England_win = sort(England_win, decreasing = TRUE)[1],
#                                    England_played = sort(England_played, decreasing = TRUE)[1],
#                                    Datetime = Date),by = date]
# worldcupeuro <- worldcupeuro[order(Datetime)]

#create end of the game variable
#add 90 minutes if there are no penalties or extra time
#add 90 + 45 minutes if there is extra time
#add 90 + 45 + 15 minutes if there is a penalty shootout

worldcupeuro[Penalties == "", End_of_game := Date + 90*60]
worldcupeuro[Penalties == "extra", End_of_game := Date + 135*60]
worldcupeuro[Penalties == "penalties", End_of_game := Date + 150*60]
worldcupeuro[, how.long := (End_of_game-Date)]
worldcupeuro <- worldcupeuro[Team1 == "England" | Team2 == "England",]



worldcupeuro[, EndTime_frame := cut(End_of_game, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2016-12-31 00:00:00"),by='3 hours'))]
worldcupeuro[, EndTime_frame := ymd_hms(EndTime_frame)]
worldcupeuro[, StartTime_frame := cut(Date, breaks = seq(ymd_hms("2010-01-01 00:00:00"),ymd_hms("2016-12-31 00:00:00"),by='3 hours'))]
worldcupeuro[, StartTime_frame := ymd_hms(StartTime_frame)]
worldcupeuro[, Latter := as.numeric((End_of_game-EndTime_frame)/60)/(as.numeric(how.long)*60)]
worldcupeuro[, Former := as.numeric(180-(Date-StartTime_frame)/60)/(as.numeric(how.long)*60)]

worldcupeuro[StartTime_frame == EndTime_frame, Time_frame := EndTime_frame]
worldcupeuro[Latter > Former, Time_frame := EndTime_frame]
worldcupeuro[Latter < Former, Time_frame := StartTime_frame]

all.days <- merge(all.days, worldcupeuro, by = "Time_frame", all.x = TRUE)

all.days[, year := year(Time_frame)]
all.days[, month := month(Time_frame, label = TRUE)]
all.days[, day := day(Time_frame)]


###########################################################################
#create when it ended dummy var
#then follow the next: 0-3, 3-6, 6-9, 9-12
#with result vars: win, lost, lost

all.days[, During := England_played == TRUE ]
all.days[, Threeafter := shift(England_played, 1, type = "lag") ]
all.days[, Sixafter := shift(England_played, 2, type = "lag") ]
all.days[shift(England_played, 3, type = "lag") | shift(England_played, 4, type = "lag"),
         Twelveafter := TRUE]
all.days[shift(England_played, 5, type = "lag") | shift(England_played, 6, type = "lag") |
           shift(England_played, 7, type = "lag") | shift(England_played, 8, type = "lag"),
         Twentyfourafter := TRUE]


all.days[England_win | shift(England_win, 1, type = "lag") | shift(England_win, 2, type = "lag") |
           shift(England_win, 3, type = "lag") | shift(England_win, 4, type = "lag") |
         shift(England_win, 5, type = "lag") | shift(England_win, 6, type = "lag") |
           shift(England_win, 7, type = "lag") | shift(England_win, 8, type = "lag"),
         England_win := TRUE]


all.days[England_lost | shift(England_lost, 1, type = "lag") | shift(England_lost, 2, type = "lag") |
           shift(England_lost, 3, type = "lag") | shift(England_lost, 4, type = "lag") |
           shift(England_lost, 5, type = "lag") | shift(England_lost, 6, type = "lag") |
           shift(England_lost, 7, type = "lag") | shift(England_lost, 8, type = "lag"),
         England_lost := TRUE]



all.days[England_draw | shift(England_draw, 1, type = "lag") | shift(England_draw, 2, type = "lag") |
           shift(England_draw, 3, type = "lag") | shift(England_draw, 4, type = "lag") |
           shift(England_draw, 5, type = "lag") | shift(England_draw, 6, type = "lag") |
           shift(England_draw, 7, type = "lag") | shift(England_draw, 8, type = "lag"),
         England_draw := TRUE]


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

Three_hours <- all.days[, c(1, 22:26, 36:55)]
rm(list=setdiff(ls(), "Three_hours"))



all.days[,Day_of_week := factor(as.character(wday(Time_frame, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun","Mon","Tue", "Wed"))]
all.days[,year := factor(year)]
all.days <- all.days[year != 2017,]
all.days[,year := factor(year)]
all.days[,month := month(Time_frame, label = TRUE)]
all.days[, month := as.character(month)]
all.days[, month := factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                             "Jun", "Jul", "Aug", "Sep", "Oct",
                                             "Nov", "Dec"))]

library(MASS)
library(lmtest)

# setnames(all.days, c("During_draw", "During_win", "During_lost","Three_draw", "Three_win",      
#         "Three_lost", "Six_draw", "Six_win", "Six_lost",  "Twelve_draw",     
#         "Twelve_win", "Twelve_lost",  "Twentyfour_draw", "Twentyfour_win",  "Twentyfour_lost"), 
#         c("Draw_during","Win_during", "Lost_during",
#           "Draw_0-3h","Win_0-3h", "Lost_0-3h", "Draw_3-6h","Win_3-6h", "Lost_3-6h",
#           "Draw_6-12h", "Win_6-12h", "Lost_6-12h", "Draw_12-24h","Win_12-24h","Lost_12-24h"))
# 


all.days[is.na(N), N := 0]
all.days[is.na(Physical_injury), Physical_injury := 0]
all.days[is.na(Outside), Outside := 0]
#all.days[is.na(DA_incidents), DA_incidents := 0]
all.days[is.na(Alcohol), Alcohol := 0]
all.days[, Period := factor(hour(Time_frame))]




data <- data.table()
regressions_all <- list()
formula <- "~ During_draw+ During_win +During_lost+Three_draw+ Three_win + Three_lost +
               Six_draw + Six_win +Six_lost +Twelve_draw + Twelve_win +Twelve_lost +
               Twentyfour_draw + Twentyfour_win +
               Twentyfour_lost + year + Day_of_week + month + Period" 
#vars <- c("N", "Alcohol", "DA_incidents", "Physical_injury")
vars <- c("N", "Alcohol", "Physical_injury")

for (i in 1:length(vars)) {
  ng <- glm.nb(formula = paste(vars[i], formula),all.days)
  poi <- glm(formula =paste(vars[i], formula),all.days,
             family = "poisson")
  test1 <- lrtest(ng, poi)
    if (test1$`Pr(>Chisq)`[2] > 0.05) {
      #there's no evidence for overdispersion, go with the poisson model
      regressions_all[[i]] <- poi
      type <- "Poisson"
    } else {
      regressions_all[[i]] <- ng
      type <- "NB"
    }
    est1 <- cbind(Estimate = coef(regressions_all[[i]]), confint(regressions_all[[i]]))
    est1 <- exp(est1)
    est1 <- cbind(est1, vars[i], type)
    data <- rbind(data, est1)
  # names(regressions_all[[i]]$coefficients) <- c("Constant", "Draw_during", "Lost_during","Win_during",  "Draw_0-3h", "Lost_0-3h","Win_0-3h",
  #                                               "Draw_3-6h", "Lost_3-6h","Win_3-6h","Draw_6-12h", "Lost_6-12h","Win_6-12h",
  #                                               "Draw_12-24h","Lost_12-24h","Win_12-24h", "Fri", "Sat", "Sun", "Mon", "Tue",
  #                                               "Wed",2011:2016, "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
}

data <- data.table(data)
data1 <- data.table(data)
data[, variable_name := rep(names(est1[,1]), 4)]
setnames(data, c("2.5 %","97.5 %","V4"), c("Lower","Upper","Dep_var"))
data[, variable_name := gsub("month|year|Day_of_week", "", variable_name)]
data <- data[variable_name != "(Intercept)",]
data <- data[!grepl("Period", variable_name),]
data[variable_name == "During_draw1", variable_name := "Draw_during"]
data[variable_name == "During_lost1", variable_name := "Lost_during"]
data[variable_name == "During_win1", variable_name := "Win_during"]
data[variable_name == "Three_draw1", variable_name := "Draw_0-3h"]
data[variable_name == "Three_lost1", variable_name := "Lost_0-3h"]
data[variable_name == "Three_win1", variable_name := "Win_0-3h"]
data[variable_name == "Six_draw1", variable_name := "Draw_3-6h"]
data[variable_name == "Six_lost1", variable_name := "Lost_3-6h"]
data[variable_name == "Six_win1", variable_name := "Win_3-6h"]
data[variable_name == "Twelve_draw1", variable_name := "Draw_6-12h"]
data[variable_name == "Twelve_lost1", variable_name := "Lost_6-12h"]
data[variable_name == "Twelve_win1", variable_name := "Win_6-12h"]
data[variable_name == "Twentyfour_draw1", variable_name := "Draw_12-24h"]
data[variable_name == "Twentyfour_lost1", variable_name := "Lost_12-24h"]
data[variable_name == "Twentyfour_win1", variable_name := "Win_12-24h"]

data <- data[,variable_name := factor(variable_name, levels = rev(c("Draw_during", "Draw_0-3h", "Draw_3-6h","Draw_6-12h","Draw_12-24h",
                                                                "Lost_during", "Lost_0-3h", "Lost_3-6h","Lost_6-12h","Lost_12-24h",
                                                                "Win_during",  "Win_0-3h", "Win_3-6h","Win_6-12h","Win_12-24h",
                                                                "Fri", "Sat", "Sun", "Mon", "Tue","Wed",2011:2016, "Feb", "Mar",
                                                                "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))]
data[, Estimate := round(as.numeric(Estimate),2)]
data[, Upper := round(as.numeric(Upper),2)]
data[, Lower := round(as.numeric(Lower),2)]
data[, Controls := ifelse(variable_name %in% levels(data$variable_name)[24:38], FALSE, TRUE)]

data[Dep_var == "N", Dep_var := "All_reported"]
#data[, Dep_var := factor(Dep_var, levels = c("All_reported", "Alcohol", "Physical_injury", "DA_incidents"))]
data[,Dep_var := gsub("_"," ",Dep_var)]
data[, Dep_var := factor(Dep_var, levels = c("All reported", "Alcohol", "Physical injury"))]




library(ochRe)
library(RColorBrewer)
source("multiplot.R")

p1 <- ggplot(data[Controls == FALSE,], aes(variable_name,Estimate, group = factor(Dep_var), colour = factor(Dep_var))) +
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.6))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position ="none")+ geom_hline(yintercept = 1, linetype = "dashed") +labs(x = "", y = "")+
  scale_color_manual(values=brewer.pal(8,"Dark2")) + coord_flip()+
  theme(legend.title=element_blank(),
        text = element_text(size=20))+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+ ylim(c(0,10.5))
p1_leg <- ggplot(data[Controls == FALSE,], aes(variable_name,Estimate, group = factor(Dep_var), colour = factor(Dep_var))) +
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.6))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank()) +
  theme(legend.position ="bottom")+ geom_hline(yintercept = 1, linetype = "dashed") +labs(x = "", y = "")+
  scale_color_manual(values=brewer.pal(8,"Dark2")) + coord_flip()+
  theme(legend.title=element_blank())+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(face = "bold", size = 16))

p2 <- ggplot(data[Controls == TRUE,], aes(variable_name,Estimate, group = factor(Dep_var), colour = factor(Dep_var))) +
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.6))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position ="none")+ geom_hline(yintercept = 1, linetype = "dashed") +labs(x = "", y = "")+
  scale_color_manual(values=brewer.pal(8,"Dark2")) + coord_flip()+
  theme(legend.title=element_blank())+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))+ ylim(c(0,10.5))


g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg<-g_legend(p1_leg)
library(gridExtra)
grid.arrange(arrangeGrob(p1, p2,nrow = 1), leg,nrow=2, heights = c(0.9, 0.1))

ggplot(data[Controls == FALSE,], aes(variable_name,Estimate, group = factor(Dep_var), colour = factor(Dep_var))) +
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position=position_dodge(width=0.6))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position ="bottom")+ geom_hline(yintercept = 1, linetype = "dashed") +labs(x = "", y = "")+
  scale_color_manual(values=brewer.pal(8,"Dark2")) + coord_flip()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=18))+ 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  theme(text = element_text(size=16),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+ ylim(c(0,10.5))



ggsave("C:\\Anna\\Crimes\\wm_new\\football_revisited\\Time_course.png")

# multiplot(p1,p2,cols = 2)
# 
# library(stargazer)
# 
# stargazer(regressions_all[[1]], regressions_all[[2]], regressions_all[[3]], regressions_all[[4]],
#           dep.var.caption = "",dep.var.labels = "",
#           column.labels  = c("All reported", "Alcohol-related", "DA incidents", "Physical injury"),
#           type = "latex", single.row = TRUE,no.space = TRUE, font.size = "footnotesize",
#           out="C:/Anna/Crimes/wm_new/football_revisited/threehours.tex")




