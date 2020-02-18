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
setwd("C:/Anna/Crimes/wm_supersupernew")

#incident reports some of which all da but they cant respond to all of them
#we only used incidents that had a full crime rceord, involving da incidents that are not crimes
#based on severity perceived in the call centre
#no crime no - non severe stuff
#persistent caller


#read in the crime files
########################SKIP THIS AND READ IN ALL_DATA.RDATA#############################
crimes_data <- read.csv("ID_ID_WARKS_STUDY_CRIMES_2010.csv", fill = TRUE, header = TRUE, sep = "," , stringsAsFactors=FALSE)

invisible(sapply(2011:2019, function (x) {
  crime <- read.csv(paste0("ID_ID_WARKS_STUDY_CRIMES_",x, ".csv"), fill = TRUE, header = TRUE, sep = "," , stringsAsFactors=FALSE)
  names(crime) <- names(crimes_data)
  crimes_data <<- rbind(crimes_data, crime)
}))

crimes_data <- data.table(crimes_data)
#tolower
setnames(crimes_data, colnames(crimes_data), tolower(colnames(crimes_data)))

#turn Crime_ID into a factor
crimes_data[, crime_number := as.factor(crime_number)]


incidents_data <- read.csv("WARKS_STUDY_INCS_2010.csv", fill = TRUE, header = TRUE, sep = "," , stringsAsFactors=FALSE)
incidents_data <- unique(incidents_data)
invisible(sapply(2011:2019, function (x) {
  incident <- read.csv(paste0("WARKS_STUDY_INCS_",x, ".csv"), fill = TRUE, header = TRUE, sep = "," , stringsAsFactors=FALSE)
  incident <- unique(incident)
  names(incident) <- names(incidents_data)
  incidents_data <<- rbind(incidents_data, incident)
  print(x)
}))
incidents_data <- data.table(incidents_data)
incidents_data[, CRIME_NUMBER := as.factor(CRIME_NUMBER)]


#create DOmestic abuse and alcohol markers in the incidents and crimes data
#for each crime, we'll be using information from matching incidents entries about its nature

#we only want those incidents that have crime records from 13m to 2.5 million
#for an analysis with all incidents data, see all_incident_robustness.R
incidents_data <- incidents_data[CRIME_NUMBER %in% unique(crimes_data$crime_number),]
incidents_data[, Alc := !(is.na(KEYWORDS1) & is.na(KEYWORDS2) & is.na(KEYWORDS3) & is.na(KEYWORDS4)) | grepl("ALCOHOL", QUALIFIER)]
incidents_data[, DA := grepl("DOMESTIC", QUALIFIER) | grepl("DOMESTIC", INCIDENT_CLASS) | grepl("DOMESTIC", FINAL_TYPE)]

incidents_data[, Alcohol_inc := ifelse(any(Alc), "Yes", "No"), .(CRIME_NUMBER)]
incidents_data[, DA_inc := ifelse(any(DA), "Yes", "No"), .(CRIME_NUMBER)]
#first sort out the alcohol indicator duplication issue
crimes_data[,Alcohol := ifelse(any(ai %in% "Yes"), "Yes", "No"), .(crime_number)]
crimes_data <- unique(crimes_data[,-c("ai")])
incidents_data <- incidents_data[,list(DA_inc = DA_inc[1],Alcohol_incidents = Alcohol_inc[1]), .(CRIME_NUMBER)]
#now add the incidents data and create final alcohol and domestic abuse markers
crimes_data <- merge(crimes_data, incidents_data, by.x = "crime_number",
                     by.y = "CRIME_NUMBER", all.x = T)
#final alcohol marker - including info from incidents data
crimes_data[,Alcohol := ifelse(any(Alcohol %in% "Yes") | any(Alcohol_incidents %in% "Yes"), "Yes", "No"), .(crime_number)]
#final da marker - including info from incidents data
crimes_data[,DomesticAbuse := ifelse(grepl("DOMESTIC", offence) | grepl("COERCIVE", offence) | grepl("Yes", DA_inc), "Yes", "No"), .(crime_number)]


#################### CRIME CODE CLASSIFICATION ############################

#so we have two crime codes (code_1 and code_2 likely corresponding to the class and subclass of offences)
#but we need to create a factor variable which contains the description of the crime

#https://data.gov.uk/dataset/recorded-crime-counting-rules
offence_list <- data.table(read.csv("offences_list.csv"))
#replace NA code_2 (as in the data)
offence_list[is.na(code_2), code_2 := 0]
#get first row of each code combination category (the class and subclass are always the same for these)
offence_list <- offence_list[, list(Offence_class = Class[1], Offence_subclass = Sub.class[1]), by = list(code_1, code_2)]

#crimes_data <- merge(crimes_data, offence_list, by = c("code_1", "code_2"), all.x = TRUE)
#unmatched <- unique(crimes_data[is.na(Offence_class), c("code_1", "code_2", "short_descript")])
#about a 120 unmatched code_1 and code_2 combinations, but most of these are non-crimes
#there are quite a few offences that are missing from the list, so make correction list
#where the we have the old code_1 and code_2 along iwth the new ones
correction <- data.table(read.csv("correction.csv",header = TRUE))

#I can't come up with a quicker way to do this properly
correction[, collapsed_corr := paste(code_1_o, code_2_o, collapse = " "), by = 1:nrow(correction)]
crimes_data[, coll_crime_code := paste(homc_code, hooc_code, collapse = " "), by = 1:nrow(crimes_data)]
#add them as new columns and update these
crimes_data <- cbind(crimes_data,correction[match(crimes_data[,coll_crime_code], correction[,collapsed_corr]),c("code_1_n", "code_2_n")])

#add new columns instead of changing the old ones
crimes_data[is.na(homc_code), homc_code := code_1]
crimes_data[is.na(hooc_code), hooc_code := code_2]
offence_list[, Offence_class := as.character(Offence_class)]
offence_list[, Offence_subclass := as.character(Offence_subclass)]
crimes_data[is.na(code_1_n), code_1_n := homc_code]
crimes_data[is.na(code_2_n), code_2_n := hooc_code]

crimes_data <- merge(crimes_data, offence_list, by.x = c("code_1_n", "code_2_n"), by.y = c("code_1", "code_2"), all.x = TRUE)
#the offence subclass is sometimes missing but the main class is always there (unless it is a non-crime)
#correct the weird ones before converting it into a factor
crimes_data[Offence_class == "Theft  ", Offence_class := "Theft"]
crimes_data[Offence_subclass == " ", Offence_subclass := NA]
crimes_data[Offence_subclass == "", Offence_subclass := NA]

crimes_data[,Offence_subclass := as.factor(Offence_subclass)]
crimes_data[,Offence_class := as.factor(Offence_class)]
#make sure it is only the non crimes that remain unclassified
unique(crimes_data[is.na(Offence_class), c("homc_code", "hooc_code")])


#################### PERSON SPECIFIC VARIABLES (ID, ROLE, AGE, GENDER, ETHN) ########################

#create ID, role, age, ethnicity and gender variables
crimes_data[, PID := ifelse(!is.na(nominalrefurn1), nominalrefurn1, nominalrefurn2), by = 1:nrow(crimes_data)]
crimes_data[, role := ifelse(!is.na(roletype1), "VICT", roletype2), by = 1:nrow(crimes_data)]
#Can be either: Defendant (charged or cautioned or fixed penalty notice, etc,); Suspect (suspected but not charged etc); Probably did it but not officially a suspect; Responsible - committed the offence but can't be charged because below the age of criminal responsibility, for example.
crimes_data[, age := ifelse(role == "VICT", ageurn1, ageurn2), by = 1:nrow(crimes_data)]
crimes_data[, ethnic := ifelse(role == "VICT",selfeadescurn1, selfeadescurn1), by = 1:nrow(crimes_data)]
crimes_data[, gender := ifelse(role == "VICT", gender1, gender2), by = 1:nrow(crimes_data)]
#in ethnic, change "NOT STATED" to NA
crimes_data[ethnic == "NOT STATED", ethnic := NA]
#age issue (I can't really come up with a better way of doing this)
crimes_data[age < 0 | age > 100 | is.nan(age), age := NA]

#convert them back to factor variables
crimes_data[, PID := as.factor(PID)]
crimes_data[, role := as.factor(role)]
crimes_data[, ethnic := as.factor(ethnic)]
crimes_data[, gender := as.factor(gender)]


###################### INJURY CLASSIFICATION ###################
#injurytext1 and injurytext2 are never both populated, so we can make one vraiable out of it
crimes_data[, Injury.text := ifelse(!is.na(injurytext1), injurytext1, injurytext2)]
crimes_data[, Injury.code := ifelse(!is.na(injurycode1), injurycode1, injurycode2)]
crimes_data[, Injury_class := as.factor(c("dead", "serious", "minor", "threat", "no injury")[Injury.code])]


#########################Regression 1: model specification#####################################################################
#include all domestic abuse cases, just need to know when it happened and whether it was alcohol-related or not.
options(scipen = 999)
da_data <- crimes_data[DomesticAbuse == "Yes",list(Alcohol = Alcohol[1], date_first_commited = date_first_commited[1]),.(crime_number) ]
da_data <- da_data[,.N,.(Alcohol, date_first_commited)]
da_data[, when.committed := ifelse(nchar(date_first_commited) == 13,substring(paste(0, date_first_commited, sep = ""),1,8),
                                   substring(date_first_commited,1,8))]
da_data[, when.committed := mdy(when.committed)]
all.days<- data.table(expand.grid(when.committed = seq(ymd("2010-01-01"), ymd("2019-10-10"), by = "days"),
                                  Alcohol = c("No","Yes")))

all.days <- merge(all.days, da_data[, -c("date_first_commited")], 
                  by = c("when.committed","Alcohol"), all.x = TRUE)
all.days[is.na(all.days)] <- 0
#add day-specific controls
all.days[,Day_of_week := factor(as.character(wday(when.committed, label = TRUE)), levels = c("Thu", "Fri", "Sat", "Sun",
                                                                                             "Mon","Tue", "Wed"))]

all.days[, year := as.factor(year(when.committed))]
all.days[, month := factor(as.character(month(when.committed, label = TRUE)))]

all.days[, XMAS := ifelse(month(when.committed) == 12 & mday(when.committed) %in% c(24,25,26),T,F)]
all.days[, NYE := ifelse(month(when.committed) == 1 & mday(when.committed)==1,T,F)]



#descriptive
library(readxl)

popest <- function(year) {
  data <- data.table(read_excel("popestimates.xlsx", sheet = paste0("Mid-", year," Persons")), year = year)
  sum(data[grepl("Sandwell|Birmingham|Coventry|Walsall|Wolverhampton|Dudley|Solihull", unlist(data[,3])),4])
}

population <- data.table(year = as.factor(2010:2018))
population[, pop := as.numeric(popest(year)), by = 1:nrow(population)]

population[, Rate := ((DA/no.day)/pop)*100000]

population[Alcohol == "Yes", round(min(Rate),2)]

#create nice plot of alcohol and non-alcohol by weekday
#need to bootstrap and stratify by year
library(boot)
stat <- function(x, i) {
  data <- x[i,]
  return(mean(data$N))
}


get.stats <- function(Alcohols, Day) {
  dataset <- all.days[Alcohol == Alcohols & Day_of_week == Day,]
  bootres <- boot(dataset, stat, R=1000, strata = dataset$month)
  conf <- boot.ci(bootres, type = "perc")    
  return(list(bootres$t0, conf$percent[4], conf$percent[5]))
}

toplot <- data.table(expand.grid(Alcohol = unique(all.days$Alcohol), Day.of.week = unique(all.days$Day_of_week)))

toplot[, Mean := get.stats(Alcohols = Alcohol, Day = Day.of.week)[[1]], by = 1:nrow(toplot)]
toplot[, Lower := get.stats(Alcohols = Alcohol, Day = Day.of.week)[[2]], by = 1:nrow(toplot)]
toplot[, Upper := get.stats(Alcohols = Alcohol, Day = Day.of.week)[[3]], by = 1:nrow(toplot)]

toplot[, Day.of.week := factor(Day.of.week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))]

ggplot(toplot, aes(Day.of.week, Mean, colour = Alcohol)) + geom_point(size = 2) + theme_bw() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size =1 ) + 
  theme(legend.position = "bottom", text = element_text(size = 18))+
  scale_colour_manual(values=c("cornflowerblue", "darkorange"))+
  labs(colour="Alcohol-related", x = "Day of the week", y = "Daily average number of cases")

ggsave("descriptives.pdf")


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
worldcupeuro[, Outcome :=  ifelse(!is.na(England_win), "England win", ifelse(!is.na(England_draw), "England draw", "England lost"))]
worldcupeuro[, Team := ifelse(Team1 == "England", Team2, Team1)]


descript <- worldcupeuro[,c(3,6,8,15,16)][order(Date)]
setcolorder(descript, c(1,5,2,3,4))
descript[, Date := as.character(Date)]
descript[, year := as.integer(year)]
print(xtable(descript, type = "latex"), include.rownames = F)

worldcupeuro <- worldcupeuro[,c(3,6,9:13)]


all.days <- merge(all.days, worldcupeuro, by.x = "when.committed",
                  by.y = "Date", all.x = TRUE)
all.days[is.na(England_windraw), England_windraw := FALSE]
all.days[is.na(England_win), England_win := FALSE]
all.days[is.na(England_draw), England_draw := FALSE]
all.days[is.na(England_lost), England_lost := FALSE]
all.days[is.na(England_played), England_played := FALSE]
all.days[when.committed %in% worldcupeuro[England_played==TRUE,Date +1], After_England := TRUE]
all.days[is.na(After_England), After_England := FALSE]


all.days[England_win == TRUE,Type.of.day := "England win"]
all.days[England_draw == TRUE,Type.of.day := "England draw"]
all.days[England_lost == TRUE,Type.of.day := "England lost"]
all.days[After_England == TRUE,Type.of.day := "After England"]
all.days[is.na(Type.of.day),Type.of.day := "Nonmatch day"]

all.days[(when.committed >= ymd("2010-06-11") & when.committed <= ymd("2010-07-11")) | 
           (when.committed >= ymd("2014-06-12") & when.committed <= ymd("2014-07-13")) |
           (when.committed >= ymd("2012-06-08") & when.committed <= ymd("2012-07-01")) | 
           (when.committed >= ymd("2016-06-10") & when.committed <= ymd("2016-07-10")) |
           (when.committed >= ymd("2018-06-14") & when.committed <= ymd("2018-07-15")),
         Tournament_on := TRUE]
all.days[is.na(Tournament_on), Tournament_on := FALSE]
all.days[, Type.of.day := ifelse(Type.of.day == "Nonmatch day" & Tournament_on == TRUE,
                                 "Tournament on",Type.of.day)]
all.days[,Type.of.day := as.factor(Type.of.day)]

all.days[,Type.of.day := factor(Type.of.day, levels = c("Nonmatch day","Tournament on", "England win","England draw",
                                                        "England lost","After England"))]

all.days_original <- all.days


save.image("C:/Anna/Crimes/wm_supersupernew/all_data.RData")

