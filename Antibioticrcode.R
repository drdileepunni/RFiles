library(dplyr)
library(ggplot2)
library(googlesheets)

gs_auth(new_user = T)
ab_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQmcXcW0gDDhPXo9JsDDRmmcVWHlXgXP6YP8ds18RvwVu3GK8eJctK-qFnsdBSVBoGxB08TUyfIUF-G/pub?gid=1346253917&single=true&output=csv"
abdata <- read.csv(ab_url)

abdata <- abdata %>% 
  select("CPMRN", "Month.of.Admission", "Hospital", "Restricted.antibiotics", "Class", 
         "Name.1", "Cumulative.DDD")

##ANTIBIOTIC ICU DAYS##
countas <- abdata %>%
  group_by(Name) %>%
  summarise(sum(Countas))

abdata2 <- merge(abdata, countas, by='Name', all = T)
gs_new(title = "Antibiotics", ws_title = "Rexport", input = abdata2)

##INDIV ANTIBIOTIC DAYS##
indiv_countas <- abdata %>%
  group_by(Name, Name.1) %>%
  summarise(sum(Countas))

abdata3 <- abdata %>%
  right_join(indiv_countas, by = c("Name", "Name.1"))

gs_new(title = "Antibiotics_R", ws_title = "Rexport", input = abdata3)

##FINDING TOTAL CUM###
t_cumul <- abdata %>%
  group_by(CPMRN, Month.of.Admission) %>%
  summarise(cumul_ddd = sum(Cumulative.DDD), no_ab = length(unique(Name.1)),
            res_ab = length(unique(Restricted.antibiotics)))

##MONTHWISE##

monthwise <- t_cumul %>%
  group_by(Month.of.Admission) %>%
  summarise(total_ddds = sum(cumul_ddd), adms = length(cumul_ddd),
            max_no_ab = max(no_ab), no_res_ab = max(res_ab))
ddds_per_adm <- monthwise$total_ddds/monthwise$adms
monthwise <- cbind(monthwise, ddds_per_adm)
monthwise <- monthwise[c(3,1,6,5,4,2),]

##CLEANING##

abdata$Restricted.antibiotics[abdata$Restricted.antibiotics == "PIPERACILLIN+TAZOBACTAM"] <- "Piperacillin + tazobactam"
abdata$Restricted.antibiotics[abdata$Restricted.antibiotics == "LINEZOLID"] <- "Linezolid"
abdata$Restricted.antibiotics[abdata$Restricted.antibiotics == "TEICOPLANIN"] <- "Teicoplanin"
abdata$Restricted.antibiotics[abdata$Restricted.antibiotics == "MEROPENEM"] <- "Meropenem"
abdata$Restricted.antibiotics[abdata$Restricted.antibiotics == "AMIKACIN"] <- "Amikacin"
abdata$Restricted.antibiotics <- factor(abdata$Restricted.antibiotics)

##PLOTTING MONTHWISE USE##

restr_ab <- abdata %>% 
  group_by(Month.of.Admission, Restricted.antibiotics) %>%
  summarise(times = length(Restricted.antibiotics))
str(restr_ab)
restr_ab$Month.of.Admission <- factor(restr_ab$Month.of.Admission, levels = c("July 2018", "August 2018", 
                                        "September 2018", "October 2018", 
                                        "November 2018", "December 2018"))
restr_ab <- with(restr_ab, restr_ab[order(Month.of.Admission, Restricted.antibiotics, times), ])
restr_ab$Restricted.antibiotics[restr_ab$Restricted.antibiotics == ""] <- NA
restr_ab <- na.omit(restr_ab)

gs_new(title = "Antibiotics_R(2)", ws_title = "restr", input = restr_ab)

##CREATE A SEPERATE DF##

sep <- t_cumul %>%
  right_join(countas, by = c("Name"))

colnames(sep) <- c("Name", "Total cumul DDD", "Antibiotic days")

## FINDING NUMBER OF ANTIBIOTICS ##

##FINAL EXPORT AS GOOGLE SHEETS##
AB <- gs_title("Antibiotics_R")
AB_df <- gs_read(AB)
