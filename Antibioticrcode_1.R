library(dplyr)
ab_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTrcu9tivxn3C2M4YxVmFzbAt0w90iTGS5KRkpt_olIXXpWUc4_0grLn6nBB3LVuTi0Ztd_GanEP-3-/pub?gid=2052269565&single=true&output=csv"
abdata <- read.csv(ab_url)

abdata <- abdata %>% 
  select(2,3,4,7,8:13)

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