library(readxl)
library(dplyr)

Out <- read_xlsx("New_outcomevars.xlsx", sheet = 1)
glimpse(Out)

#AGE ~ ROSC
Age <- Out %>%
    select(Age, Outcome)
Age$Age <- as.numeric(Age$Age)
Age$Outcome <- as.factor(Age$Outcome)
head(Age)
Age <- na.omit(Age)
mean(Age$Age)
sd(Age$Age)

#Boxplot
ggplot(Age, aes(x = Outcome, y = Age)) +
  geom_boxplot()

#Table creation
Age_tab <- Age %>%
  group_by(Outcome) %>%
  summarise(mean(Age), sd(Age))

#Creating p value
Age_t <- t.test(Age$Age ~ Age$Outcome, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)
Age_t$p.value

#GENDER ~ ROSC

Gen <- Out %>%
  select(Outcome, Gender)

Gen$Outcome <- as.factor(Gen$Outcome)
Gen$Gender <- as.factor(Gen$Gender)

head(Gen)

#Creating table
G_tab <- table(Gen$Outcome, Gen$Gender)

#Creating barplot
barplot(G_tab, cluster=T, legend=T)

#Chisq test
chisq.test(G_tab, correct = T)

#PROVIDER~OUTCOME
Provider <- Out %>%
  select(`Provider of first cpr`, Outcome)
Provider$Outcome <- as.factor(Provider$Outcome)
Provider$`Provider of first cpr` <- as.factor(Provider$`Provider of first cpr`)
head(Provider)
Provider <- na.omit(Provider)
Provider$`Provider of first cpr`[Provider$`Provider of first cpr`=="clinical assistant"] <- "Nurse"

#Creating table
P_tab <- table(Provider$Outcome, Provider$`Provider of first cpr`)
P_tab1 <- P_tab[,-1]

#Creating barplot
barplot(P_tab1, cluster=T, legend=T)

#Chisq test
P_chi <- chisq.test(P_tab1, correct = F)

P_chi
summary(P_chi)

#DAYS TO CODE ~ OUTCOME

Days <- Out %>%
  select(`Number of days to arrest after icu admission`, Outcome)
Days <- na.omit(Days)
names(Days)[1] <- paste("ndays")
Days$Outcome <- as.factor(Days$Outcome)

Tab_days <- Days %>%
  group_by(Outcome) %>%
  summarise(mean(ndays), sd(ndays))

#Creating barplot
barplot(D_tab, cluster=T, legend=T)

#t test
t_ndays <- t.test(Days$ndays ~ Days$Outcome, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)

#INITIAL RHYTHM ~ OUTCOME
Int_rhyth <- Out %>%
  select(`Initial rhythm`, Outcome)
Int_rhyth <- na.omit(Int_rhyth)
names(Int_rhyth)[1] <- paste("rhythm")

Int_rhyth$rhythm[Int_rhyth$rhythm=="bradycardia"] <- "non-shockable"
Int_rhyth$rhythm[Int_rhyth$rhythm=="normal perfusing rhythm"] <- "non-shockable"
Int_rhyth$rhythm[Int_rhyth$rhythm=="VF"] <- "shockable"
Int_rhyth$rhythm[Int_rhyth$rhythm == "PEA"] <- "non-shockable"
Int_rhyth$rhythm[Int_rhyth$rhythm == "asystole"] <- "non-shockable"
Int_rhyth$rhythm[Int_rhyth$rhythm == "pulseless electrical activity"] <- "non-shockable"

Int_rhyth$Outcome <- as.factor(Int_rhyth$Outcome)
Int_rhyth$rhythm <- as.factor(Int_rhyth$rhythm)
head(Int_rhyth)

#Creating table
I_tab <- table(Int_rhyth$Outcome, Int_rhyth$rhythm)
I_tab

#Creating barplot
barplot(I_tab, cluster=T, legend=T)

#Chisq test
I_chi <- chisq.test(I_tab, correct = F)
I_chi

#INTUBATION STATUS ~ OUTCOME
P_Intub <- Out %>%
  select(`Pre-event airway`, Outcome) %>%
  na.omit() 
names(P_Intub)[1] <- paste("airway")
P_Intub$airway[P_Intub$airway == "Endotracheal Tube mechanically ventilated"] <- "ET tube"
P_Intub$airway[P_Intub$airway != "ET tube"] <- "no ET tube"

P_Intub$Outcome <- as.factor(P_Intub$Outcome)
P_Intub$airway <- as.factor(P_Intub$airway)
head(P_Intub)

#Creating table
P_tab <- table(P_Intub$Outcome, P_Intub$airway)
P_tab

#Creating barplot
barplot(P_tab, cluster=T, legend=T)

#Chisq test
P_chi <- chisq.test(P_tab, correct = F)
P_chi

#PRESSOR ~ OUTCOME
Pr_Intub <- Out %>%
  select(`ALS interventions in place at time of event(before cpr was initiated)`, Outcome) %>%
  na.omit() 
names(Pr_Intub)[1] <- paste("pressors")

#Creating table
Pr_tab <- table(Pr_Intub$Outcome, Pr_Intub$pressors)
Pr_tab

#Creating barplot
barplot(Pr_tab, cluster=T, legend=T)

#Chisq test
Pr_chi <- chisq.test(Pr_tab, correct = F)
Pr_chi

#DURATION OF CPR ~ OUTCOME
Duration <- Out %>%
  select(`Time CPR given for`, Outcome) %>%
  na.omit() 

names(Duration)[1] <- "Time"

Duration$Time[Duration$Time == "31 to 40 mins"] <- "More than 30 mins"
Duration$Time[Duration$Time == "51 to 60 mins"] <- "More than 30 mins"
Duration$Time[Duration$Time == "11- 19 mins"] <- "11 to 19 mins"

Duration$Time <- as.factor(Duration$Time)
Duration$Outcome <- as.factor(Duration$Outcome)

levels(Duration$Time)

#Creating table
T_tab <- table(Duration$Time, Duration$Outcome)
T_tab

#Sorting rownames
NT_tab <- factor(rownames(T_tab), levels=c("LESS than 10 mins", "11 to 19 mins", 
                                           "20 to 30 mins", "More than 30 mins")) 
sortedT_tab <- T_tab[order(NT_tab),]
sortedT_tab

#Creating barplot
barplot(sortedT_tab, beside = T, legend.text = T)

#Chisq testing
Dur_chi <- chisq.test(sortedT_tab, correct = F)
Dur_chi

#GCS ~ OUTCOME
GCS_Data <- Out %>%
  select(`Preevent Glassgow coma scale`, `Glasgow Coma Score after cpr`, Outcome) %>%
  na.omit() 

Pre_event <- GCS_Data %>%
  select(`Preevent Glassgow coma scale`, Outcome)

Post_event <- GCS_Data %>%
  select(`Glasgow Coma Score after cpr`, Outcome)

names(Pre_event)[1] <- "GCS_pre"
names(Post_event)[1] <- "GCS_post"

Pre_event$GCS_pre[Pre_event$GCS_pre >= 8] <- "More than 8"
Pre_event$GCS_pre[Pre_event$GCS_pre < 8] <- "Less than 8"

Post_event$GCS_post[Post_event$GCS_post >= 8] <- "More than 8"
Post_event$GCS_post[Post_event$GCS_post < 8] <- "Less than 8"

Pre_event$GCS_pre <- as.factor(Pre_event$GCS_pre)
Post_event$GCS_post <- as.factor(Post_event$GCS_post)
Pre_event$Outcome <- as.factor(Pre_event$Outcome)
Post_event$Outcome <- as.factor(Post_event$Outcome)

head(Pre_event)
head(Post_event)

#Creating table
Pre_tab <- table(Pre_event$GCS_pre, Pre_event$Outcome)
Post_tab <- table(Post_event$GCS_post, Post_event$Outcome)
Pre_tab
Post_tab

#Chisq test
Pre_chi <- chisq.test(Pre_tab, correct = F)
Post_chi <- chisq.test(Post_tab, correct = F)
Pre_chi
Post_chi