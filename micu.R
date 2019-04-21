library(dplyr)
library(ggplot2)

micu_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTUozjbX46q0FU3NLKV3xnUrMEtef1qEheTV0F5w0CjOFE0r8XuIqsfBtCz_EzNaZ_zGCj8HOeXSaOw/pub?gid=272284428&single=true&output=csv"
micu <- read.csv(micu_url)
micu$Month.of.Admission <- as.character(micu$Month.of.Admission)
micu$Class <- as.character(micu$Class)
micu$Antibiotic <- as.character(micu$Antibiotic)

micu$Month.of.Admission[micu$Month.of.Admission == "July 2018"] <- "Jul"
micu$Month.of.Admission[micu$Month.of.Admission == "August 2018"] <- "Aug"
micu$Month.of.Admission[micu$Month.of.Admission == "September 2018"] <- "Sep"
micu$Month.of.Admission[micu$Month.of.Admission == "October 2018"] <- "Oct"
micu$Month.of.Admission[micu$Month.of.Admission == "November 2018"] <- "Nov"
micu$Month.of.Admission[micu$Month.of.Admission == "December 2018"] <- "Dec"

micu$Month.of.Admission <- as.factor(micu$Month.of.Admission)

micu2_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTUozjbX46q0FU3NLKV3xnUrMEtef1qEheTV0F5w0CjOFE0r8XuIqsfBtCz_EzNaZ_zGCj8HOeXSaOw/pub?gid=1671631297&single=true&output=csv"
micu2 <- read.csv(micu2_url)
micu2$Month.of.Admission <- as.character(micu2$Month.of.Admission)
micu2$Class <- as.character(micu2$Class)
micu2$Antibiotic <- as.character(micu2$Antibiotic)

micu2$Month.of.Admission[micu2$Month.of.Admission == "July 2018"] <- "Jul"
micu2$Month.of.Admission[micu2$Month.of.Admission == "August 2018"] <- "Aug"
micu2$Month.of.Admission[micu2$Month.of.Admission == "September 2018"] <- "Sep"
micu2$Month.of.Admission[micu2$Month.of.Admission == "October 2018"] <- "Oct"
micu2$Month.of.Admission[micu2$Month.of.Admission == "November 2018"] <- "Nov"
micu2$Month.of.Admission[micu2$Month.of.Admission == "December 2018"] <- "Dec"

micu2$Month.of.Admission[micu2$Month.of.Admission == "10/19/2018"] <- "Oct"

levels(micu2$Month.of.Admission) <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

##BUBBLE CHART##
bb <- micu %>%
  select(Month.of.Admission, 
         Daily.DDD, Cumulative.DDD, Predicted.mort)

plot <- ggplot(bb, aes(x = Month.of.Admission, y = Daily.DDD, color = Predicted.mort, size = Cumulative.DDD)) +
  geom_point(position = "jitter", alpha = 0.6) +
  scale_y_continuous("Daily DDD", limits = c(0,5), 
                     breaks = seq(0,5,1)) +
  scale_x_discrete("Month of admission", 
                   limits = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

##TYPE OF ANTIBIOTIC##
type <- micu2 %>%
  select(Month.of.Admission, Cumulative.DDD, Daily.Cumul, Class, Primary.Diagnosis, Predicted.mort)

type$Cumulative.DDD[is.na(type$Cumulative.DDD)] <- 0
type$Daily.Cumul[is.na(type$Daily.Cumul)] <- 0

plot2 <- ggplot(type, aes(x = Month.of.Admission, y = Cumulative.DDD, color = Class)) +
  geom_point(position = "jitter", alpha = 0.6, size = 2) +
  scale_y_continuous("Cumulative DDD", limits = c(0,10), 
                     breaks = seq(0,10,2)) +
  scale_x_discrete("Month of admission", 
                   limits = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

##TYPE OVER MONTHS##
str(type)
plot3 <- ggplot(type, aes(x = Class, y = Cumulative.DDD)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous("Cumulative DDD", limits = c(0,10), breaks = seq(0,10,2)) +
  facet_grid(~Month.of.Admission)

##BUBBLE CHART TOTAL##
type <- micu2 %>%
  select(Month.of.Admission, Cumulative.DDD, Daily.Cumul, Class, Antibiotic, Primary.Diagnosis, Predicted.mort, Total.Cumulative)

type$Cumulative.DDD[is.na(type$Cumulative.DDD)] <- 0
type$Daily.Cumul[is.na(type$Daily.Cumul)] <- 0

tab <- type %>%
  group_by(Month.of.Admission, Class) %>%
  summarise(sum(Cumulative.DDD), mean(Cumulative.DDD))

tab <- tab[(tab$`sum(Cumulative.DDD)`!=0), ]

tiff("test.tiff", units="in", width=5, height=5, res=300)
ggplot(tab, aes(x = Month.of.Admission, y = `mean(Cumulative.DDD)`, 
                         color = Class, size = `sum(Cumulative.DDD)`)) + 
  geom_point(position = "jitter", alpha = 0.6) +
  scale_y_continuous("Exposure per admission (in DDDs)", limits = c(0.5,4), 
                     breaks = seq(0.5,4,1.5)) +
  scale_x_discrete("Month of admission", 
                   limits = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(values = c("Betabeta" = "#33B8FF", "Ceph12" = "#3336FF", "Ceph34" = "#0000FF", "Penicillins" = "#000080",
                                "AG" = "#FF0000", "Vanco" = "#8B0000", "Carbepenems" = "#000000", "Macro" = "#228B22", 
                                "Metro" = "#711D43", "Misc" = "#88601B", "Quinolones" = "#A9A9A9", "Tetracyclines" = "#FF1493")) +
  scale_size(range = c(1,20)) +
  theme_classic()
dev.off()

##BUBBLE CHART SYSTEMWISE##

totalcum_mean <- type %>%
  select(Primary.Diagnosis, Total.Cumulative) %>%
  na.omit() %>%
  group_by(Primary.Diagnosis) %>%
  summarise(mean(Total.Cumulative))

tab_dx <- type %>%
  group_by(Primary.Diagnosis, Class) %>%
  summarise(sum(Cumulative.DDD), mean(Cumulative.DDD))

tab_daily <- micu2 %>%
  group_by(Primary.Diagnosis) %>%
  summarise(mean(Total.Cumulative))

bb_sys_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSUTdc1XNJUOLSFsMA6HrlXAwfWaNhANwjFWhQ-nGC8c1_yJraTsQOWeXnjfUqz7QUKZw1QrcVST6yU/pub?gid=0&single=true&output=csv"
bb_sys <- read.csv(bb_sys_url)
bb_sys <- bb_sys[bb_sys$sum.Cumulative.DDD.!=0, ]
bb_sys <- bb_sys[ ,c(-4)]
str(bb_sys)
colnames(bb_sys)[3] <- "Cumulative DDD"
colnames(bb_sys)[4] <- "mean exposure"

levels(bb_sys$Primary.Diagnosis)

ggplot(bb_sys, aes(x = Primary.Diagnosis, y = `mean exposure`, 
                color = Class, size = `Cumulative DDD`)) + 
  geom_jitter(height = 0.6, alpha = 0.6) +
  scale_y_continuous("Mean total antibiotic exposure per admission (in DDDs)", limits = c(0,10), 
                     breaks = seq(0,10,2)) +
  scale_color_manual(values = c("Betabeta" = "#33B8FF", "Ceph12" = "#3336FF", "Ceph34" = "#0000FF", "Penicillins" = "#000080",
                                "AG" = "#FF0000", "Vanco" = "#8B0000", "Carbepenems" = "#000000", "Macro" = "#228B22", 
                                "Metro" = "#711D43", "Misc" = "#88601B", "Quinolones" = "#A9A9A9", "Tetracyclines" = "#FF1493")) +
  scale_size(range = c(1,20)) +
  theme_classic()

##CORRELATION##

micu %>% 
  select(Class, Total.Cumulative, Serum.Cr, Days.on.Foley, 
         S.F.ratio, TropICS.score) -> micu_cor
micu_cor$Serum.Cr <- as.character(micu_cor$Serum.Cr)
cr_cor <- micu_cor %>%
  select(Total.Cumulative, Serum.Cr)
cr_cor <- cr_cor[-c(101,118,28), ]
cr_cor$Serum.Cr <- as.numeric(cr_cor$Serum.Cr)
cr_cor <- na.omit(cr_cor)
str(cr_cor)

ggplot(cr_cor, aes(x = Serum.Cr, y = Total.Cumulative)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(cr_cor$Total.Cumulative, cr_cor$Serum.Cr, method = "pearson")
cor.test(cr_cor$Total.Cumulative, cr_cor$Serum.Cr, method = "spearman")

micu_cor %>%
  select(Days.on.Foley, Total.Cumulative) -> fol_cor
str(fol_cor)
fol_cor <- na.omit(fol_cor)
fol_cor2 <- fol_cor[fol_cor$Days.on.Foley != 0, ]

ggplot(fol_cor2, aes(x = Days.on.Foley, y = Total.Cumulative)) +
  geom_point() +
  geom_smooth(method = "lm")

cor.test(fol_cor2$Days.on.Foley, fol_cor2$Total.Cumulative, method = "pearson")
cor.test(fol_cor2$Days.on.Foley, fol_cor2$Total.Cumulative, method = "spearman")

micu_cor %>%
  select(S.F.ratio, Total.Cumulative) -> sf_cor
str(sf_cor)
sf_cor <- na.omit(sf_cor)
sf_cor <- sf_cor[sf_cor$S.F.ratio>10, ]

ggplot(sf_cor, aes(x = S.F.ratio, y = Total.Cumulative)) +
  geom_jitter() +
  geom_smooth(method = "lm")

cor.test(sf_cor$S.F.ratio, sf_cor$Total.Cumulative, method = "pearson")
cor.test(sf_cor$S.F.ratio, sf_cor$Total.Cumulative, method = "spearman")

micu %>%
  select(TLC, Total.Cumulative) -> tlc_cor
str(tlc_cor)
tlc_cor$TLC <- as.character(tlc_cor$TLC)
tlc_cor$TLC <- as.numeric(tlc_cor$TLC)
tlc_cor <- na.omit(tlc_cor)
tlc_cor <- tlc_cor[tlc_cor$TLC>100, ]

ggplot(tlc_cor, aes(x = TLC, y = Total.Cumulative)) +
  geom_jitter() +
  geom_smooth(method = "lm")

cor.test(tlc_cor$TLC, tlc_cor$Total.Cumulative, method = "pearson")
cor.test(tlc_cor$TLC, tlc_cor$Total.Cumulative, method = "spearman")

micu %>%
  select(TropICS.score, Total.Cumulative) -> trp_cor
str(trp_cor)
trp_cor <- na.omit(trp_cor)

ggplot(trp_cor, aes(x = TropICS.score, y = Total.Cumulative)) +
  geom_jitter() +
  geom_smooth(method = "lm")

cor.test(trp_cor$TropICS.score, trp_cor$Total.Cumulative, method = "pearson")
cor.test(trp_cor$TropICS.score, trp_cor$Total.Cumulative, method = "spearman")

##NUMBER OF ANTIBIOTICS##

num <- micu %>%
  select(Number.of.ABs, Primary.Diagnosis, Month.of.Admission)

num <- na.omit(num)
num_tab <- num %>%
  group_by(Month.of.Admission) %>%
  summarise(mean(Number.of.ABs), sd(Number.of.ABs))
fill <- "#4271AE"
line <- "#1F3552"
ggplot(num, aes(x = Month.of.Admission, y = Number.of.ABs)) +
  scale_x_discrete("Month of admission", 
                   limits = c("Jul", "Aug", "Sep", 
                              "Oct", "Nov", "Dec")) +
  geom_boxplot(fill = fill, color = line, alpha = 0.7, outlier.color = "red")

##DDD PER 100 PATIENT DAYS##

d100 <- micu2 %>%
  select(Length.of.stay, Antibiotic, Cumulative.DDD) %>%
  na.omit() %>%
  group_by(Antibiotic) %>%
  summarise(sum(Length.of.stay), sum(Cumulative.DDD))

library(googlesheets)
gs_auth(new_user = T)
gs_new("AB", ws_title = "ablist", input = d100)
ad_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vS1EEia_r_7hwMdGXUt8PbTEnFY2gVRuCh6CDEbZVUU1uGuGwUtSOFAzlr06pTWWreVFJKi6gGBbbow/pub?gid=0&single=true&output=csv"
ad <- read.csv(ad_url)
ad$AD <- as.character(ad$AD)
ad$AD <- as.numeric(ad$AD)
ad$AD[is.na(ad$AD)] <- 1019.30

ggplot(ad, aes(x = Antibiotic, y = AD, fill = Study)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete("Antibiotic", 
                   limits = c("Total", "Piperacillin betalactamase", "Meropenem",
                              "Linezolid", "Vancomycin"))







