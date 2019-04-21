install.packages("ROCR")
library(ROCR)
library(dplyr)

roc_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSVtsgRGMEBxeceAQQAuTtzOC5Yp54dXFCSLI1r3LgdzjSpvvhfT4TLb2OEbZHZwaNBJIZa327szIvg/pub?gid=0&single=true&output=csv"
values <- read.csv(roc_url)

score_df <- values %>%
  select(ICU.Survival.1, TropICS.score, Pred.Mort)

score_df$TropICS.score[score_df$TropICS.score == "Cannot be calculated"] <- NA
score_df$Pred.Mort[score_df$Pred.Mort == "Cannot be calculated"] <- NA

score_df <- na.omit(score_df)

score_df$TropICS.score <- as.character(score_df$TropICS.score)
score_df$Pred.Mort <- as.character(score_df$Pred.Mort)
score_df <- data.frame(survivial = score_df[,1], lapply(score_df[,c(2,3)], as.numeric))

levels(score_df$survivial) <- c(1, 0)

str(score_df)



