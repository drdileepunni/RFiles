library(dplyr)
library(ggplot2)
library(googlesheets)

bi_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRBVyY5zJ-7v5FeFQ41OkbwK_Vst0KiIlywcZHZS8UJcfqBDg6fpHLhIW0YS1I4imVnxXLAtFcqIV9f/pub?gid=1019560877&single=true&output=csv"
bi <- read.csv(bi_url, stringsAsFactors = F)
bi <- bi[-c(34,35,37,38)]

replacena <- function(col, val) {
  for(i in 1:length(col)) {
    if (is.na(col[i])) {
      col[i] = val
    }
  }
  return(col)
}

gethigh <- function(x) {
  arr = sort(x, decreasing = T)
  print(head(arr, 10))
}

getlow <- function(x) {
  arr = sort(x, decreasing = T)
  print(tail(arr, 10))
}

##replacewith <- function(col, torepl, repl) {
##  for (i in 1:length(col)) {
##    if (col[i]==torepl) {
##      col[i] = repl
##    }
##  }
##  return(col)
##}

glimpse(bi)

convertNum <- function(x) {
  x <- as.numeric(x)
}

bi <- cbind(bi[1:13], apply(bi[14:33],2, convertNum), bi[34])

bi$Survival[bi$Survival==""] <- "Alive"
bi$Survival[bi$Survival=="undefined"] <- "Alive"

bi$GCS <- as.factor(bi$GCS)
bi$GCS <- factor(bi$GCS, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15))
bi$GCS[is.na(bi$GCS)] <- 15
sum(is.na(bi$GCS))

bi$Temparature[bi$Temparature<80] <- 98.6
bi$Temparature[is.na(bi$Temparature)] <- 98.6

sum(is.na(bi$HR))
plot(bi$HR)
bi2 <- sort(bi$HR, decreasing = T)
tail(bi2, 10)
sum(is.na(bi$HR))
bi$HR[is.na(bi$HR)] <- 87

attach(bi)
sum(is.na(bi$SpO2))
sum(SpO2<1, na.rm = T)
plot(bi$SpO2)
bi2 <- sort(bi$SpO2, decreasing = T)
tail(bi2, 10)
sum(is.na(SpO2))
bi$SpO2[is.na(bi$SpO2)] <- 99

replacewith <- function(col) {
  for (i in 1:length(col)) {
    if (col[i]<=1) {
      col[i] = 92
    }
  }
  return(col)
}
bi$SpO2 <- replacewith(SpO2)

sum(is.na(SBP))
plot(SBP)
bi2 <- sort(SBP, decreasing = T)
tail(bi2, 10)
bi$SBP[is.na(bi$SBP)] <- 129

sum(is.na(bi$MAP))
plot(bi$MAP)
bi2 <- sort(bi$MAP, decreasing = T)
tail(bi2, 10)
bi$MAP[is.na(bi$MAP)] <- 92
bi$MAP <- replacewith(bi$MAP)

sum(is.na(bi$RR))
plot(bi$RR)
bi2 <- sort(bi$RR, decreasing = T)
tail(bi2, 10)
bi$RR[is.na(bi$RR)] <- 22
bi$RR[bi$RR > 50] <- 22

bi$FiO2[is.na(bi$FiO2)] <- 0.21

bi$Hb <- replacena(Hb, 14)
bi$TLC <- replacena(TLC, 8000)
bi$Platelets <- replacena(Platelets, 240000)
bi$K <- replacena(K, 4)
bi$Na <- replacena(Na, 140)
bi$Serum.Cr <- replacena(Serum.Cr, 1)
bi$Blood.Urea <- replacena(Blood.Urea, 29)
bi$Bili <- replacena(Bili, 0.8)
bi$Urine.output <- replacena(Urine.output, 1000)
bi$Lactate <- replacena(Lactate, 1.6)
bi$INR <- replacena(INR, 1)
bi$FiO2 <- replacena(FiO2, 21)
bi$PaO2 <- replacena(PaO2, 100)
bi$PaCO2 <- replacena(PaCO2, 40)
bi$pH <- replacena(pH, 7.4)
bi$A.a.gradient <- replacena(A.a.gradient, 12)
bi$HCO3 <- replacena(HCO3, 24)

bi2 <- sort(bi$RR, decreasing = T)
tail(bi2, 10)

high <- apply(bi[23:33], 2, gethigh)
low <- apply(bi[23:33], 2, getlow)

bi$Platelets[bi$Platelets<1000] <- 240000
bi$Urine.output[bi$Urine.output == 0] <- 1000
bi$TLC[bi$TLC > 200000] <- 8000 
bi$Na[bi$Na > 180] <- 140
bi$Age[is.na(bi$Age)] <- 51

write.csv(bi, file = "NN_source_file.csv")
