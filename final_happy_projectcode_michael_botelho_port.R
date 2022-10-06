rm(list=ls())
data<- read.csv(file.path("happiness_metrics_data.csv"), header=FALSE)

# Fixing Data Types
data[,6] <- as.numeric(as.character(data[,6]))
data[,7] <- as.numeric(as.character(data[,7]))
data[,19] <- as.numeric(as.character(data[,19]))
data[,21] <- as.numeric(as.character(data[,21]))
data[,22] <- as.numeric(as.character(data[,22]))
data[,24] <- as.numeric(as.character(data[,24]))
data[,23] <- as.numeric(as.character(data[,23]))
data[,20] <- as.numeric(as.character(data[,20]))
data[,18] <- as.numeric(as.character(data[,18]))
str(data)

# Naming Variables
happyrank <- data$V6
happyscore <- data$V7
elec <- data$V21
lfp_rate <- data$V22
unem <- data$V24
armed_fp <- data$V23
imports_gs <- data$V19
bus_score <- data$V20
alcohol <- data$V18

#Naming Columns
colnames(data)[6] <- "happyrank"
colnames(data)[7] <- "happyscore"
colnames(data)[21] <- "elec"
colnames(data)[22] <- "lfp_rate"
colnames(data)[24] <- "unem"
colnames(data)[23] <- "armed_fp"
colnames(data)[19] <- "imports_gs"
colnames(data)[20] <- "bus_score"
colnames(data)[18] <- "alcohol"

#Regressions: 1 (rank), 2 (score), 3 (fitted score)
model1 <- lm(happyrank~elec+unem+lfp_rate+armed_fp+imports_gs+bus_score+alcohol)
summary(model1)
model2 <- lm(happyscore~elec+unem+lfp_rate+armed_fp+imports_gs+bus_score+alcohol)
summary(model2)
model11 <- lm(happyscore~elec+imports_gs+bus_score)
summary(model11)


#T-Tests for Each Variable
elec_hs_reg <- lm(happyscore~elec)
summary(elec_hs_reg)
qt(1-(0.01/2), 136)
(1-pt(10.97, 136))*2

alcohol_hs_reg <- lm(happyscore~alcohol)
summary(alcohol_hs_reg)
qt(1-(0.01/2), 133)
(1-pt(3.994, 133))*2

armedfp_hs_reg <- lm(happyscore~armed_fp)
summary(armedfp_hs_reg)
qt(1-(0.80/2), 131)
(1-pt(0.129, 131))*2

busscore_hs_reg <- lm(happyscore~bus_score)
summary(busscore_hs_reg)
qt(1-(0.01/2), 135)
(1-pt(9.987, 135))*2

gs_hs_reg <- lm(happyscore~imports_gs)
summary(gs_hs_reg)
qt(1-(0.1232/2), 132)
(1-pt(1.551, 132))*2

lfp_hs_reg <- lm(happyscore~lfp_rate)
summary(lfp_hs_reg)
qt(1-(0.126/2), 135)
(1-pt(1.537, 135))*2

unem_hs_reg <- lm(happyscore~unem)
summary(unem_hs_reg)
qt(1-(0.28/2), 135)
(1-pt(1.072, 135))*2



#F-Test
rest <- lm(happyscore~ elec+ bus_score +alcohol)
summary(rest)
unrest <- lm(happyscore~elec +bus_score +alcohol + unem + armed_fp + lfp_rate + imports_gs)
summary(unrest)
rss.r <- sum(residuals(rest)^2)
rss.ur <- sum(residuals(unrest)^2)
test.stat <- ((rss.r-rss.ur)/4/rss.ur/(138-8))
test.stat
f.crit <- qf((1-0.05), 4, (138-8))              
f.crit              
f.pvalue <- (1-pf(test.stat, 4, 138-8))
f.pvalue


#Final Model
mainreg <- lm(happyscore~ elec+ bus_score +alcohol)
summary(mainreg)
