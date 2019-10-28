## Cheating additional analysis ##


## WVS Analysis ##
## Data preparation
library(foreign)
library(ggplot2)
library(readstata13)
setwd("C:\\Users\\User\\Downloads")

wv <- WV6_Data_R
rm(WV6_Data_R)

wvs <- wv[, c("V2", "V3", "V75", "V198", "V199", "V200", "V201", "V238",
              "V239", "V240", "V242", "V248")] # keep variables of interest

colnames(wvs) <- c("country", "in", "success", "benefits", "fare",
                   "stealing", "cheating", "class", "income", "gender", "age", "education")

# Define negative categories missing as NA (as in codebook)
wvs[wvs <= -1] <- NA
wvs_c <- na.omit(wvs)

# Descriptive Analysis

summary(wvs_c$benefits); summary(wvs_c$fare); summary(wvs_c$stealing); summary(wvs_c$cheating)
hist(wvs_c$cheating, freq = F)


par(mfrow = c(2, 2))

## Income
cheatinc <- c(mean(wvs_c$cheating[wvs_c$income == 1]), mean(wvs_c$cheating[wvs_c$income == 2]),
              mean(wvs_c$cheating[wvs_c$income == 3]), mean(wvs_c$cheating[wvs_c$income == 4]),
              mean(wvs_c$cheating[wvs_c$income == 5]), mean(wvs_c$cheating[wvs_c$income == 6]),
              mean(wvs_c$cheating[wvs_c$income == 7]), mean(wvs_c$cheating[wvs_c$income == 8]),
              mean(wvs_c$cheating[wvs_c$income == 9]), mean(wvs_c$cheating[wvs_c$income == 10]))
names(cheatinc) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
barplot(cheatinc, main = "Cheating Per Income Group",
        xlab = "Income Group", ylab = "Cheating Preference",
        ylim = c(0, 2.5))

## Class
cheatcla <- c(mean(wvs_c$cheating[wvs_c$class == 1]), mean(wvs_c$cheating[wvs_c$class == 2]),
              mean(wvs_c$cheating[wvs_c$class == 3]), mean(wvs_c$cheating[wvs_c$class == 4]),
              mean(wvs_c$cheating[wvs_c$class == 5]))
names(cheatcla) <- c("1", "2", "3", "4", "5")
barplot(cheatcla, main = "Cheating Per Class",
        xlab = "Class", ylab = "Cheating Preference",
        ylim = c(0, 3))

## Success
cheatsuc <- c(mean(wvs_c$cheating[wvs_c$success == 1]), mean(wvs_c$cheating[wvs_c$success == 2]),
              mean(wvs_c$cheating[wvs_c$success == 3]), mean(wvs_c$cheating[wvs_c$success == 4]),
              mean(wvs_c$cheating[wvs_c$success == 5]), mean(wvs_c$cheating[wvs_c$success == 6]))
names(cheatsuc) <- c("1", "2", "3", "4", "5", "6")
barplot(cheatsuc, main = "Cheating Per Success Perception",
        xlab = "Success very much like me?", ylab = "Cheating Preference",
        ylim = c(0, 2.5))


## Education
cheatedu <- c(mean(wvs_c$cheating[wvs_c$education == 1]), mean(wvs_c$cheating[wvs_c$education == 2]),
              mean(wvs_c$cheating[wvs_c$education == 3]), mean(wvs_c$cheating[wvs_c$education == 4]),
              mean(wvs_c$cheating[wvs_c$education == 5]), mean(wvs_c$cheating[wvs_c$education == 6]),
              mean(wvs_c$cheating[wvs_c$education == 7]), mean(wvs_c$cheating[wvs_c$education == 8]),
              mean(wvs_c$cheating[wvs_c$education == 9]))
names(cheatedu) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
barplot(cheatedu, main = "Cheating Per Education",
        xlab = "Education Group", ylab = "Cheating Preference",
        ylim = c(0, 2.5))
title("Cheating World Wide", outer = T)


## Regression

cheat <- lm(cheating ~ success + income + class + education + age + gender, data = wvs_c)
summary(cheat)
## Income, class positive significant effect; education negative significant effect

cheat2 <- lm(cheating ~ success + income + class + education + age + gender + benefits + fare + stealing, data = wvs_c)
summary(cheat2)
## Income and class in both models good predictor; more education, less cheating; no success effect


## SPLIT BY COUNTRIES ##

## OECD
# Subset data

oecd <- subset(wvs_c, subset = country == 36 | country == 40 | country == 56 | country == 124 | country == 152 |
                 country == 203 | country == 208 | country == 233 | country == 246 | country == 250 | country == 276 |
                 country == 300 | country == 348 | country == 352 | country == 372 | country == 376 | country == 380 |
                 country == 392 | country == 410 | country == 428 | country == 440 | country == 442 | country == 484 |
                 country == 528 | country == 554 | country == 578 | country == 616 | country == 620 | country == 703 |
                 country == 705 | country == 724 | country == 752 | country == 756 | country == 792 | country == 826 |
                 country == 840) # not all OECD countries in this wave apparently?
unique(oecd$country)               

## Same analysis as above
# Descriptive Analysis

summary(oecd$benefits); summary(oecd$fare); summary(oecd$stealing); summary(oecd$cheating)
hist(oecd$cheating, freq = F)


par(mfrow = c(2, 2))

## Income
cheatinc_o <- c(mean(oecd$cheating[oecd$income == 1]), mean(oecd$cheating[oecd$income == 2]),
              mean(oecd$cheating[oecd$income == 3]), mean(oecd$cheating[oecd$income == 4]),
              mean(oecd$cheating[oecd$income == 5]), mean(oecd$cheating[oecd$income == 6]),
              mean(oecd$cheating[oecd$income == 7]), mean(oecd$cheating[oecd$income == 8]),
              mean(oecd$cheating[oecd$income == 9]), mean(oecd$cheating[oecd$income == 10]))
names(cheatinc_o) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
barplot(cheatinc_o, main = "Cheating Per Income Group",
        xlab = "Income Group", ylab = "Cheating Preference",
        ylim = c(0, 2))

## Class
cheatcla_o <- c(mean(oecd$cheating[oecd$class == 1]), mean(oecd$cheating[oecd$class == 2]),
              mean(oecd$cheating[oecd$class == 3]), mean(oecd$cheating[oecd$class == 4]),
              mean(oecd$cheating[oecd$class == 5]))
names(cheatcla_o) <- c("1", "2", "3", "4", "5")
barplot(cheatcla_o, main = "Cheating Per Class",
        xlab = "Class", ylab = "Cheating Preference",
        ylim = c(0, 2))

## Success
cheatsuc_o <- c(mean(oecd$cheating[oecd$success == 1]), mean(oecd$cheating[oecd$success == 2]),
              mean(oecd$cheating[oecd$success == 3]), mean(oecd$cheating[oecd$success == 4]),
              mean(oecd$cheating[oecd$success == 5]), mean(oecd$cheating[oecd$success == 6]))
names(cheatsuc_o) <- c("1", "2", "3", "4", "5", "6")
barplot(cheatsuc_o, main = "Cheating Per Success Perception",
        xlab = "Success very much like me?", ylab = "Cheating Preference",
        ylim = c(0, 2))


## Education
cheatedu_o <- c(mean(oecd$cheating[oecd$education == 1]), mean(oecd$cheating[oecd$education == 2]),
              mean(oecd$cheating[oecd$education == 3]), mean(oecd$cheating[oecd$education == 4]),
              mean(oecd$cheating[oecd$education == 5]), mean(oecd$cheating[oecd$education == 6]),
              mean(oecd$cheating[oecd$education == 7]), mean(oecd$cheating[oecd$education == 8]),
              mean(oecd$cheating[oecd$education == 9]))
names(cheatedu_o) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
barplot(cheatedu_o, main = "Cheating Per Education",
        xlab = "Education Group", ylab = "Cheating Preference",
        ylim = c(0, 2))
title("Cheating OECD", outer = T)


## Regression

cheat_o <- lm(cheating ~ success + income + class + education + age + gender, data = oecd)
summary(cheat_o)
## Income, class positive but no significant effect

cheat2_o <- lm(cheating ~ success + income + class + education + age + gender + benefits + fare + stealing, data = oecd)
summary(cheat2_o)
## Income in second model good predictor



###MANAGEMENT GAME###

## Data preparation
require(foreign)
require(ggplot2)
require(MASS)
install.packages("Hmisc")
require(Hmisc)
require(reshape2)
cheat <- read.csv2("entre1.csv")
median(cheat$jnCorrectRET) # median is 6; threshold for classification
cheat$ability <- ifelse(cheat$jnCorrectRET > 6, 1, 0)

cheat$jedec <- cheat$jedec1 + cheat$jedec2 +
  cheat$jedec3 + cheat$jedec4 + cheat$jedec5 # avoid multiple conditions

cheat$cheating <- NA
cheat$cheating <- ifelse(cheat$jedec == 5, 0,
                      ifelse(cheat$jedec == 0, 2, 1)) # trichotomous classification

# cheat == 0 = honest all the time (1 over all 5 rounds)
# cheat == 1 = honest and cheat sometimes
# cheat == 2 = cheat all the time (0 over all 5 rounds)
# higher values -> more cheating

cheat2 <- subset(cheat, subset = jAuditRate == 0) # look only at 0% audit sessions

 # create binary variable ability
prop.table(table(cheat$jnCorrectRET > 6))
prop.table(table(cheat$ability)) #check for correct classification

## Descriptive analysis
m1 <- lm(jCheat ~ ability + jage_subject + jgender, data = cheat2)
summary(m1)
tapply(cheat2$jCheat, cheat2$ability, FUN = mean)
## No significant relationship between ability and cheating probability

m2 <- lm(cheating ~ ability + jage_subject + jgender, data = cheat2)
summary(m2)
tapply(cheat2$cheating, cheat2$ability, FUN = mean)
## significant relationship
cheat2$cheating <- as.factor(cheat2$cheating)
ordlog1 <- polr(cheating ~ ability + jage_subject + jgender, data = cheat2, Hess = TRUE)
summary(ordlog1)
## signficant relationship

m3 <- lm(jCheat ~ ability + jage_subject + jgender, data = cheat)
summary(m3)
tapply(cheat$jCheat, cheat$ability, FUN = mean)
## No significant relationship between ability and cheating probability

m4 <- lm(cheating ~ ability + jage_subject + jgender, data = cheat)
summary(m4)
tapply(cheat$cheating, cheat$ability, FUN = mean)
## significant positive relationship
cheat$cheating <- as.factor(cheat$cheating)
ordlog2 <- polr(cheating ~ ability + jage_subject + jgender, data = cheat, Hess = TRUE)
summary(ordlog2)

## weighted average approach
# jedec1: 110 vs 520
# jedec2: 207 vs 520
# jedec3: 320 vs 520
# jedec4: 420 vs 520
# jedec5: 520 vs 520

## weighting by incentives for truth (fraction of truth cost)
w1 <- 110 / (110 + 520)
w2 <- 207 / (207 + 520)
w3 <- 320 / (320 + 520)
w4 <- 420 / (420 + 520)
w5 <- 520 / (520 + 520)

## weighted truth average for each participant
cheat$truthavg <- (cheat$jedec1 * w1 + cheat$jedec2 * w2 + cheat$jedec3 * w3 + cheat$jedec4 * w4 + cheat$jedec5 * w5) / 5
m5 <- lm(truthavg ~ ability + jage_subject + jgender, data = cheat)
summary(m5)
## weak negative relationship more ability less truthfulness

######REPLICATION GIBSON ET AL########
gibson1 <- read.dta("gibson.dta")
gibson2 <- read.dta("gibsonfull.dta")
# Focus on ratiocorrect in gibson1 (ability) and truth in gibson 2 (lying)
gibson <- data.frame(id = gibson1$vpn, ability = gibson1$ratiocorrect, truth = gibson2$truth,
                     sex = gibson2$sex, age = gibson2$age)
model1 <- lm(truth ~ ability + sex + age, data = gibson)
summary(model1)
## Significant relationship
## cluster by subject






