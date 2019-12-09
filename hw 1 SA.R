#########################################################
# Survival Analysis HW1
#########################################################

# Loading packages!
install.packages("survival")
install.packages("survminer")
install.packages("devtools")
library(devtools)
library(survival)
library(survminer)
library(haven)

summary(is.na(hurricane))
# Reading in data

hurricane <- read_sas("C:\\Users\\17046\\OneDrive\\Documents\\MSA 20\\Survival Analysis\\Homework1_SA\\hurricane.sas7bdat")

hurricane <- data.frame(hurricane)
summary(hurricane)

# Initial exploration
survive <- as.factor(hurricane$survive)
table(survive)/sum(table(survive))

# Creating surv object

hurr_surv <- Surv(hurricane$hour, hurricane$survive == 0)

hurr_km <- survfit(hurr_surv ~ 1, data = hurricane)

summary(hurr_km)$table
print(hurr_km)

plot(hurr_km, main = "Survival Function", xlab = "hour", ylab = "Survival Probability")

ggsurvplot(hurr_km, data = hurricane, conf.int = TRUE, palette = "purple",
           xlab = "hour", ylab = "Survival Probability", legend = "none",
           break.y.by = 0.1)

# Stratified Analysis #
strat <- survdiff(hurr_surv ~ reason, data = hurricane)
strat

# extracting a p-value for survdiff since pairwise isnt working
1 - pchisq(strat$chisq, length(strat$n) - 1)
devtools::install_github("zabore/ezfun")
ezfun::sdp(strat)

#fitting 
h_strat <- survfit(hurr_surv ~ reason, data = hurricane)
print(h_strat)


#plot stratified analysis
ggsurvplot(fit = h_strat, palette = c("red", "blue","orange", "purple", "green"), data=hurricane)
plot(h_strat, main="Survival Function", xlab = "function", ylab = "survival prob")

# do pairwise comparison between stratified curves
pairwise_survdiff(hurr_surv ~ reason, data = hurricane, p.adjust.method = "BH", na.action, rho=0)

survdiff(hurr_surv ~ reason, data=hurricane)

# Hazard Function #
summary(hurr_km)
hurr_km$hp <- hurr_km$n.event/hurr_km$n.risk
print(hurr_km$hp)


simple_haz <- merge(data.frame(time = seq(1,10,1)), data.frame(time = hurr_km$time, hp = hurr_km$hp), by = "time", all = TRUE)
simple_haz[is.na(simple_haz) == TRUE] <- 0
print(simple_haz)

plot(y = simple_haz$hp, x = simple_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

ggsurvplot(hurr_km, data = hurricane, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "hour", ylab = "Cumulative Hazard", legend = "none")

#for stratified 
summary(h_strat)
h_strat$hp <- h_strat$n.event/h_strat$n.risk
print(h_strat$hp)

strat_haz <- merge(data.frame(time = seq(1,10,1)), data.frame(time = h_strat$time, hp = h_strat$hp), by = "time", all = TRUE)
strat_haz[is.na(strat_haz) == TRUE] <- 0
print(strat_haz)

plot(y = strat_haz$hp, x = strat_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

ggsurvplot(h_strat, data = hurricane,
           fun = "cumhaz",
           conf.int = TRUE,
           palette = c("red", "blue","orange", "purple", "green"),
           xlab = "hour",
           ylab = "Cumulative Hazard",
           legend = "none")

# Hazard Function - Recidivism Data Set #
hurr_km$hp <- hurr_km$n.event/hurr_km$n.risk
hurr_haz <- merge(data.frame(time = seq(1,24,1)), data.frame(time = hurr_km$time, hp = hurr_km$hp), by = "time", all = TRUE)
hurr_haz[is.na(hurr_haz) == TRUE] <- 0

plot(y = hurr_haz$hp, x = hurr_haz$time, main = "Hazard Probability Function", xlab = "Tenure", ylab = "Hazard Probability",
     type = 'l')

ggsurvplot(hurr_km, data = hurricane, fun = "cumhaz", conf.int = TRUE, palette = "purple",
           xlab = "Week", ylab = "Cumulative Hazard", legend = "none")








