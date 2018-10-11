# reading data
require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# descriptive statistics
summary(absence)

# groups and percentage
tbl <- table(absence$Reason.for.absence)
tbl_p <- cbind(tbl, prop.table(tbl) * 100)

# printing the group tables
library(xtable)
data(tbl_p)
xtable(head(tbl_p, 14))
xtable(tail(tbl_p, 14))

# t-tests for smokers and drinkers
t.test(absence$Absenteeism.time.in.hours ~ absence$Social.drinker, alternative="two.sided", conf.level = 0.95)
t.test(absence$Absenteeism.time.in.hours ~ absence$Social.smoker, alternative="two.sided", conf.level = 0.95)
t.test(absence$Absenteeism.time.in.hours ~ absence$Disciplinary.failure, alternative="two.sided", conf.level = 0.95)

# contingency table and chi-squared test
# not enough evidence to reject H0
chisq.test(table(absence$Reason.for.absence, absence$Day.of.the.week))
chisq.test(table(absence$Month.of.absence, absence$Day.of.the.week))
chisq.test(table(absence$Day.of.the.week, absence$Seasons))
chisq.test(table(absence$Day.of.the.week, absence$Education))
# enough evidence to reject H0
chisq.test(table(absence$Reason.for.absence, absence$Month.of.absence))
chisq.test(table(absence$Reason.for.absence, absence$Seasons))

# absence_cat <- absence[ , c("Reason.for.absence", "Month.of.absence", "Day.of.the.week", "Seasons", "Education")]

# scatter plots and correlation tests on certain variables
plot(absence$Height, absence$Absenteeism.time.in.hours)
cor.test(absence$Transportation.expense, absence$Distance.from.Residence.to.Work)
cor.test(absence$Son, absence$Absenteeism.time.in.hours)

# correlation table for continuous variables
absence_num <- absence[ , -which(names(absence) %in% c("ID","Reason.for.absence", "Month.of.absence", "Day.of.the.week", "Seasons", "Disciplinary.failure", "Education", "Social.drinker", "Social.smoker", "Hit.target"))]
absence_cor <- cor(absence_num)

# printing correlation table
library(xtable)
data(absence_cor)
xtable(absence_cor)

# correlogram 
library(corrplot)
corrplot(absence_cor, method="circle", type = "lower")
