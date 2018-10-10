# reading data
require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absense <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# descriptive statistics
summary(absense)

# groups and percentage
tbl <- table(absense$Reason.for.absence)
tbl_p <- cbind(tbl, prop.table(tbl) * 100)

# printing the group tables
library(xtable)
data(tbl_percent)
xtable(head(tbl_percent, 14))
xtable(tail(tbl_percent, 14))

# t-tests for smokers and drinkers
t.test(absense$Absenteeism.time.in.hours ~ absense$Social.drinker, alternative="two.sided", conf.level = 0.95)
t.test(absense$Absenteeism.time.in.hours ~ absense$Social.smoker, alternative="two.sided", conf.level = 0.95)
t.test(absense$Absenteeism.time.in.hours ~ absense$Disciplinary.failure, alternative="two.sided", conf.level = 0.95)

# contingency table and chi-squared test
chisq.test(table(absense$Reason.for.absence, absense$Day.of.the.week))
chisq.test(table(absense$Month.of.absence, absense$Day.of.the.week))
chisq.test(table(absense$Day.of.the.week, absense$Seasons))
chisq.test(table(absense$Day.of.the.week, absense$Education))