require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# scatter plots and correlation tests on certain variables
plot(absence$Height, absence$Absenteeism.time.in.hours)
cor.test(absence$Transportation.expense, absence$Distance.from.Residence.to.Work)

# correlation table for numeric variables
absence.numeric <- absence[ , -which(names(absence) %in% c("ID","Reason.for.absence", "Month.of.absence", "Day.of.the.week", "Seasons", "Disciplinary.failure", "Education", "Social.drinker", "Social.smoker", "Hit.target"))]
absence.correlation <- cor(absence.numeric)

# printing correlation table
library(xtable)
data(absence.correlation)
xtable(absence.correlation)

# correlogram 
library(corrplot)
corrplot(absence.correlation, method="color")