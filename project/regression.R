require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

absence.numeric <- absence[ , -which(names(absence) %in% c("ID","Reason.for.absence", "Month.of.absence", "Day.of.the.week", "Seasons", "Disciplinary.failure", "Education", "Social.drinker", "Social.smoker", "Hit.target", "Son", "Absenteeism.time.in.hours", "Work.load.Average.day"))]
pairs.panels(absence.numeric, col = "red")
fit <- lm(absence.numeric$Service.time ~ ., data = absence.numeric)
summary(fit)
plot(fit)