# chi-squared tests with 0.05 confidence level for Absenteeism_at_work

require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# highly independent meaning high p-value
chisq.test(table(absence$Day.of.the.week, absence$Social.drinker))
chisq.test(table(absence$Disciplinary.failure, absence$Education))

# highly dependent meaning low p-value and high df
chisq.test(table(absence$Reason.for.absence, absence$Month.of.absence))
chisq.test(table(absence$Reason.for.absence, absence$Seasons))
chisq.test(table(absence$Reason.for.absence, absence$Education))
chisq.test(table(absence$Reason.for.absence, absence$Disciplinary.failure))
chisq.test(table(absence$Reason.for.absence, absence$Social.drinker))
chisq.test(table(absence$Reason.for.absence, absence$Social.smoker))
chisq.test(table(absence$Month.of.absence, absence$Disciplinary.failure))