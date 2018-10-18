require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# statistically significant result to reject H0 with low p-value therefore high significance level

aov.results <- aov(absence$Absenteeism.time.in.hours ~ absence$Reason.for.absence)
aov.results <- aov(absence$Distance.from.Residence.to.Work ~ absence$Reason.for.absence)
aov.results <- aov(absence$Work.load.Average.day ~ absence$Reason.for.absence)

# Disciplinary.failure
aov.results <- aov(absence$Absenteeism.time.in.hours ~ absence$Disciplinary.failure)
aov.results <- aov(absence$Age ~ absence$Disciplinary.failure)

# Month.of.absence
aov.results <- aov(absence$Transportation.expense ~ absence$Month.of.absence)
aov.results <- aov(absence$Work.load.Average.day ~ absence$Month.of.absence)

# Day.of.the.week
aov.results <- aov(absence$Distance.from.Residence.to.Work ~ absence$Day.of.the.week)
aov.results <- aov(absence$Body.mass.index ~ absence$Day.of.the.week)
aov.results <- aov(absence$Son ~ absence$Day.of.the.week)

# Seasons
aov.results <- aov(absence$Work.load.Average.day ~ absence$Seasons)

# Education
aov.results <- aov(absence$Distance.from.Residence.to.Work ~ absence$Education)
aov.results <- aov(absence$Service.time ~ absence$Education)
aov.results <- aov(absence$Son ~ absence$Education)
aov.results <- aov(absence$Age ~ absence$Education)
aov.results <- aov(absence$Weight ~ absence$Education)
aov.results <- aov(absence$Body.mass.index ~ absence$Education)

# Social.drinker
aov.results <- aov(absence$Age ~ absence$Social.drinker)
aov.results <- aov(absence$Service.time ~ absence$Social.drinker)
aov.results <- aov(absence$Distance.from.Residence.to.Work ~ absence$Social.drinker)
aov.results <- aov(absence$Son ~ absence$Social.drinker)
aov.results <- aov(absence$Pet ~ absence$Social.drinker)
aov.results <- aov(absence$Weight ~ absence$Social.drinker)
aov.results <- aov(absence$Transportation.expense ~ absence$Social.drinker)
aov.results <- aov(absence$Body.mass.index ~ absence$Social.drinker)

# Social.smoker
aov.results <- aov(absence$Age ~ absence$Social.smoker)
aov.results <- aov(absence$Son ~ absence$Social.smoker)
aov.results <- aov(absence$Pet ~ absence$Social.smoker)
aov.results <- aov(absence$Weight ~ absence$Social.smoker)
aov.results <- aov(absence$Body.mass.index ~ absence$Social.smoker)

summary(aov.results)