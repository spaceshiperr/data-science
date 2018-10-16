# t.test for Absenteeism_at_work

require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# groups with statistically significant p-value to reject H0

t.test(Distance.from.Residence.to.Work ~ Social.drinker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Service.time ~ Social.drinker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Age ~ Social.drinker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Son ~ Social.drinker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Pet ~ Social.drinker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Weight ~ Social.drinker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Body.mass.index ~ Social.drinker, alternative="two.sided", conf.level = 0.95, data = absence)

t.test(Age ~ Social.smoker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Son ~ Social.smoker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Weight ~ Social.smoker, alternative="two.sided", conf.level = 0.95, data = absence)
t.test(Body.mass.index ~ Social.smoker, alternative="two.sided", conf.level = 0.95, data = absence)

t.test(absence$Absenteeism.time.in.hours ~ absence$Disciplinary.failure, alternative="two.sided", conf.level = 0.95)

boxplot(Weight ~ Social.drinker, data = absence, col = 'aquamarine3', xlab = 'Social.drinker', ylab = 'Weight', main = 'Box plot for Weight by Social.drinker factor')
boxplot(Distance.from.Residence.to.Work ~ Social.drinker, data = absence)
boxplot(Service.time ~ Social.drinker, data = absence)
boxplot(Body.mass.index ~ Social.drinker, data = absence)