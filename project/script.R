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
corrplot(absence_cor, method="color", type = "lower")

# two-variable linear regression
plot(absence$Age ~ absence$Service.time)
# calc age mean 
age_mean <- mean(absence$Age, na.rm = T)
# plot horizontal line at age_mean
abline(h = age_mean)
# use lm to fit a regression line through the data
# regression equation: y = ax + b
# 1st param: intercept is b
# 2nd param: it is a
# here y is Age and x is Service.time
model1 <- lm(absence$Age ~ absence$Service.time)
abline(model1, col = "red")
plot(model1)
summary(model1)

# multivariate linear regression
regression_vars <- data.frame(Age = absence$Age, Weight = absence$Weight, Body.mass.index = absence$Body.mass.index, Pet = absence$Pet, Service.time = absence$Service.time)
model <- lm(regression_vars$Service.time ~ ., data = regression_vars)

# cluster analysis
k.max <- 15
# determine the number of clusters from the plot
wss <- sapply(1:k.max, 
              function(k){kmeans(log10(absence_num+1), k, nstart=50,iter.max = k.max)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# let's assume optimal number of clusters from the plot is 5
cluster.count <- 5
fit <- kmeans(absence_num, cluster.count)

#plot all clusters
library(cluster)
clusplot(absence, fit$cluster, color = TRUE, shade = TRUE, lines = 0, labels = 2)
# view the clusters for 2 variables
plot(absence$Absenteeism.time.in.hours ~ absence$Transportation.expense, col = fit$cluster)
