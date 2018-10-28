require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# split data into training and validation samples
# we'll use (training.size%) for training and (100 - training.size)% for validation
training.size <- 0.8
training.index <- sample.int(length(absence$Service.time), round(length(absence$Service.time) * training.size))
training.sample <- absence[training.index,]
validation.sample <- absence[-training.index,]

# use stepwise selection of variables by backwards elimination
# we'll consider all candidate variables and eliminate one at a time with highest p-value in fit
training.sample <- training.sample[ , -which(names(training.sample) %in% c("ID","Reason.for.absence", "Month.of.absence", "Day.of.the.week", "Seasons", "Disciplinary.failure", "Education", "Social.drinker", "Social.smoker", "Hit.target", "Son", "Absenteeism.time.in.hours", "Work.load.Average.day"))]
fit <- lm(training.sample$Service.time ~ ., data = training.sample)
summary(fit)
plot(fit)

# view the plots
library(psych)
pairs.panels(training.sample, col = "red")

# finding confidence intervals with .95 confidence level
confint(fit)

# find all predicted values for both training and validation samples
training.sample$pred.Service.time <- predict(fit, newdata = training.sample)
validation.sample$pred.Service.time <- predict(fit, newdata = validation.sample)

# check how good the model perfomance on the training set by correlation squared
# should be very close to R the squared from the fit model
training.correlation <- round(cor(training.sample$pred.Service.time, training.sample$Service.time), 2)
training.correlation^2

# check how good the model perfomance on the validation set by correlation squared
# should be relatively close to the R squared from the fit model
validation.correlation <- round(cor(validation.sample$pred.Service.time, validation.sample$Service.time), 2)
validation.correlation^2
