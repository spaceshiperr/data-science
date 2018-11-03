# read data
require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/hw2/data")
ads <- read.csv("Advertising.csv", header = TRUE,  sep=",")

# remove X column since we don't need it here
ads <- ads[ , -which(names(ads) %in% c("X"))]

# split data into training and validation samples
# we'll use (training.size%) for training and (100 - training.size)% for validation
training.size <- 0.8
training.index <- sample.int(length(ads$sales), round(length(ads$sales) * training.size))
training.sample <- ads[training.index,]
validation.sample <- ads[-training.index,]

# use stepwise selection of variables by backwards elimination
# we'll consider all candidate variables and eliminate one at a time
fit <- lm(sales ~ ., data = training.sample)
summary(fit)

# finding confidence intervals with .95 confidence level
confint(fit)

# plot the model
plot(fit)

# evaluate the model by cross-validation

# find all predicted values for both training and validation samples
training.sample$pred.sales <- predict(fit, newdata = training.sample)
validation.sample$pred.sales <- predict(fit, newdata = validation.sample)

# check how good the model perfomance on the training set by correlation squared
# should be very close to R the squared from the fit model
training.correlation <- round(cor(training.sample$pred.sales, training.sample$sales), 2)
training.correlation^2

# check how good the model perfomance on the validation set by correlation squared
# should be relatively close to the R squared from the fit model
validation.correlation <- round(cor(validation.sample$pred.sales, validation.sample$sales), 2)
validation.correlation^2
