require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/hw2")
ads <- read.csv("Advertising.csv", header = TRUE,  sep=",")

ads.channels = ads[ , -which(names(ads) %in% c("X"))]
fit <- lm(ads.channels$sales ~ ., data = ads.channels)
summary(fit)
plot(fit)
