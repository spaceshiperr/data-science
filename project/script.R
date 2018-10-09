# reading data
require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absense <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# descriptive statistics
summary(absense)

# groups and percentage
tbl <- table(absense$Reason.for.absence)
tbl_percent <- cbind(tbl, prop.table(tbl) * 100)

# printing the group tables
library(xtable)
data(tbl_percent)
xtable(head(tbl_percent, 14))
xtable(tail(tbl_percent, 14))