# reading data
require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

# descriptive statistics
summary(absence)

# groups and percentage
table <- table(absence$Reason.for.absence)
table.percent <- cbind(tbl, prop.table(tbl) * 100)

# printing the group tables
library(xtable)
data(table.percent)
xtable(table.percent)
