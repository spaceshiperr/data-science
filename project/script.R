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
# провести к одной шкале
library(cluster)
clusplot(absence, fit$cluster, color = TRUE, shade = TRUE, lines = 0, labels = 2)
# view the clusters for 2 variables
plot(absence$Absenteeism.time.in.hours ~ absence$Transportation.expense, col = fit$cluster)
