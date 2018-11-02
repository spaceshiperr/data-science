# reading data
require("foreign")
setwd("/home/spaceshiperr/Documents/github/data-science/project/dataset")
absence <- read.csv("Absenteeism_at_work.csv", header = TRUE,  sep=";")

absence.numeric <- absence[ , -which(names(absence) %in% c("ID","Reason.for.absence", "Month.of.absence", "Day.of.the.week", "Seasons", "Disciplinary.failure", "Education", "Social.drinker", "Social.smoker", "Hit.target"))]

# cluster analysis

k.max <- 15
# determine the number of clusters from the scree plot
wss <- sapply(1:k.max, 
              function(k){kmeans(log10(absence.numeric+1), k, nstart=50,iter.max = k.max)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# let's assume optimal number of clusters from the plot is 5
cluster.count <- 5

# normalize data so that mean is 0 and std dev is 1
means <- apply(absence.numeric, 2, mean)
std.devs <- apply(absence.numeric, 2, sd)
absence.normalized <- scale(absence.numeric, means, std.devs)

# k-means
fit <- kmeans(absence.normalized, cluster.count)

# means for each cluster
fit.means <- aggregate(abse, list(fit$cluster), mean)
# print means table
library(xtable)
xtable(fit.means)

#plot all clusters
library(cluster)
clusplot(absence, fit$cluster, color = TRUE, shade = TRUE, lines = 0, labels = 2)
# view the clusters for 2 variables
plot(absence$Absenteeism.time.in.hours ~ absence$Transportation.expense, col = fit$cluster)

# Hierarchical Clustering 
summary(absence.normalized)

# calculate euclidian distance 
distance <- dist(absence.normalized)

# cluster dendogram with complete linkage
hc.c <- hclust(distance)
plot(hc.c, hang = -1)
plot(hc.c)
# cluster dendogram with average linkage
hc.a <- hclust(distance, method = "average")
plot(hc.a, hang = -1)
plot(hc.a)
# cluster membership
member.c <- cutree(hc.c, 3)
member.a <- cutree(hc.a, 3)
# compare methods
table(member.c, member.a)

# compare cluster means
# we can see which variables play important role in cluster analysis (with bigger variation)
# normalized values
aggregate(absence.normalized, list(member.c), mean)
# in original units
aggregate(absence.numeric, list(member.c), mean)

#silhouette plot to see how far are the samples from their cluster
library(cluster)
plot(silhouette(cutree(hc.c, 3), distance))
