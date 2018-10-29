# get protein data
install.packages("ClustOfVar")

d <- data(package = "ClustOfVar")
nm <- d$results[, "Item"]
data(list = nm, package = "ClustOfVar")

# hierarchical clustering

# calculate euclidian distance 
distance <- dist(protein)

# cluster dendogram

# hc.s <- hclust(distance, method = "ward.D")
# hc.s <- hclust(distance, method = "ward.D2")
# hc.s <- hclust(distance, method = "single")
# hc.s <- hclust(distance, method = "average")
# hc.s <- hclust(distance, method = "mcquitty")
# hc.s <- hclust(distance, method = "centroid")

hc.c <- hclust(distance, method = "complete")
hc.a <- hclust(distance, method = "average")

plot(hc.c, hang = -1)
plot(hc.a, hang = -1)

# determine the number of clusters from the scree plot
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(log10(protein+1), k, nstart=50,iter.max = k.max)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares", 
     main = "Scree plot")

k.count <- 5

# cluster membership
member.c <- cutree(hc.c, k.count)
member.a <- cutree(hc.a, k.count)

# compare methods
table(member.c, member.a)

# compare cluster means
# we can see which variables play important role in cluster analysis (with bigger variation)
means <- aggregate(protein, list(member.c), mean)
# print table
library(xtable)
xtable(means)

#silhouette plot to see how far are the samples from their cluster
# we can see Italy is an outlier in cluster 1
library(cluster)
plot(silhouette(cutree(hc.m, k.count), distance))
plot(silhouette(cutree(hc.c, k.count), distance))

# k-means clustering
fit <- kmeans(protein, k.count)

# observe the clusters
plot(protein$White.Meat ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
plot(protein$Eggs ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
plot(protein$Milk ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
plot(protein$Fish ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
plot(protein$Cereals ~ protein$Fish, data = protein, col = fit$cluster, pch = 20, main = "Scatter plot for protein data with k-means clustering")
plot(protein$Starchy.Foods ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
plot(protein$Nuts ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
plot(protein$Fruite.veg. ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
plot(protein$Red.Meat ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)

# means for each cluster
fit.means <- aggregate(protein, list(fit$cluster), mean)
# print means table
library(xtable)
xtable(fit.means)
