# get protein data
install.packages("ClustOfVar")
d <- data(package = "ClustOfVar")
nm <- d$results[, "Item"]
data(list = nm, package = "ClustOfVar")

# Hierarchical Clustering
# normalization is not needed since the data is in gramms

# remove counrty column
# protein <- protein[, -c(1, 1)]

# calculate euclidian distance 
distance <- dist(protein)

# cluster dendogram with complete linkage
hc.c <- hclust(distance)
plot(hc.c, hang = -1)
plot(hc.c)

# cluster dendogram with average linkage
hc.a <- hclust(distance, method = "average")
plot(hc.a, hang = -1)
plot(hc.a)



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
aggregate(protein, list(member.c), mean)

#silhouette plot to see how far are the samples from their cluster
# we can see Italy is an outlier in cluster 1
library(cluster)
plot(silhouette(cutree(hc.c,k.count), distance))

# hc.s <- hclust(distance, method = "ward.D")
# hc.s <- hclust(distance, method = "ward.D2")
# hc.s <- hclust(distance, method = "single")
# hc.s <- hclust(distance, method = "complete")
hc.s <- hclust(distance, method = "average")
# hc.s <- hclust(distance, method = "mcquitty")
hc.s <- hclust(distance, method = "median")
# hc.s <- hclust(distance, method = "centroid")

plot(silhouette(cutree(hc.s, 5), distance))

# plot(protein.numeric$White.Meat ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$Eggs ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$Milk ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$Fish ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$Cereals ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$Starchy.Foods ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$Nuts ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$Fruite.veg. ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)
# plot(protein.numeric$ ~ protein.numeric$Fish, data = protein.numeric, col = fit$cluster, pch = 20)


# k-means clustering
fit <- kmeans(protein, 5)
fit
plot(protein$White.Meat ~ protein$Fish, data = protein, col = fit$cluster, pch = 20)
