# get protein data
install.packages("ClustOfVar")
d <- data(package = "ClustOfVar")
nm <- d$results[, "Item"]
data(list = nm, package = "ClustOfVar")

# Hierarchical Clustering

# normalization is not needed since the data is in gramms
