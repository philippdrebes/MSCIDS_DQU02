
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Install packages

if (!require("readxl")) install.packages("readxl", dependencies=TRUE)
if (!require("MVA")) install.packages("MVA", dependencies=TRUE)
if (!require("cluster")) install.packages("cluster", dependencies=TRUE)

library(readxl)
library(MVA)
library(cluster)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Read data

data <- read_excel("<YOUR PATH>/learning.xlsx")
head(data)
data
plot(data)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Preparation

datas <- scale(data, center = FALSE, scale = TRUE)
boxplot(datas)

plot(datas)

dists <- dist(datas)


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Clustering → Variant 4 clusters
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––---------------------------------– 

km <- kmeans(datas, centers = 4, nstart = 10)
groups_km <- km$cluster
groups_km


cluster_size <- cbind(sum(groups_km == 1), sum(groups_km == 2), sum(groups_km == 3), sum(groups_km == 4))
cluster_size


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Plot / Scree plot / Silhouette plot

plot(datas, pch = groups_km, col=groups_km, lwd=2)
legend("topright", legend = 1:4, pch = 1:4, col=1:4, bty="n")

reps <- rep(0, 6)
for (i in 1:6) reps[i] <- sum(kmeans(datas, centers = i, nstart = 20)$withinss)
par(mfrow = c(1,1))
plot(1:6, reps, type = "b", xlab = "Number of groups", ylab = "Sum of squares")

plot(silhouette(groups_km, dists))


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Hierarchical cluster analysis

dc <- dist(datas, method = "euclidean")
dc

cc <- hclust(dc, method = "complete")
plot(cc,cex = 0.3, hang = -1)




# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Clustering → Variant 3 clusters
# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––---------------------------------– 

km <- kmeans(datas, centers = 3, nstart = 10)
groups_km <- km$cluster
groups_km


cluster_size <- cbind(sum(groups_km == 1), sum(groups_km == 2), sum(groups_km == 3))
cluster_size


# –––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––– Plot / Scree plot / Silhouette plot

plot(datas, pch = groups_km, col=groups_km, lwd=2)
legend("topright", legend = 1:3, pch = 1:3, col=1:3, bty="n")

reps <- rep(0, 6)
for (i in 1:6) reps[i] <- sum(kmeans(datas, centers = i, nstart = 20)$withinss)
par(mfrow = c(1,1))
plot(1:6, reps, type = "b", xlab = "Number of groups", ylab = "Sum of squares")

plot(silhouette(groups_km, dists))

