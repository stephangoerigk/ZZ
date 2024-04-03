library(factoextra)
library(NbClust)

iris

dt = dist(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")], method = "euclidean")

clust = hclust(dt, method = "ward.D2")
dend <- as.dendrogram(clust, hang = -1)

pruned = dynamicTreeCut::cutreeDynamic(clust, distM = as.matrix(dt), method = "hybrid", minClusterSize = 1)
table(pruned)

iris$pruned = pruned

ggplot(iris, aes(x = Sepal.Length, y= Petal.Width, colour = factor(pruned))) +
  geom_point()


plot(dend)

