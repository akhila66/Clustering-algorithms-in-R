install.packages("tidyverse")
library(tidyverse)
library(onehot)
library(cluster)
library(reshape2)

header <- c('Sex','Length','Diameter','Height','Whole_weight','Shucked_weight','Viscera_weight','Shell_weight','Rings')
df <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",header = FALSE)
names(df) <- header # setting headers



funModeling::df_status(df)

encoder = onehot(df)
x <- data.frame(predict(encoder, df))

funModeling::df_status(x)


set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(x, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


kmeansObj <- kmeans(x,4)
kmeansObj$withinss

ggData$cluster <- as.factor(kmeansObj$cluster)
pairs(ggData, col=ggData$cluster)

melted<- melt(ggData,id.vars='cluster')

head(melted)

ggplot(melted, aes(x = cluster, y = value)) + 
  geom_boxplot()+facet_wrap(~variable)


#--------------------------------------------

kmedoidObj <- pam(df,4)
ggData$cluster <- as.factor(kmedoidObj$clustering)
pairs(ggData, col=ggData$cluster)

melted <- melt(ggData,id.vars='cluster')

head(melted)

ggplot(melted, aes(x = cluster, y = value)) + 
  geom_boxplot()+facet_wrap(~variable)
#-------------------------------------------
#https://uc-r.github.io/hc_clustering

#Hierarchical clustering can be divided into two main types: agglomerative and divisive.

#Note that agglomerative clustering is good at identifying small clusters. Divisive hierarchical clustering is good at identifying large clusters.

#https://www.r-bloggers.com/hierarchical-clustering-in-r-2/

HAgloclusters <- hclust(dist(x))

clusterCut <- cutree(HAgloclusters, 4)

ggData$cluster <- as.factor(clusterCut)
pairs(ggData, col=ggData$cluster)

melted<- melt(ggData,id.vars='cluster')
head(melted)
ggplot(melted, aes(x = cluster, y = value)) + 
  geom_boxplot()+facet_wrap(~variable)


hclus<-hclust(dist(x),method="complete")   #notice the long chains (e.g., very unbalanced)
clusterCut <- cutree(hclus, 4)
ggData$cluster <- as.factor(clusterCut)
pairs(ggData, col=ggData$cluster)



# Dissimilarity matrix
d <- dist(x, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
clusterCut <- cutree(hc1, 4)
ggData$cluster <- as.factor(clusterCut)
pairs(ggData, col=ggData$cluster)

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

#----------

#https://www.datanovia.com/en/lessons/dbscan-density-based-clustering-essentials/

library(dbscan) # giving worst results

db <- dbscan(x, eps = .5, minPts = 200)
ggData$cluster <- as.factor(db$cluster)
pairs(ggData, col=ggData$cluster)

 melted<- melt(ggData,id.vars='cluster')
head(melted)
ggplot(melted, aes(x = cluster, y = value)) + 
  geom_boxplot()+facet_wrap(~variable)





