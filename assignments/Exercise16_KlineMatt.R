# Assignment: Exercise 16
# Name: Kline, Matthew
# Date: 2020-10-31

setwd("C:/Users/Matt Kline/Documents/GitHub/dsc520")


library(tidyverse)
library(caret)
library(class)
library(ggplot2)
library(cluster)

clustering_df <- read.csv("data/clustering-data.csv")

ggplot(clustering_df, aes(x=x,y=y)) + geom_point() + ggtitle("Cluster Data")


k2 <- kmeans(clustering_df, centers=2, nstart=25)
k3 <- kmeans(clustering_df, centers=3, nstart=25)
k4 <- kmeans(clustering_df, centers=4, nstart=25)
k5 <- kmeans(clustering_df, centers=5, nstart=25)
k6 <- kmeans(clustering_df, centers=6, nstart=25)
k7 <- kmeans(clustering_df, centers=7, nstart=25)
k8 <- kmeans(clustering_df, centers=8, nstart=25)
k9 <- kmeans(clustering_df, centers=9, nstart=25)
k10 <- kmeans(clustering_df, centers=10, nstart=25)
k11 <- kmeans(clustering_df, centers=11, nstart=25)
k12 <- kmeans(clustering_df, centers=12, nstart=25)


clustering_df$cluster_2 <- as.factor(k2$cluster)
clustering_df$cluster_3 <- as.factor(k3$cluster)
clustering_df$cluster_4 <- as.factor(k4$cluster)
clustering_df$cluster_5 <- as.factor(k5$cluster)
clustering_df$cluster_6 <- as.factor(k6$cluster)
clustering_df$cluster_7 <- as.factor(k7$cluster)
clustering_df$cluster_8 <- as.factor(k8$cluster)
clustering_df$cluster_9 <- as.factor(k9$cluster)
clustering_df$cluster_10 <- as.factor(k10$cluster)
clustering_df$cluster_11 <- as.factor(k11$cluster)
clustering_df$cluster_12 <- as.factor(k12$cluster)



ggplot(clustering_df, aes(x=x,y=y,color=cluster_2)) + geom_point() + ggtitle("Cluster Data k == 2")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_3)) + geom_point() + ggtitle("Cluster Data k == 3")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_4)) + geom_point() + ggtitle("Cluster Data k == 4")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_5)) + geom_point() + ggtitle("Cluster Data k == 5")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_6)) + geom_point() + ggtitle("Cluster Data k == 6")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_7)) + geom_point() + ggtitle("Cluster Data k == 7")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_8)) + geom_point() + ggtitle("Cluster Data k == 8")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_9)) + geom_point() + ggtitle("Cluster Data k == 9")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_10)) + geom_point() + ggtitle("Cluster Data k == 10")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_11)) + geom_point() + ggtitle("Cluster Data k == 11")
ggplot(clustering_df, aes(x=x,y=y,color=cluster_12)) + geom_point() + ggtitle("Cluster Data k == 12")


error <- NULL
p <- 1
k_value <- 2:12
for(i in k_value){
  cluster <- kmeans(clustering_df, centers= i, nstart=25)
  clustering_df$cluster <- as.factor(cluster$cluster)
  clustering_df$x.clust <- cluster$centers[clustering_df$cluster,"x"] - clustering_df$x
  clustering_df$y.clust <- cluster$centers[clustering_df$cluster,"y"] - clustering_df$y
  clustering_df$tot_dis <- sqrt((clustering_df$x.clust ** 2)+ (clustering_df$y.clust ** 2))
  
  error[p] <- mean(clustering_df$tot_dis)
  p <- p + 1
}

error_df <- data.frame(k_value, error)

ggplot(error_df, aes(x=k_value,y=error))+ geom_line(color="red") + geom_point() + xlab("K value") + ylab("Average of residuals") + ggtitle("Optimal K")
