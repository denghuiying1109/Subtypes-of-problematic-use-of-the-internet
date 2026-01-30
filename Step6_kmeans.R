################################ K-means clustering ###############################
library(tidyverse)  
library(cluster)    
library(factoextra)

data<-read.csv("/Analysis data/CFA_factor_score.csv")
data <- subset(data, treat == 1)
mydata<-dplyr::select(data, EID,Reward,Negative_emotion,Executive_function)

rownames(mydata) <- mydata[,1] 
mydata <- mydata[,-1] 
mydata <- scale(mydata)

#visualize the distance matrix
distance <- get_dist(mydata)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#....................................................................................
library(NbClust)

nb<-NbClust(mydata, distance = "euclidean", min.nc = 2, max.nc = 10,
              method = "kmeans",index="all")

cluster_counts <- table(nb$Best.nc[1,]) 

barplot(cluster_counts, 
        main = "Optimal number of clusters - k = 3",
        xlab = "Number of clusters k",
        ylab = "Frequency among all indices",
        col = "steelblue", border = "black",
        las = 1,         
        cex.main = 1.2,  
        cex.lab = 1.2,   
        cex.axis = 1.2,  
        ylim = c(0, max(cluster_counts) + 1)) 

##........................................................................
set.seed(123)
k3 <- kmeans(mydata, centers = 3, nstart = 25)

mydata_km<-cbind(mydata_km,km_cluster3=k3$cluster)

write.csv(mydata_km,"/Analysis data/Kmeans_CFA.csv")