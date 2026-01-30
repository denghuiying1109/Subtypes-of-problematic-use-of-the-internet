################################ Dice coefficient ###################################
LPA<-read.csv("/Analysis data/LPA_CFA.csv")
kmeans<-read.csv("/Analysis data/Kmeans_CFA.csv")

LPA<-dplyr::select(LPA, EID,Group_LPA)
kmeans<-dplyr::select(LPA, EID,km_cluster3)

LPA_Kmeans<-merge(LPA,kmeans,by=c("EID"), all.x=TRUE)

data <- subset(LPA_Kmeans, treat == 1)

group_A <- data$Group_LPA
group_B <- data$km_cluster3

dice_coefficient <- function(group_A, group_B) {
  A_intersect_B <- sum(group_A == group_B)
  dice <- (2 * A_intersect_B) / (length(group_A) + length(group_B))
  return(dice)
}

dice <- dice_coefficient(group_A, group_B)
print(dice)