library(dplyr)
library(ggplot2)
library(purrr)

medical <- read.csv("D:\\MS DA\\D212\\medical_clean.csv")

sum(duplicated(medical))
sum(is.na(medical))
colnames(medical)
ageinc <- select(medical, 16:17)
ageinc <- as.data.frame(scale(ageinc))
plot(ageinc)
write.csv(ageinc, file = "D:/MS DA/D212/ageinc.csv")

model_km9 <- kmeans(ageinc, centers=9)
clust_km9 <- model_km9$cluster
ageinc_km9 <- mutate(ageinc, cluster=clust_km9)
head(ageinc_km9)
ggplot(ageinc_km9, aes(x=Age, y=Income, color=factor(cluster))) + geom_point()

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x=ageinc, centers=k)
  model$tot.withinss
})

elbow_df <- data.frame(k = 1:10, totwithinss = tot_withinss)
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + scale_x_continuous(breaks=1:10)

model_km3 <- kmeans(ageinc, centers=3)
clust_km3 <- model_km3$cluster
ageinc_km3 <- mutate(ageinc, cluster=clust_km3)
ggplot(ageinc_km3, aes(x=Age, y=Income, color=factor(cluster))) + geom_point()
