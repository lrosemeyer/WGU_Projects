library(dplyr)
library(factoextra)

medical <- read.csv('D:/MS DA/D212/Task 2/medical_clean.csv')
sum(duplicated(medical))
sum(is.na(medical))
colnames(medical)
ageinc <- medical[,c(11,15:17,21:24,40:42)]
ageinc_scale <- scale(ageinc)
head(ageinc_scale)
write.csv(ageinc_scale, "D:/MS DA/D212/Task 2/ageinc_scale.csv")

ageinc.pca <- prcomp(ageinc_scale)
ageinc.pca$rotation
fviz_eig(ageinc.pca, choice="eigenvalue", addlabels=T)
summary(ageinc.pca)
