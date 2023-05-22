medical <- read.csv("D://MS DA//D209 Task 1//medical_clean.csv", stringsAsFactors = T)
library(dplyr)
library(ggplot2)
library(class)
library(caTools)
library(caret)
library(pROC)
head(medical)
str(medical)

sum(duplicated(medical))
colSums(is.na(medical))


disease <- medical %>% select(Initial_admin:Stroke, Overweight:Asthma)
disease[2:12] <- ifelse(disease[2:12] == "Yes", 1, 0)

# Elective Admission = 1
# Emergency Admission = 2
# Observation Admission = 3
disease[1] <- ifelse(disease[1] == "Elective Admission", 1, ifelse(disease[1] == "Emergency Admission", 2, 3))

write.csv(disease, "D://MS DA//D209 Task 1//cleaned_data_set.csv")

head(disease)
str(disease)

set.seed(1)
sample <- sample.split(disease$Initial_admin, SplitRatio = 0.8)
train <- subset(disease, sample == T)
test <- subset(disease, sample == F)
dim(train)
dim(test)

write.csv(train, "D://MS DA//D209 Task 1//train_disease.csv")
write.csv(test, "D://MS DA//D209 Task 1//test_disease.csv")

train_admin <- disease[sample,1]
test_admin <- disease[!sample,1]
train_admin <- as.factor(train_admin)
test_admin <- as.factor(test_admin)
length(train_admin)
length(test_admin)

write.csv(train_admin, "D://MS DA//D209 Task 1//train_admin.csv")
write.csv(test_admin, "D://MS DA//D209 Task 1//test_admin.csv")


k.2 <- knn(train = train, test = test, cl = train_admin, k=2)
confusionMatrix(k.2, test_admin)
k.3 <- knn(train = train, test = test, cl = train_admin, k=3)
k.4 <- knn(train = train, test = test, cl = train_admin, k=4)
confusionMatrix(k.3, test_admin)
confusionMatrix(k.4, test_admin)
multiclass.roc(as.numeric(test$Initial_admin), as.numeric(k.4), levels = c(1,2,3))

