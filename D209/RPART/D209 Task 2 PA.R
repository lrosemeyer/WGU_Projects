churn <- read.csv("D://MS DA//D209 Task 2//churn_clean.csv")
library(dplyr)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)

head(churn)
str(churn)
sum(duplicated(churn))
colSums(is.na(churn))

services <- churn %>% select(20, 27:38)
head(services)
str(services)

write.csv(services, "D://MS DA//D209 Task 2//cleaned_data.csv")

set.seed(1)
sample <- sample.split(services$Churn, SplitRatio = 0.8)
train <- subset(services, sample == T)
test <- subset(services, sample == F)
dim(train)
dim(test)

write.csv(train, "D://MS DA//D209 Task 2//train_services.csv")
write.csv(test, "D://MS DA//D209 Task 2//test_services.csv")

train_churn <- services[sample,1]
test_churn <- services[!sample, 1]
train_churn <- as.factor(train_churn)
test_churn <- as.factor(test_churn)
length(train_churn)
length(test_churn)

write.csv(train_churn, "D://MS DA//D209 Task 2//train_churn.csv")
write.csv(test_churn, "D://MS DA//D209 Task 2//test_churn.csv")

tree <- rpart(Churn ~ ., data=train)
rpart.plot(tree)
p <- predict(tree, train, type = "class")
p_test <- predict(tree, test, type = "class")
confusionMatrix(p, as.factor(train$Churn))
confusionMatrix(p_test, as.factor(test$Churn))
p_test <- ifelse(p_test == "Yes", 1, 0)
test$Churn <- ifelse(test$Churn == "Yes", 1, 0)
mean((p_test - test$Churn)^2)
