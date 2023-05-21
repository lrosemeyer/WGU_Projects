churn <- read.csv("D://MS DA//D208 Task 2//churn_clean.csv")
library(dplyr)
library(ggplot2)
library(corrplot)
library(fastDummies)
library(Hmisc)
library(car)
library(caret)
library(randomForest)
library(pscl)
head(churn)
str(churn)

log_churn_data <- churn %>% select(-CaseOrder:-Lng, -Item1:-Item8, -TimeZone, -Job, -Port_modem, -Tablet, -OnlineSecurity, -OnlineBackup, -DeviceProtection)
summary(log_churn_data)
log_churn_data[sapply(log_churn_data, is.character)] <- lapply(log_churn_data[sapply(log_churn_data, is.character)], as.factor)
summary(log_churn_data)

sum(duplicated(log_churn_data))
colSums(is.na(log_churn_data))

log_churn_data_zscore <- scale(log_churn_data %>% select(Population, Children, Age, Income, Outage_sec_perweek, Email, Contacts, Yearly_equip_failure, Tenure, MonthlyCharge, Bandwidth_GB_Year))
head(log_churn_data_zscore)
log_churn_data_zscore <- as.data.frame(log_churn_data_zscore)
hist.data.frame(log_churn_data_zscore)

log_churn_data$Churn <- as.factor(log_churn_data$Churn)
churn_logreg <- glm(Churn ~ ., data=log_churn_data, family="binomial")
summary(churn_logreg)
vif(churn_logreg)

reduced_log_churn <- glm(Churn~. -Bandwidth_GB_Year -Population -Outage_sec_perweek -Income -Age -Email -Marital -Area -Yearly_equip_failure -Children, data=log_churn_data, family="binomial")
summary(reduced_log_churn)
vif(reduced_log_churn)

reduced_log_churn <- glm(Churn ~ Gender + Contacts + Techie + Contract + InternetService + Phone + Multiple + TechSupport + StreamingTV + StreamingMovies + PaperlessBilling + PaymentMethod + Tenure + MonthlyCharge, data=log_churn_data, family="binomial")
summary(reduced_log_churn)
vif(reduced_log_churn)

predicted <- predict(reduced_log_churn, type="response")
predicted
predicted <- ifelse(predicted>=0.5, "1", "0")
levels(as.factor(predicted))
actual <- ifelse(log_churn_data$Churn == "No","0","1")
levels(as.factor(actual))
confusionMatrix(as.factor(actual), as.factor(predicted))
                