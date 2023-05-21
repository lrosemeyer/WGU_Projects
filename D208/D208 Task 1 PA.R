medical <- read.csv("D://MS DA//D208 Task 1//medical_clean.csv")
library(dplyr)
library(visdat)
library(Hmisc)
library(car)
library(ggplot2)
library(corrplot)
library(fastDummies)
library(leaps)
head(medical)
str(medical)

mlr_medical_data <- medical %>%
  select(-CaseOrder:-Lng, -Item1:-Item8, -TimeZone, -Job, -Services)
summary(mlr_medical_data)

sum(duplicated(mlr_medical_data))
colSums(is.na(mlr_medical_data))

mlr_medical_data_zscore <- scale(mlr_medical_data %>% select(Population, Children, Age, Income, VitD_levels, Doc_visits, Full_meals_eaten, vitD_supp, Initial_days, TotalCharge, Additional_charges))
head(mlr_medical_data_zscore)

mlr_medical_data_zscore <- as.data.frame(mlr_medical_data_zscore) #https://www.statology.org/r-error-operator-is-invalid-for-atomic-vectors/
hist.data.frame(mlr_medical_data_zscore)

str(mlr_medical_data)
colnames(mlr_medical_data)
dummy_cols(mlr_medical_data, remove_first_dummy = T)


income_mlr <- lm(Income ~ ., data=mlr_medical_data)
summary(income_mlr)
vif(income_mlr)
cor(mlr_medical_data$Initial_days, mlr_medical_data$TotalCharge)

income_mlr <- lm(Income ~ . -Initial_days, data=mlr_medical_data)
vif(income_mlr)
summary(income_mlr)
anova(income_mlr)
incomefit <- regsubsets(Income ~ . -Initial_days, data=mlr_medical_data)
summary(incomefit)

reduced_income_mlr <- lm(Income ~ VitD_levels + Doc_visits + Initial_admin + Overweight + Reflux_esophagitis + TotalCharge, data=mlr_medical_data)
summary(reduced_income_mlr)
anova(reduced_income_mlr)
hist(reduced_income_mlr$residuals)
plot(reduced_income_mlr)
hist(predict(reduced_income_mlr), main="Predicted Income", xlab="")
hist(medical$Income, main = "Actual Income", xlab = "")
plot(medical$Income, predict(reduced_income_mlr))

