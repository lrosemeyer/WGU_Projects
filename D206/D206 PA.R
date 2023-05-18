#Load file and look at head and str to get info about the data
medical <- read.csv("D:/MS DA/D206/medical_raw_data.csv")
head(medical)
str(medical)

#set max print option to 10000, look for duplicate observations
options(max.print = 10000)
sum(duplicated(medical))

#load visdat and check for missing values
library(visdat)
colSums(is.na(medical))
vis_miss(medical)

#Use Mice to fill in missing values and keep distributions similar
library(mice)
medical$Soft_drink <- as.factor(medical$Soft_drink)
md.pattern(medical,rotate.names = TRUE)
imputed_medical <- mice(medical)
summary(imputed_medical)
imputed_medical$imp$Income
imputed_medical <- complete(imputed_medical)

#Verify there are no missing values
vis_miss(imputed_medical)

#Verify all variables with imputed values have similar distribution as before imputation
summary(medical$Income)
summary(imputed_medical$Income)
hist(medical$Income, labels=T, breaks = 10, main="Income Before Imputation", xlab="Income")
hist(imputed_medical$Income, labels=T, breaks=10, main="Income After Imputation", xlab="Income")
barplot(table(medical$Children), main="Children Before Imputation", xlab="Children")
barplot(table(imputed_medical$Children), main="Children After Imputation", xlab="Children")
hist(medical$Age, main="Age Before Imputation", xlab="Age", labels=T)
hist(imputed_medical$Age, main="Age After Imputation", xlab="Age", labels=T)
prop.table(table(medical$Soft_drink))
prop.table(table(imputed_medical$Soft_drink))
prop.table(table(medical$Overweight))
prop.table(table(imputed_medical$Overweight))
prop.table(table(medical$Anxiety))
prop.table(table(imputed_medical$Anxiety))
hist(medical$Initial_days, main="Initial Days Before Imputation", xlab="Initial Days", labels=T)
hist(imputed_medical$Initial_days,main="Initial Days After Imputation", xlab="Initial Days", labels=T)

#Check for outliers in numeric data
str(imputed_medical)
boxplot(imputed_medical$Children, main="Children", horizontal = T)
table(imputed_medical$Children)#Has reasonable outliers
boxplot(imputed_medical$Age)
boxplot(imputed_medical$Income)
hist(imputed_medical$Income, main="Income", xlab="Income") #Has reasonable outliers
boxplot(imputed_medical$Doc_visits)
boxplot(imputed_medical$VitD_levels, main="Vitamin D Levels", horizontal=T) #Has Outliers
boxplot(imputed_medical$Full_meals_eaten) #Has some outliers
boxplot(imputed_medical$VitD_supp) #Has some outliers
boxplot(imputed_medical$Initial_days)
boxplot(imputed_medical$TotalCharge) #Has outliers/should dive deeper
boxplot(imputed_medical$Additional_charges) #Has reasonable outliers

#Dive deeper into VitD_levels
hist(imputed_medical$VitD_levels, breaks=20)
summary(imputed_medical$VitD_levels)

#Dive deeper into Full_meals_eaten
table(imputed_medical$Full_meals_eaten)
summary(imputed_medical$Full_meals_eaten)

#Dive deeper into VitD_supp
hist(imputed_medical$VitD_supp, breaks=5, right = F, labels = TRUE)
summary(imputed_medical$VitD_supp)
table(imputed_medical$VitD_supp)


#Dive deeper into Total Charge (per day)
hist(imputed_medical$TotalCharge, main="Total Charge Before Imputation", xlab="Total Charge")
summary(imputed_medical$TotalCharge)

#Replace values to the right of the gap in the histogram with the median TotalCharge
sort(boxplot(imputed_medical$TotalCharge)$out, decreasing=T)
hist(imputed_medical$TotalCharge)
imputed_medical$TotalCharge[imputed_medical$TotalCharge>11000] <- median(imputed_medical$TotalCharge)
boxplot(imputed_medical$TotalCharge)
hist(imputed_medical$TotalCharge, main="Total Charge After Imputation", xlab="Total Charge", labels=T)

#Dive deeper into Additional_charges
hist(imputed_medical$Additional_charges)


#Start re-expressing categorical variables
library(dplyr)
str(imputed_medical)

#Re-express education
unique(imputed_medical$Education)
edu.num <- recode(imputed_medical$Education, "Some College, Less than 1 Year"=12, "Some College, 1 or More Years, No Degree"=13, "GED or Alternative Credential"=12, "Regular High School Diploma"=12, "Bachelor's Degree"=16, "Master's Degree"=18, "Nursery School to 8th Grade"=8, "9th Grade to 12th Grade, No Diploma"=11, "Doctorate Degree"=22, "Associate's Degree"=14, "Professional School Degree"=12, "No Schooling Completed"=0)
imputed_medical$Education_numeric <- as.numeric(edu.num)
str(imputed_medical)
unique(imputed_medical$Education_numeric)

#Re-express ReAdmis
unique(imputed_medical$ReAdmis)
readmis.num <- recode(imputed_medical$ReAdmis, "No"=0, "Yes"=1)
imputed_medical$ReAdmis <- as.numeric(readmis.num)
unique(imputed_medical$ReAdmis)

#Re-express Soft_drink
unique(imputed_medical$Soft_drink)
soft.num <- recode(imputed_medical$Soft_drink, "No"=0, "Yes"=1)
imputed_medical$Soft_drink <- as.numeric(soft.num)
unique(imputed_medical$Soft_drink)

#Re-express HighBlood
unique(imputed_medical$HighBlood)
highblood.num <- recode(imputed_medical$HighBlood, "No"=0, "Yes"=1)
imputed_medical$HighBlood <- as.numeric(highblood.num)
unique(imputed_medical$HighBlood)

#Re-express Stroke
unique(imputed_medical$Stroke)
stroke.num <- recode(imputed_medical$Stroke, "No"=0, "Yes"=1)
imputed_medical$Stroke <- as.numeric(stroke.num)
unique(imputed_medical$Stroke)

#Re-express Arthritis
unique(imputed_medical$Arthritis)
arth.num <- recode(imputed_medical$Arthritis, "No"=0, "Yes"=1)
imputed_medical$Arthritis <- as.numeric(arth.num)
unique(imputed_medical$Arthritis)

#Re-express Diabetes
unique(imputed_medical$Diabetes)
diab.num <- recode(imputed_medical$Diabetes, "No"=0, "Yes"=1)
imputed_medical$Diabetes <- as.numeric(diab.num)
unique(imputed_medical$Diabetes)

#Re-express Hyperlipidemia
unique(imputed_medical$Hyperlipidemia)
hyp.num <- recode(imputed_medical$Hyperlipidemia, "No"=0, "Yes"=1)
imputed_medical$Hyperlipidemia <- as.numeric(hyp.num)
unique(imputed_medical$Hyperlipidemia)

#Re-express BackPain
unique(imputed_medical$BackPain)
back.num <- recode(imputed_medical$BackPain, "No"=0, "Yes"=1)
imputed_medical$BackPain <- as.numeric(back.num)
unique(imputed_medical$BackPain)

#Re-express Allergic_rhinitis
unique(imputed_medical$Allergic_rhinitis)
allrhi.num <- recode(imputed_medical$Allergic_rhinitis, "No"=0, "Yes"=1)
imputed_medical$Allergic_rhinitis <- as.numeric(allrhi.num)
unique(imputed_medical$Allergic_rhinitis)

#Re-express Reflux_esophagitis
unique(imputed_medical$Reflux_esophagitis)
reflux.num <- recode(imputed_medical$Reflux_esophagitis, "No"=0, "Yes"=1)
imputed_medical$Reflux_esophagitis <- as.numeric(reflux.num)
unique(imputed_medical$Reflux_esophagitis)

#Re-express Asthma
unique(imputed_medical$Asthma)
asthma.num <- recode(imputed_medical$Asthma, "No"=0, "Yes"=1)
imputed_medical$Asthma <- as.numeric(asthma.num)
unique(imputed_medical$Asthma)

#Re-express Complication_risk
unique(imputed_medical$Complication_risk)
comp.num <- recode(imputed_medical$Complication_risk, "Low"=1, "Medium"=2, "High"=3)
imputed_medical$Complication_risk <- as.numeric(comp.num)
unique(imputed_medical$Complication_risk)

str(imputed_medical)

#PCA
library(factoextra)
str(imputed_medical)
data.frame(colnames(imputed_medical))
imputed_medical[,c(12,16,17,20,24,25,26,27,43,44,45)]
imputed_medical.pca<-prcomp(imputed_medical[,c(12,16,17,20,24,25,26,27,43,44,45)], center=T, scale=T)
imputed_medical.pca$rotation
fviz_eig(imputed_medical.pca, choice = "eigenvalue", addlabels = T)

write.csv(imputed_medical, "D:\\MS DA\\D206\\clean_medical_data.csv")
