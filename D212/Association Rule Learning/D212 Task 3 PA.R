library(tidyverse)
library(dplyr)
library(datasets)

teleco <- read.csv("D:\\MS DA\\D212\\Task 3\\teleco_market_basket.csv")
dim(teleco)

cleaned_teleco <- teleco[!apply(teleco == "", 1, all), ]
dim(cleaned_teleco)

cleaned_teleco$ID <- factor(seq.int(nrow(cleaned_teleco)))

cleaned_teleco <- as.data.frame(unclass(cleaned_teleco), stringsAsFactors = T)

library(tidyr)
pre_trans <- pivot_longer(cleaned_teleco, cols = 1:20, names_to = "ItemNo", values_to = "Product")
pre_trans <- pre_trans[, c(1,3)]
pre_trans <- pre_trans[!(pre_trans$Product == ""),]
print(pre_trans, n=100)
list_data <- as.data.frame(pre_trans)
list_data <- split(list_data$Product, list_data$ID)
str(list_data)
head(list_data)

library(arules)
basket <- as(list_data, "transactions")
basket <- as(basket, "matrix")
str(basket)
dim(basket)
basket

arules <- apriori(basket, control=list(verbose=F), parameter=list(supp=0.008, conf=0.4, minlen=2))

redundant_r <- is.redundant(arules)
refined_arules <- arules[!redundant_r]
inspect(head(sort(refined_arules, by="lift", decreasing=T), 20))

summary(refined_arules)
