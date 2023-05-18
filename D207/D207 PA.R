churn <- read.csv("churn_clean.csv")
head(churn)
table(churn$Churn)
table(churn$Contacts)

library(ggplot2)
ggplot(churn, aes(x=Contacts, fill=Churn)) + geom_histogram()

library(tidyverse)
test_data <- churn %>%
  select(Churn, Contacts)

dim(test_data)
head(test_data)
table(test_data)
t.test(Contacts ~ Churn, data=test_data, alternative="less")

boxplot(churn$Outage_sec_perweek, horizontal=T, xlab="Seconds", main="Outage Time per Week (Seconds)")
ggplot(churn, aes(Outage_sec_perweek)) + geom_histogram() + ggtitle("Outage Time per Week (Seconds)") + xlab("Seconds")
ggplot(churn, aes(Tenure)) + geom_histogram() + ggtitle("Tenure Length") + xlab("Months")
boxplot(churn$Tenure, horizontal = T, xlab="Months", main="Tenure")
ggplot(churn, aes(Contract)) + geom_bar() + geom_text(stat = "count", aes(label = after_stat(count)), vjust = -.4) + ggtitle("Contract Type")
ggplot(churn, aes(PaymentMethod)) + geom_bar() + geom_text(stat="count", aes(label=after_stat(count)), vjust = -.4) + ggtitle("Payment Method") + xlab("Payment Method")
ggplot(churn, aes(Tenure, Bandwidth_GB_Year)) + geom_point(shape=".") + ggtitle("Bandwidth Per Year vs. Tenure") + xlab("Tenure (in Months)") + ylab("Bandwidth Per Year (in GBs)")
ggplot(churn) + geom_bar(aes(Techie, fill=Tablet)) + scale_fill_manual(values = c("Yes"="Blue", "No"="Red")) + ggtitle("Techie vs. Tablet")
tablet <- table(churn$Techie, churn$Tablet)
proptab <- tablet/margin.table(tablet)
proptab
barplot(proptab, col = c("Blue", "Red"), legend.text = c("No", "Yes"), main="Tablet vs. Techie", xlab = "Tablet")
library(expss)
cross_rpct(churn, Techie, Tablet)
