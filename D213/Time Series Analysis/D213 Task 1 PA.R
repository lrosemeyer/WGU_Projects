library(astsa)
library(forecast)
library(bayesforecast)
library(ggplot2)
library(fUnitRoots)

med<-read.csv("medical_time_series.csv", row.names = "Day")

 # Clean data
sum(duplicated(med))
sum(is.na(med))
print(med)
dim(med)

 # Create main time series
med <- ts(med, start=0, frequency = 365)
plot(med)

 # Create train and test data
med_train <- head(med, 585)
plot(med_train)
print(med_train)

med_test <- tail(med,146)
plot(med_test)
print(med_test)

write.csv(med_train, "med_train.csv")
write.csv(med_test, "med_test.csv")

 # Test for stationarity
adfTest(med)

d_med<-diff(med)
plot(d_med)

adfTest(d_med)

 # Decompose the data, run acf/pacf, spectral density
decomp <- stl(med[,1], s.window='periodic')
autoplot(decomp)

acf2(d_med, max.lag=365)

mvspec(d_med)

 # Run auto arima
arima_med <- auto.arima(med)
arima_med
arima_med_train <- auto.arima(med_train)
arima_med_train

sarima(med,1,1,0) #1.207342

sarima(med,1,1,0,0,0,2,12)

sarima(med_train,1,1,0)
sarima(med_train,1,1,0,0,0,2,12)

pred <- sarima.for(med,n.ahead = 365,1,1,0,0,0,2,12, plot.all = T)
pred

med_pred <- sarima.for(med_train, n.ahead=146,1,1,0,0,0,2,12, plot.all = T) + autolayer(med_test)

autoplot(pred$pred)
