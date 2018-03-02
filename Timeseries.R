library(forecast)
library(lubridate)
data <- read.csv(file="C:\\Users\\virin\\ts_data.csv", stringsAsFactors = FALSE)

myts <- ts(data$counts, frequency = 7)
print(myts)

print(class(myts))

#This tells you that the data series is in a time series format
start(myts)
end(myts)
frequency(myts)

#summary of data
summary(myts)

#The number of pageviews distributed across the spectrum
plot(myts)
abline(reg=lm(myts~time(myts)))

#Trends observed
#1. The count of pageviews and variance is decreasing

#This will aggregate the cycles and display a week on week trend
plot(aggregate(myts,FUN=mean))

#Box plot across days will give us a sense on seasonal effect
boxplot(myts~cycle(myts))

#plotting 
acf(diff(log(myts)))
pacf(diff(log(myts)))

#To find the (p, d, q) values
library(forecast)
auto.arima(myts)

fit <- arima(log(myts), c(2, 0, 0),seasonal = list(order = c(0, 1, 1), period = 7))
pred <- predict(fit,n.ahead=1*7)
pred1 <- 2.718^pred$pred
print(round(pred1))
ts.plot(myts,2.718^pred$pred, log = "y", lty = c(1,3))

