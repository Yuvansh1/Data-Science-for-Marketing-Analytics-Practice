#Predicting passsenger traffic using Prophet

"The dataset is a univariate time series that contains hourly passenger traffic 
for a new public transport service. We are trying to forecast the traffic for next 
7 months given historical traffic data of last 25 months." 



library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
require(xlsx)

setwd("C:/Users/yuvbhard/Desktop/Data-Science-for-Marketing-Analytics-Practice/Facebook Prophet Time Series Model (R & Python)")
# read data
train = fread("Train_SU63ISt.csv")
test = fread("Test_0qrQsBZ.csv")

# Extract date from the Datetime variable
train$Date = as.POSIXct(strptime(train$Datetime, "%d-%m-%Y"))
test$Date = as.POSIXct(strptime(test$Datetime, "%d-%m-%Y"))


# Convert 'Datetime' variable from character to date-time format
train$Datetime = as.POSIXct(strptime(train$Datetime, "%d-%m-%Y %H:%M"))
test$Datetime = as.POSIXct(strptime(test$Datetime, "%d-%m-%Y %H:%M"))

# Aggregate train data day-wise
aggr_train = train[,list(Count = sum(Count)), by = Date]

#Remove row with NA value from df

aggr_train = aggr_train[!is.na(aggr_train$Date), ]


# Visualize the data
ggplot(aggr_train) + geom_line(aes(Date, Count))


# Change column names from Date, Count to ds,y
names(aggr_train) = c("ds", "y")


# Model building
m = prophet(aggr_train, daily.seasonality=TRUE)
future = make_future_dataframe(m, periods = 213)
forecast = predict(m, future)

# Visualize forecast
plot(m, forecast)

# proportion of mean hourly 'Count' based on train data
mean_hourly_count = train %>%
  group_by(hour = hour(train$Datetime)) %>%
  summarise(mean_count = mean(Count))


s = sum(mean_hourly_count$mean_count)
mean_hourly_count$count_proportion = mean_hourly_count$mean_count/s

# variable to store hourly Count
test_count = NULL

for(i in 763:nrow(forecast)){
  test_count = append(test_count, mean_hourly_count$count_proportion * forecast$yhat[i])
}

new_t = lapply(strsplit(as.character(test_count),split=','),trimws)
test$Count = test_count['Value']

