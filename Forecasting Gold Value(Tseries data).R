getwd()
#Setting Workind Directory
setwd("C:/Users/shiva/Desktop/R working directory/MODULE-4")

#Import gold Value data 
data<- read.csv("C:/Users/shiva/Downloads/gold_price_data.csv",header = T)
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
str(data)
head(data)

data$Date<- as.Date(data$Date)
str(data)

#Plotting Date vs Value
ggplot(data,aes(Date,Value))+geom_line()+geom_smooth(method = "lm")+labs(x="Date",y="Value")

#Convert data to Time series data
Tsdata<- ts(data$Value,start = c(1970,1),end = c(2020,3),frequency = 4)




#HoltWinters
Model<- HoltWinters(Tsdata)
attributes(Model)
library(forecast)

pred<- forecast(Model,12)
pred

autoplot(pred)


