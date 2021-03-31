
rm(list = ls())
#Importing Gold value data
data<- read.csv("C:/Users/shiva/Downloads/gold_price_data.csv",header=T)
View(data)
str(data)
library(lubridate)
#converting to date data type
data$Date<-ymd(data$Date)
str(data)
class(data)
#plot
qplot(data$Date,data$Value)
qplot(data$Date,log(data$Value))

#using Prophet package 
library(prophet)
ds<- data$Date
y<- log(data$Value)
df<- data.frame(ds,y)
View(df)


Model <- prophet(df)
Model
attributes(Model)


Future<- make_future_dataframe(Model,365)
Future

Forecast <- predict(Model,Future)
plot(Model,Forecast)
qplot(Model,Forecast)
prophet_plot_components(Model,Forecast)
dyplot.prophet(Model,Forecast)

