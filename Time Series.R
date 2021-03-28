
library(forecast)
library(tseries)
#Creating a Times Series object in R
sales<- c(18,33,41,7,34,35,24,25,24,21,25,20,22,31,40,29,
          25,21,25,21,22,54,31,25)
sales
tsales<- ts(sales,start = c(2003,1),frequency = 12)
tsales

#Plot tsales
plot(tsales)

start(tsales)
end(tsales)
frequency(tsales)

#Subsetting tsales
tsales.subset<- window(tsales,start=c(2003,5),end=c(2004,6))
tsales.subset

#------------------------------------------------------------#
#AIR PASSENGERS DATASET -INBUILT IN R

#Seasonal decompositon using stl()
plot(AirPassengers)

#Log Transformation
lAirPassengers<- log(AirPassengers)
plot(lAirPassengers,main="log(AirPassengers)")
?stl()
fit<-stl(x = lAirPassengers,s.window = "period")
fit_Decomp<- decompose(lAirPassengers)
plot(fit)
fit$time.series

exp(fit$time.series)
library(forecast)
monthplot(AirPassengers,xlab="",ylab="")
seasonplot(AirPassengers)
?AirPassengers




#Exponential Smoothing with levels,slope & seasonal Components
Fit<- HoltWinters(log(AirPassengers))
Fit
head(Fit)
Pred<- forecast(Fit,5)
Pred
plot(Pred,main="Forecast for AirTravel",xlab="log(AirPassengers)")
Pred$mean<- exp(Pred$mean)
Pred$lower<- exp(Pred$lower)
Pred$upper<- exp(Pred$upper)

P<- cbind(Pred$mean,Pred$lower,Pred$upper)
dimnames(P)[[2]]<- c("Mean","Lo 80","Lo 95","Hi 80","Hi 95")
P



#---------------------------------
#ARIMA
#NILE DATA
plot(Nile)
ndiffs(Nile)
?ndiffs
dNile<-diff(Nile)
dNile
plot(dNile)
adf.test(dNile)


#Fit a ARIMA model
F<- arima(Nile,order = c(0,1,1))
F
accuracy(F)




#--------------------------------------
#Sunspots data
sunspots
Fit_sun<- auto.arima(sunspots)
Fit_sun
f<-forecast(Fit_sun,3)
f
plot(f)
accuracy(Fit_sun)

