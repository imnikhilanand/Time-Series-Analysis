library(fma)
library(expsmooth)
library(lmtest)
library(tseries)
library(xts)
library(fUnitRoots)
library(Metrics)

v1<-c(14,19,39,22,16,28,21,37,58,25,23,25,47,44,26,24,21,19,33,33,18,16,27,29,23,54,38,23)
new_ts<-ts(v1,start=1,end=29,frequency=1)

#plotting the initial data
summary(new_ts)
plot(new_ts)

#splitting the data
sr = window(new_ts,start=1, end=20)
ser = window(new_ts,start=20, end=29)
trainData <- sr
testData <- ser

#building model
arimaMod <- auto.arima(ts(trainData,frequency = 4),D=1)
arimaMod
summary(arimaMod)
arimaMod.Fr <-forecast(arimaMod,h=10)
plot(arimaMod.Fr)
lines(testData,col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

result<-forecast(arimaMod,h=10)

result

actual_res<-c(33,18,16,27,29,23,54,38,23,14)
recieved_res<-c(21,19,33,33,21,19,33,33,21,19)
rmse(actual_res, recieved_res)



testData