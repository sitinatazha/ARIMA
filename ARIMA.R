#packages
library(tseries)
library(timeSeries)
library(FitAR)
library(EnvStats)
library(forecast)
library(lmtest)
library(normwhn.test)
library(mvnormtest)

#memanggil data
data <- read.delim("clipboard")
data

#Mengubah data ke bentuk time series
data1 <- ts(data, start=c(2017,6), freqency=12)
data1

#membuat plot
plot.ts(data1)

# uji statsioneritas data dalam ragam
lambda <- BoxCox.lambda(data1)
lambda

#Transformasi BoxCox
transformasi.BoxCox <- BoxCox(data1,0.5)
lambda2 <- BoxCox.lambda(transformasi.BoxCox)
lambda2

#Transformasi Log
tranformasi.log <- log(data1)
tranformasi.log

#Uji stasioneritas data dalam rataan
adf.test(data1)

#DIFFERENCING
data2 <- diff(data1)

#uji stasioner data kembali 
adf.test(data2)

#DIFFERENCING
data3 <- diff(data2)

#uji stasioner data kembali 
adf.test(data3)

#Identifikasi model semenetara
#plot
acf(data3)
pacf(data3)

#model terbaik
fit <- arima(data1,c(1,2,2)) # angka 1 yang awal adalah PACF, 1 yang kedua adalah differencing/berapa kali menormalkan data, 2 adalah acf
fit
fit2 <- arima(data1,c(2,2,1))
fit2

#uji signifikansi parameter (t)
coeftest(fit)

#uji normalitas residual
myresid <- fit$residuals
shapiro.test(myresid)
qqnorm(myresid)

#Uji Independensi Residual (White Noise)
boxresult <- LjungBoxTest(myresid)
plot(boxresult[,3], main="Ljung-Box_Test",xlab="Lag", ylab="p-value")
Box.test(myresid, type="Ljung-Box")

#Prediksi
Fitted <- ts(data1+myresid)
Fitted2 <- ts(Fitted, start=c(2017,6), freq=12)

#peramalan
peramalan <- predict(fit,n.ahead=3)
peramalan <- peramalan$pred
peramalan1 <- ts(peramalan, start=c(2021, 12), freq=12)

#akurasi peramalan
mape <- mean(abs(myresid)/data1, na.rm = TRUE) * 100
mape

#plot hasil
plot(data1, main="Netflix", lwd=2, col="black", xlim=c(2017, 2023), ylim=c(100,500),
     type="o", pch=19)
limitDate<-end(data1)[1]+(end(data1)[2]-1)/frequency(data1)
abline(v=limitDate, lty=4)
lines(Fitted2, lwd=2, col="darkturquoise", type="o", pch=19)
lines(peramalan1, col="Red", type="o", pch=19)
legend("topleft", legend=c("Data Aktual", "Prediksi", "Peramalan"),
       col=c("black", "darkturquoise", "red"),
       lty=1, pch=c(19,19,20), cex=0.8, inset=0.05)

