install.packages("forecast")
library(forecast)
library(corrplot)
bit<-read.csv("C:/Users/CYM/Documents/Bitcoin.csv",header=TRUE)
attach(bit)
new<-data.frame(x=Bitcoin.Price)
newp<-ts(new,start=1)
plot(newp,main="price trend of 3Y")
t1<-ts(newp[400:783])
plot(t1,xlim=c(0,400),main="price trend of high frequency")
plot(diff(t1),xlim=c(0,400),main="one unit diffrence")
acf(diff(t1))
pacf(diff(t1))
plot(diff(log(t1)),xlim=c(0,400),main="one unit diffrence of log scale")
dl<-diff(log(t1))
acf(dl)
pacf(dl)

m1<-arima(dl,order=c(3,0,1))
AIC(m1)
# ARIMA(3,1,1),log scale.
m2<-arima(dl,order=c(0,0,1))
AIC(m2)
# ARIMA(0,1,1), log scale.
# so far, m2 is the best, log scale with ARIMA(0,1,1).
m3<-arima(dl,order=c(4,0,1))
AIC(m3)
m4<-arima(dl,order=c(5,0,1))
AIC(m4)
# so far, m2 is still the best.
auto.arima(log(t1))
#auto selection says ARIMA(0,1,0), see m5
tsdiag(m3)
tsdiag(m2)
m5<-arima(dl,order=c(0,0,0))
AIC(m5)
tsdiag(m5)
# even the AIC score is the smallest for ARIMA(0,1,0)
# But the diagnostic plot of m3 ARIMA(4,1,1) shows biggest p values.
# AIC=-1127.21

m6<-arima(newp,order=c(4,1,1))
AIC(m6)
tsdiag(m6)
m7<-arima(log(newp),order=c(4,1,1))
AIC(m7)
tsdiag(m7)
# so log transformation is necessary.make result consistent over time.

# is there any seasonal trend?
# assume each 5 days as a seasonal period
newseason<-ts(new,start=1,frequency = 5)
plot(log(newseason))
logseason<-log(newseason)
acf(logseason)
pacf(logseason)
plot(diff(log(newseason)))
acf(diff(logseason))
pacf(diff(logseason))
diff1<-diff(log(newseason),lag=5)
plot(diff1)
plot(diff(diff1))
acf(diff1)
pacf(diff1)
acf(diff(diff1))
pacf(diff(diff1))

m8<-arima(diff1,order=c(0,1,4),seasonal=list(order=c(0,1,4)))
AIC(m8)
tsdiag(m8)

m9<-arima(diff1,order=c(0,0,4),seasonal=list(order=c(0,0,4)))
AIC(m9)
tsdiag(m9)

m10<-arima(diff1,order=c(4,1,4),seasonal=list(order=c(4,1,4)))
AIC(m10)
tsdiag(m10)

# so far, m8 is the best model with the best diagnostic plot. 
# take difference two times along with MA(4).
# AIC =-2526.269
qqnorm(resid(m8))
qqline(resid(m8))
qqnorm(resid(m3))
qqline(resid(m3))
qqnorm(resid(m7))
qqline(resid(m7))
# m7 is the full range and m3 is the half range with same model.
qqnorm(resid(m2))
qqline(resid(m2))
qqnorm(resid(m5))
qqline(resid(m5))

# the polts are to check normality errors
# m7 may be a better choice
lognew<-ts(log(new),start=1)
m11<-arima(lognew, order=c(4,1,1))
pred1<-predict(m11,n.ahead = 5)
pred1
plot(lognew,xlim=c(750,800),ylim=c(8,10),type="n")
lines(lognew)
lines(pred1$pred + 2*pred1$se, lty=2)
lines(pred1$pred - 2*pred1$se, lty=2)
points(pred1$pred, pch=3)
title("Forecasts in log scale")
exp(9.006169)
exp(9.009011)
exp(9.008921)
exp(9.009278)
exp(9.009449)
c(exp(9.006169-1.96*0.04442157),exp(9.006169+1.96*0.04442157))
c(exp(9.009011-1.96*0.06318005),exp(9.009011+1.96*0.06318005))

#first day error=39, second day error=141
#real value of first day is 8114, second day is 8035
# day's price is calculated by (max-min)/2 of that day.
# but the forecast values all fall into 95% CI
# since the data is non-stationary, closer time will have a short 95% CI and a precise forecast
# so the model is good for short term forecast somehow

logns<-ts(log(new),start=1,frequency = 5)
m12<-arima(logns,order=c(0,2,4),seasonal=list(order=c(0,2,4)))
pred2<-predict(m12,n.ahead = 5)
pred2
plot(logns,xlim=c(150,160),ylim=c(8,10),type="n")
lines(logns,lty=1)
lines(pred2$pred + 2*pred2$se, lty=2)
lines(pred2$pred - 2*pred2$se, lty=2)
points(pred2$pred, pch=3)
title("Forecasts in log scale and seasonal trend")
exp(8.998614) 
exp(8.991663) 
exp(9.014075)
exp(9.027453)
exp(9.020439)

# m12:forecast for first day is 8091.861, second day is 8035.809
#first day error=23, second day error=0!
#amazing finding! even seasonal model does not has a very good qqnorm plot compared with non-seaonal model.
#But the forecast is very accurate.
#and we could see the forecast plot,the forecast points varies in a wider range than non-seasonal model.
# m8 is equal to m12

m13<-arima(diff1,order=c(4,0,4),seasonal=list(order=c(4,0,4)))
AIC(m13)
tsdiag(m13)

m14<-arima(diff1,order=c(3,0,4),seasonal=list(order=c(3,0,4)))
AIC(m14)
tsdiag(m14)

# m13 is better than m14 from dignostic plot

m15<-arima(diff1,order=c(5,0,4),seasonal=list(order=c(5,0,4)))
AIC(m15)
tsdiag(m15)

m16<-arima(diff1,order=c(1,1,4),seasonal=list(order=c(1,1,4)))
AIC(m16)
tsdiag(m16)
# compare with AIC and plot, m16 is almost same with m8.

m17<-arima(diff1,order=c(2,1,4),seasonal=list(order=c(2,1,4)))
AIC(m17)
tsdiag(m17)

m18<-arima(diff1,order=c(3,1,4),seasonal=list(order=c(3,1,4)))
AIC(m18)
tsdiag(m18)
# m18's dignostic plot is perfect.

# so right now, I will try m18(best plot) and m13(best AIC score).
# recall: m13<-arima(diff1,order=c(4,0,4),seasonal=list(order=c(4,0,4)))
# define m19 is same with m13
m19<-arima(logns,order=c(4,1,4),seasonal=list(order=c(4,1,4)))
pred3<-predict(m19,n.ahead = 5)
pred3
plot(logns,xlim=c(150,160),ylim=c(8,10),type="n")
lines(logns,lty=1)
lines(pred3$pred + 2*pred3$se, lty=2)
lines(pred3$pred - 2*pred3$se, lty=2)
points(pred3$pred, pch=3)
title("Forecasts of m13")

exp(9.010521)
exp(9.021931)
exp(9.032707)
exp(9.048520)
exp(9.040860)

#real value of first day is 8114, second day is 8035
# forecast value of first day is 8188, second day is 8282.

# recall: m18<-m18<-arima(diff1,order=c(3,1,4),seasonal=list(order=c(3,1,4)))
# define m20 is same with m18
m20<-arima(logns,order=c(3,2,4),seasonal=list(order=c(3,2,4)))
pred4<-predict(m20,n.ahead = 5)
pred4
plot(logns,xlim=c(150,160),ylim=c(8,10),type="n")
lines(logns,lty=1)
lines(pred4$pred + 2*pred4$se, lty=2)
lines(pred4$pred - 2*pred4$se, lty=2)
points(pred4$pred, pch=3)
title("Forecasts of m18")

exp(9.003919)
exp(8.997085)
exp(9.013933)
exp(9.038114)
exp(9.027668)

#real value of first day is 8114, second day is 8035
# forecast value of first day is 8135, second day is 8079.
# error is 21, and 44.

#so, for final model, I will probably trust model ARIMA(0,2,4) or ARIMA(3,2,4).

BIT<-cor(bit[,-1])
corrplot(BIT,method="circle")
# the correlation are not monotone.
par(mfrow=c(3,2))
plot(bit$Bitcoin.Price)
plot(bit$SP.500)
plot(bit$Fixed.Income)
plot(bit$Commodities)
plot(bit$Gold)
plot(bit$Real.Estate)

# from plots, fixed income and gold may have a silimar pattern with bitcoin price.
# so they might have a positive realtion with bitcoin price.
# and the rest of variables have a negative relation with bitcoin price.
fit1<-lm(Bitcoin.Price~SP.500+Fixed.Income+Commodities+Gold+Real.Estate,data=bit)
plot(fit1)
summary(fit1)

# check the summary results.
#SP.500, fixed income, commodities have a positive coefficient.
#Gold and real estate have a negative coefficient.
par(mfrow=c(1,1))
plot(bit$Bitcoin.Price)
points(fit1$fitted.values,col="red")

plot(bit$Bitcoin.Price)
points(fit1$fitted.values,col="red")
