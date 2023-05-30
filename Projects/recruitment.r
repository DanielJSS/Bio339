# stock and recruitment for northeast Arctic cod
# data: stock assessment from 2022 by the Joint Russian-Norwegian Working Group on Arctic Fisheries (JRN-AFWG)
# usually provided by the Arctic Fisheries Working Group (AFWG)
# recruitment age is 3 years

library(openxlsx)
library(plotrix)

setwd("~/UiB/BIO339")

# the file has a complex header structure that cannot be fully imported
d<-read.xlsx("NEAcod.xlsx",sheet=1,startRow=3)
names(d)[names(d)=="X2"]<-"recr"
names(d)[names(d)=="X5"]<-"ssb"

# for the analysis, one needs to align recruitment and the biomass that produced that recruitment
# it is easiest to handle this by making a new column where recruitment is shifted backwards by 3 years
d$recr3<-c(d$recr[4:nrow(d)],rep(NA,3))
plot(recr3~ssb,d,pch=16,xlab="SSB (t)",ylab="R (1000s)")

### Beverton-Holt

# the simplest possible model, but from the plot we already know that this is not a good model
summary(mBH<-nls(recr3~a*ssb/(1+ssb/Bhalf),data=d,start=list(a=1,Bhalf=1e7)))
hist(resid(mBH))

# fitting the model in log-log scale - assuming log-normal errors
summary(mBH<-nls(log(recr3)~log(a*ssb/(1+ssb/Bhalf)),data=d,start=list(a=1,Bhalf=1e7)))

# needs better starting values - use the ones from the normal fit
summary(mBH<-nls(log(recr3)~log(a*ssb/(1+ssb/Bhalf)),data=d,start=coef(mBH)))

# error message suggests that we need to prevent the algorith from searching in the inplausible space
summary(mBH<-nls(log(recr3)~log(a*ssb/(1+ssb/Bhalf)),data=d,start=coef(mBH),lower=c(0.1,1),algorithm="port"))

# model check
hist(resid(mBH))
scatter.smooth(fitted(mBH),resid(mBH))
scatter.smooth(d$ssb[1:length(resid(mBH))],resid(mBH))
acf(resid(mBH))

# plot
ssbs<-seq(min(d$ssb),max(d$ssb),length=100)
plot(recr3~ssb,d,pch=16,xlab="SSB (t)",ylab="R (1000s)")
lines(ssbs,exp(predict(mBH,data.frame(ssb=ssbs))))
abline(a=0,b=coef(mBH)[1],col="blue") # slope at origin
abline(v=coef(mBH)[2],col="blue") # Bhalf

### Ricker
summary(mR<-nls(recr3~a*ssb*2^(-ssb/Bhalf),data=d,start=list(a=1,Bhalf=1e7)))
hist(resid(mR))

# fitting the model in log-log scale - assuming log-normal errors
summary(mR<-nls(log(recr3)~log(a*ssb*2^(-ssb/Bhalf)),data=d,start=coef(mR)))

# model check
hist(resid(mR))
scatter.smooth(fitted(mR),resid(mR))
scatter.smooth(d$ssb[1:length(resid(mR))],resid(mR))
acf(resid(mR))

# plot
plot(recr3~ssb,d,pch=16,xlab="SSB (t)",ylab="R (1000s)")
lines(ssbs,exp(predict(mR,data.frame(ssb=ssbs))))
abline(a=0,b=coef(mR)[1],col="blue") # slope at origin
abline(v=coef(mR)[2],col="blue") # Bhalf

# plot both models
plot(recr3~ssb,d,pch=16,xlab="SSB (t)",ylab="R (1000s)")
lines(ssbs,exp(predict(mR,data.frame(ssb=ssbs))))
abline(a=0,b=coef(mR)[1],col="blue") # slope at origin
abline(v=coef(mR)[2],col="blue") # Bhalf
lines(ssbs,exp(predict(mBH,data.frame(ssb=ssbs))),lty="dashed")
abline(a=0,b=coef(mBH)[1],col="blue",lty="dashed") # slope at origin
abline(v=coef(mBH)[2],col="blue",lty="dashed") # Bhalf

# AIC favours Ricker, as does the visual impression
AIC(mR,mBH)

### both models show some autocorrelation

# calculate mean temperature for the 3 years following spawning
d$Kola3<-NA
d$Kola3[1:(nrow(d)-3)]<-apply(cbind(d$Kola[1:(nrow(d)-3)],d$Kola[1+1:(nrow(d)-3)],d$Kola[2+1:(nrow(d)-3)]),1,mean)
# express as temperature anomaly
d$Kola3<- d$Kola3-mean(d$Kola3,na.rm=TRUE)

# fitting the model in log-log scale - assuming log-normal errors
summary(mRt<-nls(log(recr3)~log(a*ssb*2^(-ssb/Bhalf))*exp(t.eff*Kola3),data=d,start=c(coef(mR),t.eff=0)))

# temperature effect is not significant but is favored by AIC (with a narrow margin)
AIC(mR,mRt)

# model diagnostics do not get better
hist(resid(mRt))
scatter.smooth(fitted(mRt),resid(mRt))
scatter.smooth(d$ssb[1:length(resid(mRt))],resid(mRt))
acf(resid(mRt))

