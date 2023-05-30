# von Bertalanffy growth curve for pollock in Masfjorden

library(openxlsx)
library(plotrix)
library(scales)
library(MASS)

setwd("./UiB/BIO339")

d<-read.xlsx("lyr Masfjorden.xlsx",sheet=1)

summary(d)
d$cohort<-d$year-d$age

# age-length plot - allows checking that age readings and length measurements are consistent
with(d,sizeplot(age,length,pch=21,scale=0.3,bg=alpha("blue",0.1),xlab="Age (yr)",ylab="Length (cm)"))

# some variation in annual numbers
truehist(d$year,prob=F)

# large variation in cohort numbers - the extremes are incompletely represented
truehist(d$cohort,prob=F)

# first look at counts per age, ignoring cohorts/years
dat<-with(d,aggregate(age,list(age=age),length))
with(dat,plot(age,x,log="y",type="o",pch=21,bg="blue",xlab="Age (yr)",ylab="Number of individuals"))

# plots per year
op<-par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(4,3,2,0.5),lab=c(5,10,7))
for(yr in unique(d$year)){
	dat<-with(d[d$year==yr,],aggregate(age,list(age=age),length))
	plot(dat$age,dat$x,log="y",type="o",pch=21,bg="blue",xlim=c(0,8),xlab="Age (yr)",ylab="Number of individuals",main=yr)
	with(dat[dat$x==max(dat$x),],points(age,x,pch=21,cex=1.5,bg="yellow"))
}
par(op)

# plots per cohort
op<-par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(4,3,2,0.5),lab=c(5,10,7))
for(yr in 1985:1988){
	dat<-with(d[d$cohort==yr,],aggregate(age,list(age=age),length))
	plot(dat$age,dat$x,log="y",type="o",pch=21,bg="blue",xlim=c(0,8),xlab="Age (yr)",ylab="Number of individuals",main=yr)
	with(dat[dat$x==max(dat$x),],points(age,x,pch=21,cex=1.5,bg="yellow"))
}
par(op)


### statistical analysis

# we do not attempt to estimate cohort-specific catch curves - just use all data
d1<-with(d,aggregate(age,list(age=age),length))

ages<-1:5

# Poisson model - the default link function is log, so often this is not explicitly specified
summary(mP<-glm(x~age,family=poisson(link="log"),data=d1[d1$age %in% ages,]))
(Z<- -summary(mP)$coef[2,1])
c(Z, Z-1.96*summary(mP)$coef[2,2], Z+1.96*summary(mP)$coef[2,2])

# linear regression on log-transformed values
summary(mL<-lm(log(x)~age,data=d1[d1$age %in% ages,]))
(Z<- -summary(mL)$coef[2,1])
c(Z, Z-1.96*summary(mL)$coef[2,2], Z+1.96*summary(mL)$coef[2,2])



