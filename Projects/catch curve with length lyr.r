# von Bertalanffy growth curve for pollock in Masfjorden
# this includes both age- and length-based versions

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

# age range to be used - here, the maximum age is not important, but the first age is very important
ages<-1:6

# Poisson model - the default link function is log, so often this is not explicitly specified
summary(mP<-glm(x~age,family=poisson(link="log"),data=d1[d1$age %in% ages,]))
(Z<- -summary(mP)$coef[2,1])
c(Z, Z-1.96*summary(mP)$coef[2,2], Z+1.96*summary(mP)$coef[2,2])

####

# length-based version
# we need a growth model - e.g., von Bertalanffy
# here we have good age data and can estimate the model directly
# in cases where length-based catch curve is needed, the growth model would need to be obtained from the literature or from length-frequency analysis

# simple von Bertalanffy fit
summary(vB<-nls(length~Linf*(1-exp(-k*(age-t0))),data=d,start=list(Linf=60,k=0.1,t0=0)))

# function for von Bertalanffy and its inverse function
vonB<-function(age,Linf,k,t0){ Linf*(1-exp(-k*(age-t0))) }
anti.vonB<-function(length,Linf,k,t0){ -1/k*log(1-length/Linf)+t0 }

# illustration only - von Bertalanffy and its inverse function
ages<-seq(0,10,length=100)
lengths<-1:70
op<-par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(4,3,0.5,0.5),lab=c(5,10,7))
plot(ages,vonB(ages,Linf=coef(vB)[1],k=coef(vB)[2],t0=coef(vB)[3]),type="l",xlab="Age (yr)",ylab="Length (cm)")
plot(lengths,anti.vonB(lengths,Linf=coef(vB)[1],k=coef(vB)[2],t0=coef(vB)[3]),type="l",ylab="Age (yr)",xlab="Length (cm)")
par(op)

# illustration only - length and age distributions
lengths<-10:65
op<-par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(4,3,0.5,0.5),lab=c(5,10,7))
hist(d$length,breaks=lengths,xlab="Length (cm)",main="")
hist(anti.vonB(d$length,Linf=coef(vB)[1],k=coef(vB)[2],t0=coef(vB)[3]),breaks=-1:11,xlab="Age (yr)",main="")
par(op)

# calculate the predicted age of each individual as an illustration
# "absolute age"
d$age.est<-anti.vonB(d$length,Linf=coef(vB)[1],k=coef(vB)[2],t0=coef(vB)[3])
with(d,sizeplot(age.est,length,pch=21,scale=0.3,bg=alpha("blue",0.1),xlab="Age (yr)",ylab="Length (cm)"))

# relative age - t0 is not important here
d$age.rel<-anti.vonB(d$length,Linf=coef(vB)[1],k=coef(vB)[2],t0=0)

# illustrate real age distribution and the one derived from length
op<-par(mfrow=c(2,1),mgp=c(2,1,0),mar=c(4,3,0.5,0.5),lab=c(5,10,7))
hist(d$age+0.001,breaks=-1:11,xlab="Age (yr)",main="")
hist(d$age.rel,breaks=-1:11,xlab="Age (yr)",main="")
par(op)

# now the length-based catch curve itself

# step 1 - create a table of length frequencies
da<-with(d,aggregate(length,list(length=length),length))

# step 2 - calculate relative age for each length interval
# these fish have been measured down to the nearest full centimeter - lengths are thus length intervals of width 1 cm
dl<-1 # width of length interval - in case there are gaps in the data

# mean relative age of fish in a length interval can be approximated as the relative age in the middle of each interval
da$age.rel<-anti.vonB(da$length+dl/2,Linf=coef(vB)[1],k=coef(vB)[2],t0=0)

# step 3 - calculate time it takes to growth to the next length interval
da$dt<-anti.vonB(da$length+dl,Linf=coef(vB)[1],k=coef(vB)[2],t0=0)-anti.vonB(da$length,Linf=coef(vB)[1],k=coef(vB)[2],t0=0)

# step 4 - determine with "age" range is suitable for fitting the catch curve
# abundance versus reconstructed relative age
with(da,plot(x~age.rel,pch=16,log="y",xlab="Relative age (yr)",ylab="Count per length interval"))
# looks like the relationship is appximately linear after relative age 2 yrs
abline(v=2,lty="dotted")

# abundance corrected with length interval duration versus reconstructed relative age
with(da,plot(x/dt~age.rel,pch=16,log="y",xlab="Relative age (yr)",ylab="Count at annualized scale"))
# looks like the relationship is appximately linear after relative age 2 yrs
abline(v=2,lty="dotted")

# step 5 - fit the catch curve
# Poisson model - the default link function is log, so often this is not explicitly specified
summary(mP<-glm(x/dt~age.rel,family=poisson(link="log"),data=da[da$age>2,]))
(Z<- -summary(mP)$coef[2,1])

# notice that these confidence intevals do not reflect uncertainty in the growth model !!!
c(Z, Z-1.96*summary(mP)$coef[2,2], Z+1.96*summary(mP)$coef[2,2])

# for comparison, linear model of log-transformed values 
summary(mL<-lm(log(x/dt)~age.rel,data=da[da$age>2,]))
(Z<- -summary(mL)$coef[2,1])
c(Z, Z-1.96*summary(mL)$coef[2,2], Z+1.96*summary(mL)$coef[2,2])

#####

# just another illustration of the underlying distributions in the actual data
op<-par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(4,3,0.5,0.5),lab=c(5,10,7))
plot(age<-table(d$age),xlab="Age (yr)",ylab="Count")
plot(len<-table(d$length),xlab="Length (cm)",ylab="Count")
plot(log10(age),xlab="Age (yr)",ylab="log10(Count)")
plot(log10(len),xlab="Length (cm)",ylab="log10(Count)")
par(op)



