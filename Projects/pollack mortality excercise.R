pollack.df<-read.xlsx("lyr Masfjorden.xlsx",sheet=1)
show(pollack)
#pollack.df <- pollack.df$sex %>% replace_na("unknown")

pollack.df_test <- pollack.df %>% drop_na(sex)#I chose to remvoe the one without a sex 

# preliminary plotting and look
sizeplot(pollack.df_test$age,pollack.df_test$length,scale=.3,pch=16)
with(pollack.df_test,tapply(length,list(sex,age),mean,na.rm=T))
plot(sort(unique(pollack.df_test$age)),tapply(pollack.df_test$length,pollack.df_test$age,mean,na.rm=T),type="o")
plot(sort(unique(pollack.df_test$age)),tapply(pollack.df_test$length,pollack.df_test$age,mean,na.rm=T),type="o",log="xy")


# simple fit
summary(vB<-nls(length~Linf*(1-exp(-k*(age-t0))),data=pollack.df_test,start=list(Linf=60,k=0.2,t0=-1)))

####1.) the V.B model fits the data  





### sex differences

summary(vB.f<-nls(length~Linf*(1-exp(-k*(age-t0))),data=pollack.df_test[pollack.df_test$sex=="1",],start=list(Linf=60,k=0.2,t0=-1)))
summary(vB.m<-nls(length~Linf*(1-exp(-k*(age-t0))),data=pollack.df_test[pollack.df_test$sex=="2",],start=list(Linf=60,k=0.2,t0=-1)))


### is the sex differrence significant?

# you can compare the confidence intervals
summary(vB.f)$coef[,"Estimate"]+1.96*summary(vB.f)$coef[,"Std. Error"]
summary(vB.f)$coef[,"Estimate"]-1.96*summary(vB.f)$coef[,"Std. Error"]

summary(vB.m)$coef[,"Estimate"]+1.96*summary(vB.m)$coef[,"Std. Error"]
summary(vB.m)$coef[,"Estimate"]-1.96*summary(vB.m)$coef[,"Std. Error"]

####2.) it does not apper to be any significat differnces in growth between the sexes 


# you can also test significance with model that includes both sexes
summary(vB.fm0<-nls(length~Linf*(1-exp(-k*(age-t0))),data=pollack.df_test,start=list(Linf=60,k=0.2,t0=-1)))	# null model
summary(vB.fm1<-nls(length~Linf*(1-exp(-k*(1+k.m*(sex=="2"))*(age-t0))),data=pollack.df_test,start=list(Linf=60,k=0.2,t0=-1,k.m=0)))	# sex effect in k
summary(vB.fm2<-nls(length~Linf*(1+Linf.m*(sex=="2"))*(1-exp(-k*(age-t0))),data=pollack.df_test,start=list(Linf=60,k=0.2,t0=-1,Linf.m=0)))	# sex effect in Linf
summary(vB.fm3<-nls(length~Linf*(1+Linf.m*(sex=="2"))*(1-exp(-k*(1+k.m*(sex=="2"))*(age-t0))),data=pollack.df_test,start=list(Linf=60,k=0.2,t0=-1,Linf.m=0,k.m=0)))	# sex effect in both
AIC(vB.fm0,vB.fm1,vB.fm2,vB.fm3)
#by the looks of it, the AIC is the lowest in model 0



#some diagnostics
sizeplot(pollack.df_test$length,resid(vB.fm0),scale=0.5)	# no clear indication of heteroschedasticity
scatter.smooth(fitted(vB.fm0),resid(vB.fm0))
hist(resid(vB.fm0))		#looks good 


# fitting on log-scale - implying log-normal errors - males only
summary(vB.L<-nls(log(length)~log(Linf*(1-exp(-k*(age-t0)))),data=pollack.df_test[pollack.df$sex=="2",],start=list(Linf=60,k=0.2,t0=-1))) # our previous starting values no longer work
summary(vB.L<-nls(log(length)~log(Linf*(1-exp(-k*(age-t0)))),data=pollack.df_test[pollack.df$sex=="2",],start=coef(vB.m) ))		# by now we can provide better starting values

sizeplot(pollack.df_test$length,resid(vB.L),scale=0.5)	# looks OK
scatter.smooth(fitted(vB.L),resid(vB.L))	# looks OK
hist(resid(vB.L))							# not much change but osme alterations 

with(pollack.df_test[pollack.df_test$sex=="2",],sizeplot(age,length,scale=.3,pch=21,bg="blue",xlab="Age (d)",ylab="Length (mm)"))
ages<-seq(min(pollack.df_test$age),max(pollack.df_test$age),length=70)
lines(ages,predict(vB.m,data.frame(age=ages)),lwd=2)
lines(ages,exp(predict(vB.L,data.frame(age=ages))),lty="dotted",lwd=2)
legend("topleft",c("normal","log-normal"),lty=c("solid","dotted"),bty="n")


####3.) The fitted curve looks adequate, However the log scale might seem to fit just a bit better 


#### 4.) 
summary(pollack.df_test)
pollack.df_test$cohort<-pollack.df_test$year-pollack.df_test$age

# age-length plot - allows checking that age readings and length measurements are consistent
with(pollack.df_test,sizeplot(age,length,pch=21,scale=0.3,bg=alpha("red",0.1),xlab="Age (yr)",ylab="Length (cm)"))

# some variation in annual numbers
truehist(pollack.df_test$year,prob=F)

# large variation in cohort numbers - the extremes are incompletely represented
truehist(pollack.df_test$cohort,prob=F)

# first look at counts per age, ignoring cohorts/years
dat<-with(pollack.df_test,aggregate(age,list(age=age),length))
with(dat,plot(age,x,log="y",type="o",pch=21,bg="blue",xlab="Age (yr)",ylab="Number of individuals"))

# plots per year
op<-par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(4,3,2,0.5),lab=c(5,10,7))
for(yr in unique(pollack.df_test$year)){
  dat<-with(pollack.df_test[pollack.df_test$year==yr,],aggregate(age,list(age=age),length))
  plot(dat$age,dat$x,log="y",type="o",pch=21,bg="blue",xlim=c(0,8),xlab="Age (yr)",ylab="Number of individuals",main=yr)
  with(dat[dat$x==max(dat$x),],points(age,x,pch=21,cex=1.5,bg="yellow"))
}
par(op)

# plots per cohort
op<-par(mfrow=c(2,2),mgp=c(2,1,0),mar=c(4,3,2,0.5),lab=c(5,10,7))
for(yr in 1985:1988){
  dat<-with(pollack.df_test[pollack.df_test$cohort==yr,],aggregate(age,list(age=age),length))
  plot(dat$age,dat$x,log="y",type="o",pch=21,bg="blue",xlim=c(0,8),xlab="Age (yr)",ylab="Number of individuals",main=yr)
  with(dat[dat$x==max(dat$x),],points(age,x,pch=21,cex=1.5,bg="yellow"))
}
par(op)


### statistical analysis

# we do not attempt to estimate cohort-specific catch curves - just use all data
d1<-with(pollack.df_test,aggregate(age,list(age=age),length))

ages<-1:5

# Poisson model - the default link function is log, so often this is not explicitly specified
summary(mP<-glm(x~age,family=poisson(link="log"),data=d1[d1$age %in% ages,]))
(Z<- -summary(mP)$coef[2,1])
c(Z, Z-1.96*summary(mP)$coef[2,2], Z+1.96*summary(mP)$coef[2,2])

# linear regression on log-transformed values
summary(mL<-lm(log(x)~age,data=d1[d1$age %in% ages,]))
(Z<- -summary(mL)$coef[2,1])
c(Z, Z-1.96*summary(mL)$coef[2,2], Z+1.96*summary(mL)$coef[2,2])




