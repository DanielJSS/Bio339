# von Bertalanffy growth curve for experimentally reared guppies
getwd()


library(openxlsx)
library(plotrix)

root<-"O:"
setwd(paste0(root,"/UiB/BIO339"))

d<-read.xlsx("guppies.xlsx",sheet=1)

summary(d) # some observations are missing lengths

show(d)

# preliminary plotting and look
sizeplot(d$age,d$length,scale=.3,pch=16)
with(d,tapply(length,list(sex,age),mean,na.rm=T))
plot(sort(unique(d$age)),tapply(d$length,d$age,mean,na.rm=T),type="o")
plot(sort(unique(d$age)),tapply(d$length,d$age,mean,na.rm=T),type="o",log="xy")

# simple fit
summary(vB<-nls(length~Linf*(1-exp(-k*(age-t0))),data=d,start=list(Linf=16,k=0.1,t0=0)))

### sex differences

summary(vB.f<-nls(length~Linf*(1-exp(-k*(age-t0))),data=d[d$sex=="F",],start=list(Linf=16,k=0.1,t0=0)))
summary(vB.m<-nls(length~Linf*(1-exp(-k*(age-t0))),data=d[d$sex=="M",],start=list(Linf=16,k=0.1,t0=0)))

### is the sex differrence significant?

# you can compare the confidence intervals
summary(vB.f)$coef[,"Estimate"]+1.96*summary(vB.f)$coef[,"Std. Error"]
summary(vB.f)$coef[,"Estimate"]-1.96*summary(vB.f)$coef[,"Std. Error"]

summary(vB.m)$coef[,"Estimate"]+1.96*summary(vB.m)$coef[,"Std. Error"]
summary(vB.m)$coef[,"Estimate"]-1.96*summary(vB.m)$coef[,"Std. Error"]

# you can also test significance with model that includes both sexes
summary(vB.fm0<-nls(length~Linf*(1-exp(-k*(age-t0))),data=d,start=list(Linf=16,k=0.1,t0=0)))	# null model
summary(vB.fm1<-nls(length~Linf*(1-exp(-k*(1+k.m*(sex=="M"))*(age-t0))),data=d,start=list(Linf=16,k=0.1,t0=0,k.m=0)))	# sex effect in k
summary(vB.fm2<-nls(length~Linf*(1+Linf.m*(sex=="M"))*(1-exp(-k*(age-t0))),data=d,start=list(Linf=16,k=0.1,t0=0,Linf.m=0)))	# sex effect in Linf
summary(vB.fm3<-nls(length~Linf*(1+Linf.m*(sex=="M"))*(1-exp(-k*(1+k.m*(sex=="M"))*(age-t0))),data=d,start=list(Linf=16,k=0.1,t0=0,Linf.m=0,k.m=0)))	# sex effect in both
AIC(vB.fm0,vB.fm1,vB.fm2,vB.fm3)


### log-normal errors - are they needed?

# some diagnostics
sizeplot(d$length,resid(vB.fm3),scale=0.5)	# no clear indication of heteroschedasticity
scatter.smooth(fitted(vB.fm3),resid(vB.fm3))
hist(resid(vB.fm3))		# there is a slight tendency for skew, but nothing serious (and in the direction where log-transformation isn't helping)

# fitting on log-scale - implying log-normal errors - males only
summary(vB.L<-nls(log(length)~log(Linf*(1-exp(-k*(age-t0)))),data=d[d$sex=="M",],start=list(Linf=16,k=0.1,t0=0))) # our previous starting values no longer work
summary(vB.L<-nls(log(length)~log(Linf*(1-exp(-k*(age-t0)))),data=d[d$sex=="M",],start=coef(vB.m) ))		# by now we can provide better starting values

sizeplot(d$length,resid(vB.L),scale=0.5)	# looks OK
scatter.smooth(fitted(vB.L),resid(vB.L))	# looks OK
hist(resid(vB.L))							# not much change

with(d[d$sex=="M",],sizeplot(age,length,scale=.3,pch=21,bg="yellow",xlab="Age (d)",ylab="Length (mm)"))
ages<-seq(min(d$age),max(d$age),length=50)
lines(ages,predict(vB.m,data.frame(age=ages)),lwd=2)
lines(ages,exp(predict(vB.L,data.frame(age=ages))),lty="dotted",lwd=2)
legend("topleft",c("normal","log-normal"),lty=c("solid","dotted"),bty="n")

