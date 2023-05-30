# blue whiting assessment with SAM

# if using for the first time or updating - this takes several minutes
devtools::install_github("fishfollower/SAM/stockassessment")

library(stockassessment)
getwd()
3
setwd("~/UiB/BIO339/blue whiting")

# the data are available in "Lowestoft XSA" or "CEFAS format"
# read.ices() convert them into simple dataframes
cn<-read.ices("cn.dat")	# catch numbers
cw<-read.ices("cw.dat") # catch weights
dw<-read.ices("dw.dat")	# discard weights (no different from the above)
sw<-read.ices("sw.dat")	# stock weights (no different from the above)
lw<-read.ices("lw.dat")	# landing weights (no different from the above)
lf<-read.ices("lf.dat")	# landing fraction
mo<-read.ices("mo.dat")	# maturity ogive (proportion mature)
nm<-read.ices("nm.dat")	# natural mortality
pf<-read.ices("pf.dat")	# proportion of fishing before spawning
pm<-read.ices("pm.dat") # proportion of natural mortality before spawning
surveys<-read.ices("survey.dat")	# survey indices (here only Intern. Blue Whiting SS Survey

# create SAM data object
dat<-setup.sam.data(
	surveys=surveys,
	residual.fleet=cn, 
	prop.mature=mo, 
	stock.mean.weight=sw, 
	catch.mean.weight=cw, 
	dis.mean.weight=dw, 
	land.mean.weight=lw,
	prop.f=pf, 
	prop.m=pm, 
	natural.mortality=nm, 
	land.frac=lf)

# create configuration object with default settings
conf<-defcon(dat)
conf<-defcon(dat,level=2) # as above, but with AR(1)

# update with setting used in BLW assessment
conf$fbarRange<-c(3,7)

# altenatively, use an existing configuration file
conf<-loadConf(dat,"caonfig.txt")

# generate initial values - could be altered
par<-defpar(dat,conf)

# fit the model
fit<-sam.fit(dat,conf,par)


# stock summary table
summary(fit)

# catches
catchtable(fit)

# illustrate - basic plots
ssbplot(fit)
fbarplot(fit)
recplot(fit)
catchplot(fit)

# SSB-R relationship (sort of)
srplot(fit)

# residuals (one observation ahead residuals)
res<-residuals(fit)
plot(res)

# retrospective plot - shows how the latest part of the assessment changes with more data
retro<-retro(fit,year=10)
plot(retro)

# short term prediction with status quo F
forecast(fit,fscale=c(1,1,1,1))

fit<-sam.fit(dat,conf,par,fullDerived=TRU