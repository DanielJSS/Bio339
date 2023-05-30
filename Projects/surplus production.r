# surplus production modelling with SPiCT

# if running the first time:
devtools::install_github("DTUAqua/spict/spict")

library(spict)
1
# loads example datasets from Polacheck et al. (1993) - including NZ rock lobsters
data(pol)

# slots obsC and obsI contain catches and abundance index, respectively
# because SPiCT is a continuous-time model, it needs information on when in a year catches and abundance index are observed
# these are slots timeC and timeI, respectively
# for NZ rock lobster, everything is assumed to apply to the beginning of each year
pol$lobster	

# nice plot on the time series
plotspict.data(pol$lobster)

# preliminary analyses based on regression fits - this would have been approach some decades ago
plotspict.ci(pol$lobster)

# fitting the model with defaul settings
# these include relatively non-informative priors for the noise parameters
res<-fit.spict(pol$lobster)
summary(res)

# default plots
plot(res)

# individual plots can be plotted separately
plotspict.production(res)

# one-step-ahead residuals
res<-calc.osa.resid(res)
plotspict.diagnostic(res)

# retrospective analysis
# Mohn's rho is a measure of average relative bias (so ideally, it should be close to zero)
retro.res<-retro(res)
  plotspict.retro(retro.res)
  