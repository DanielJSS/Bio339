install.packages("checker")
checker::chk_requirements("https://raw.githubusercontent.com/richardjtelford/Bio302/main/checker/bio302.yaml")

n_draw <- 10000

# Defining and drawing from the prior distribution
prior_rate <- runif(n_draw, 0, 1)

# Defining the generative model
gen_model <- function(rate) {
  subscribers <- rbinom(1, size = 16, prob = rate)
  subscribers
}

# Simulating the data
subscribers <- rep(NA, n_draw)
for(i in 1:n_draw) {
  subscribers[i] <- gen_model(prior_rate[i])
}

# Filtering out those parameter values that didn't result in the
# data that we actually observed
post_rate <- prior_rate[subscribers == 6]

# Checking that there enough samples left
length(post_rate)

## [1] 578

# Plotting and summarising the posterior.
hist(post_rate, xlim = c(0, 1))

mean(post_rate)

## [1] 0.3862927

quantile(post_rate, c(0.025, 0.975))

##      2.5%     97.5% 
## 0.1956573 0.6189745


sum(post_rate > 0.2) / length(post_rate)

## [1] 0.9705882

# This can be done with a for loop
singnups <- rep(NA, length(post_rate))
for(i in 1:length(post_rate)) {
  singnups[i] <- rbinom(n = 1, size = 100, prob = post_rate[i])
}

# But since rbinom is vectorized we can simply write it like this:
signups <- rbinom(n = length(post_rate), size = 100, prob = post_rate)

hist(signups, xlim = c(0, 100))

quantile(signups, c(0.025, 0.975))

##  2.5% 97.5% 
##    18    62


#Linear regression 1 :----
#Bivariate descriptive stat 
#Measures of assosiation - covariance and correaltion 
#use with two continouse variables, paried data, unclear direction of casuality
#covarance - assosiation between two variavles 
#correlation is standariced assosiation. pearson coefficient of correlation, positive or negative correlation 
# Correlacion in R cor(x=w$z,y=r$e, use ="pairwise.complete")
#least square regression, Bo is instercept, Bx is effect 
# y = cont respons, x = cont or categorical predictor,  


  
