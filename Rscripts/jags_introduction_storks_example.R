# read in data
nbchicks <- c(151,105,73,107,113,87,77,108,118,122,112,120,122,89,69,71,53,41,53,31,35,14,18)
nbpairs <- c(173,164,103,113,122,112,98,121,132,136,133,137,145,117,90,80,67,54,58,39,42,23,23)
temp <- c(15.1,13.3,15.3,13.3,14.6,15.6,13.1,13.1,15.0,11.7,15.3,14.4,14.4,12.7,11.7,11.9,15.9,13.4,14.0,13.9,12.9,15.1,13.0)
rain <- c(67,52,88,61,32,36,72,43,92,32,86,28,57,55,66,26,28,96,48,90,86,78,87)
datax <- list(N = 23, # nb of years
              nbchicks = nbchicks, 
              nbpairs = nbpairs,
              temp = (temp - mean(temp))/sd(temp), # standardized temperature
              rain = (rain - mean(rain))/sd(rain)) # standardized rainfall

# define model
logistic <- function() {
  # likelihood
  for(i in 1:N){ # loop over years
  nbchicks[i] ~ dbin(p[i],nbpairs[i]) # binomial likelihood
  logit(p[i]) <- a + b.temp * temp[i] + b.rain * rain[i] # prob success is linear fn of temp and rainfall
                                                         # on logit scale
  }
  # priors
  a ~ dnorm(0,0.001) # intercept
  b.temp ~ dnorm(0,0.001) # slope for temperature
  b.rain ~ dnorm(0,0.001) # slope for rainfall
}


# list of lists of initial values (one for each MCMC chain)
init1 <- list(a = -0.5, b.temp = -0.5, b.rain = -0.5)
init2 <- list(a = 0.5, b.temp = 0.5, b.rain = 0.5)
inits <- list(init1, init2)

# specify parameters that need to be estimated
parameters <- c("a", "b.temp", "b.rain")

# specify nb iterations for burn-in and final inference
nb.burnin <- 2000
nb.iterations <- 10000

# load R2jags
library(R2jags)

# run Jags
storks <- jags(data = datax,
               inits = inits,
               parameters.to.save = parameters,
               model.file = logistic, 
               n.chains = 2,
               n.iter = nb.iterations, # includes burn-in!
               n.burnin = nb.burnin)

# display results
storks

# explore MCMC simulations
head(storks$BUGSoutput$sims.matrix)

# get posterior means for all parameters
apply(storks$BUGSoutput$sims.matrix, 2, mean)

# get posterior sd for all parameters
apply(storks$BUGSoutput$sims.matrix, 2, sd)

# get 95% credible intervals all parameters
apply(storks$BUGSoutput$sims.matrix, 2, quantile, probs = c(2.5, 97.5)/100)

# get histogram of the posterior distribution of slopes
par(mfrow = c(1, 2))
hist(storks$BUGSoutput$sims.matrix[, "b.temp"], main = "", xlab = "temp slope")
hist(storks$BUGSoutput$sims.matrix[, "b.rain"], main = "", xlab = "rain slope")

# compare to frequentist approach
storks_mle <- glm(cbind(nbchicks, nbpairs - nbchicks) ~ temp + rain, 
    family = binomial(link = "logit"),
    data = datax)
summary(storks_mle)

