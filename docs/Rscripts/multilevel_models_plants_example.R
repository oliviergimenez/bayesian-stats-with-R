#--------- DATA

# read in data, directly from course website
library(readr)
df <- read_csv2("https://raw.githubusercontent.com/oliviergimenez/bayesian-stats-with-R/master/slides/dat/VMG.csv")
df

# use relevant format for columns Sp and Vm
library(forcats)
df$Sp <- as_factor(df$Sp)
df$Vm <- as.numeric(df$Vm)

# define numeric vector of species
species <- as.numeric(df$Sp)

# define response variable, number of seeds
y <- log(df$NGrTotest)

# standardize explanatory variable, biomass
x <- (df$Vm - mean(df$Vm))/sd(df$Vm)

#--------- COMPLETE POOLING MODEL

# complete pooling model
complete_pooling <- function(){
  # likelihood
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau.y)
    mu[i] <- a + b * x[i]
  }
  # priors
  tau.y <- 1 / (sigma.y * sigma.y)
  sigma.y ~ dunif(0,100)
  a ~ dnorm(0,0.001)
  b ~ dnorm(0,0.001)
}

# data
mydata <- list(y = y, x = x, n = length(y))

# initial values
init1 <- list(a = -0.5, b = -0.5, sigma.y = 0.5)
init2 <- list(a = 0.5, b = 0.5, sigma.y = 1.5)
init <- list(init1, init2)

# parameters to monitor
params <- c("a", "b", "sigma.y")

# call jags to fit model
library(R2jags)
complete_pooling_fit <- jags(data = mydata,
                             inits = init,
                             parameters.to.save = params,
                             model.file = complete_pooling,
                             n.chains = 2,
                             n.iter = 5000,
                             n.burnin = 2500,
                             n.thin = 1)
complete_pooling_fit

# have a look to the MCMC values
head(complete_pooling_fit$BUGSoutput$sims.matrix)

# dimensions
dim(complete_pooling_fit$BUGSoutput$sims.matrix)

# compute posterior mean
apply(complete_pooling_fit$BUGSoutput$sims.matrix, 2, mean)

# compute posterior sd
apply(complete_pooling_fit$BUGSoutput$sims.matrix, 2, sd)

# compute credible intervals
apply(complete_pooling_fit$BUGSoutput$sims.matrix, 2, quantile, probs = c(2.5, 97.5)/100)

# have a look to posterior distribution
hist(complete_pooling_fit$BUGSoutput$sims.matrix[,"b"])
plot(density(complete_pooling_fit$BUGSoutput$sims.matrix[,"b"]))

# compare to frequentist approach
complete_pooling_fit_mle <- lm(y ~ x, data = mydata)
summary(complete_pooling_fit_mle)

#--------- PARTIAL POOLING MODEL

# partial pooling model
partial_pooling <- function(){
  # likelihood
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau.y)
    mu[i] <- a[species[i]] + b * x[i]
  }
  for (j in 1:nbspecies){
    a[j] ~ dnorm(mu.a, tau.a)
  }
  # priors
  tau.y <- 1 / (sigma.y * sigma.y)
  sigma.y ~ dunif(0,100)
  mu.a ~ dnorm(0,0.001)
  tau.a <- 1 / (sigma.a * sigma.a)
  sigma.a ~ dunif(0,100)
  b ~ dnorm(0,0.001)
}

# data
mydata <- list(y = y, 
               x = x, 
               n = length(y),
               species = species,
               nbspecies = length(levels(df$Sp)))

# initial values
init1 <- list(mu.a = -0.5, sigma.a = 0.5, b = -0.5, sigma.y = 0.5)
init2 <- list(mu.a = 0.5, sigma.a = 1.5, b = 0.5, sigma.y = 1.5)
init <- list(init1, init2)

# parameters to monitor
params <- c("mu.a", "sigma.a", "b", "sigma.y")

# call jags to fit model
library(R2jags)
partial_pooling_fit <- jags(data = mydata,
                             inits = init,
                             parameters.to.save = params,
                             model.file = partial_pooling,
                             n.chains = 2,
                             n.iter = 5000,
                             n.burnin = 2500,
                             n.thin = 1)
partial_pooling_fit

# have a look to the MCMC values
head(partial_pooling_fit$BUGSoutput$sims.matrix)

# dimensions
dim(partial_pooling_fit$BUGSoutput$sims.matrix)

# compute posterior mean
apply(partial_pooling_fit$BUGSoutput$sims.matrix, 2, mean)

# compute posterior sd
apply(partial_pooling_fit$BUGSoutput$sims.matrix, 2, sd)

# compute credible intervals
apply(partial_pooling_fit$BUGSoutput$sims.matrix, 2, quantile, probs = c(2.5, 97.5)/100)

# have a look to posterior distributions
hist(partial_pooling_fit$BUGSoutput$sims.matrix[,"b"])
hist(partial_pooling_fit$BUGSoutput$sims.matrix[,"mu.a"])
hist(partial_pooling_fit$BUGSoutput$sims.matrix[,"sigma.a"])

# compare to frequentist approach
library(lme4)
partial_pooling_fit_mle <- lmer(y ~ x + (1 | species), data = mydata)
summary(partial_pooling_fit_mle)

#--------- NO POOLING MODEL

# no pooling model
no_pooling <- function(){
  # likelihood
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau.y)
    mu[i] <- a[species[i]] + b * x[i]
  }
  # priors
  tau.y <- 1 / (sigma.y * sigma.y)
  sigma.y ~ dunif(0,100)
  for (j in 1:nbspecies){
    a[j] ~ dnorm(0, 0.001)
  }
  b ~ dnorm(0,0.001)
}

# data
mydata <- list(y = y, 
               x = x, 
               n = length(y),
               species = species,
               nbspecies = length(levels(df$Sp)))

# initial values
init1 <- list(a = rep(-0.5, mydata$nbspecies), b = -0.5, sigma.y = 0.5)
init2 <- list(a = rep(0.5, mydata$nbspecies), b = 0.5, sigma.y = 1.5)
init <- list(init1, init2)

# parameters to monitor
params <- c("a", "b", "sigma.y")

# call jags to fit model
library(R2jags)
no_pooling_fit <- jags(data = mydata,
                            inits = init,
                            parameters.to.save = params,
                            model.file = no_pooling,
                            n.chains = 2,
                            n.iter = 5000,
                            n.burnin = 2500,
                            n.thin = 1)
no_pooling_fit

# compare to frequentist approach
no_pooling_fit_mle <- lm(y ~ -1 + as_factor(species) + x, data = mydata)
summary(no_pooling_fit_mle)

