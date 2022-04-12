library(brms)

#---------------- beta-binomial example

dat <- data.frame(success = 19, size = 57)
dat

# intuition
19/57

# max lik estimate
mle <- glm(cbind(success, size - success) ~ 1, 
           family = binomial("logit"), # family = binomial("identity")
           data = dat)
mle
coef(mle) # logit scale
plogis(coef(mle))

# bayesian w/ mcmc and default prior
bayes.brms <- brm(success | trials(size) ~ 1, 
                  family = binomial("logit"), 
                  data = dat)
summary(bayes.brms)
fixef(bayes.brms)
plogis(fixef(bayes.brms)[1])
plot(bayes.brms)

draws_fit <- as_draws_matrix(bayes.brms)
draws_fit
mean(plogis(draws_fit[,1]))
posterior::summarize_draws(plogis(draws_fit[,1]))

get_prior(success | trials(size) ~ 1, 
          family = binomial("logit"), 
          data = dat)

# bayesian w/ mcmc and prior specification by hand
nlprior <- prior(normal(0, 1.5), class = "Intercept")
bayes.brms <- brm(success | trials(size) ~ 1, 
                  family = binomial("logit"), 
                  data = dat, 
                  prior = nlprior)
summary(bayes.brms)
fixef(bayes.brms)
plogis(fixef(bayes.brms)[1])

draws_fit <- as_draws_matrix(bayes.brms)
draws_fit
posterior::summarize_draws(plogis(draws_fit[,1]))

#-------------- white stork example - logistic regression

nbchicks <- c(151,105,73,107,113,87,77,108,118,122,112,120,122,89,69,71,53,41,53,31,35,14,18)
nbpairs <- c(173,164,103,113,122,112,98,121,132,136,133,137,145,117,90,80,67,54,58,39,42,23,23)
temp <- c(15.1,13.3,15.3,13.3,14.6,15.6,13.1,13.1,15.0,11.7,15.3,14.4,14.4,12.7,11.7,11.9,15.9,13.4,14.0,13.9,12.9,15.1,13.0)
rain <- c(67,52,88,61,32,36,72,43,92,32,86,28,57,55,66,26,28,96,48,90,86,78,87)
dat <- data.frame(nbchicks = nbchicks, 
                  nbpairs = nbpairs,
                  temp = (temp - mean(temp))/sd(temp), # standardized temperature
                  rain = (rain - mean(rain))/sd(rain)) # standardized rainfall

# max lik estimate
mle <- glm(cbind(nbchicks, nbpairs - nbchicks) ~ temp + rain, 
           family = binomial("logit"), 
           data = dat)
mle
summary(mle)

# bayesian w/ mcmc and default prior
bayes.brms <- brm(nbchicks | trials(nbpairs) ~ temp + rain, 
                  family = binomial("logit"), 
                  data = dat)
summary(bayes.brms)
fixef(bayes.brms)
plot(bayes.brms)
waic(bayes.brms)

#-------------- seed bank example - linear mixed regression

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

# data
dat <- data.frame(y = y, x = x, species = species)

# max lik estimates
library(lme4)
mle <- lmer(y ~ x + (1 | species), data = dat)
summary(mle)

# bayesian w/ mcmc and default prior
bayes.brms <- brm(y ~ x + (1 | species), data = dat)
summary(bayes.brms)
plot(bayes.brms)
waic(bayes.brms)

#----------- GLMM example w/ transects and anemones counts

# simulate data
transects <- 10
data <- NULL
for (tr in 1:transects){
  # random effect (intercept)
  ref <- rnorm(1,0,.5) 
  # water temperature gradient
  t <- runif(1, 18,22) + runif(1,-.2,0.2)*1:20 
  # Anemone gradient (expected response)
  ans <- exp(ref -14 + 1.8 * t - 0.045 * t^2) 
  # actual counts on 20 segments of the current transect
  an <- rpois(20, ans) 
  data <- rbind(data, cbind(rep(tr, 20), t, an))
}

data <- data.frame(Transect = data[,1],
                   Temperature = data[,2],
                   Anemones = data[,3])

# standardize temperature
boo <- data$Temperature
data$Temp <- (boo - mean(boo)) / sd(boo)
head(data)

#-- dans Jags

model <- function() {
  for (i in 1:n){
    count[i] ~ dpois(lambda[i])
    log(lambda[i]) <- a[transect[i]] + b[1] * x[i] + b[2] * pow(x[i],2)
  }
  for (j in 1:nbtransects){
    a[j] ~ dnorm (mu.a, tau.a)
  }
  mu.a ~ dnorm (0, 0.001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif (0, 100)
  b[1] ~ dnorm (0, 0.001)
  b[2] ~ dnorm (0, 0.001)
}

dat <- list(n = nrow(data), 
            nbtransects = transects, 
            x = data$Temp, 
            count = data$Anemones, 
            transect = data$Transect)
init1 <- list(a = rnorm(transects), b = rnorm(2), 
              mu.a = rnorm(1), sigma.a = runif(1))
init2 <- list(a = rnorm(transects), b = rnorm(2), 
              mu.a = rnorm(1), sigma.a = runif(1))
inits <- list(init1, init2)
par <- c ("a", "b", "mu.a", "sigma.a")

library(R2jags)
fit <- jags(data = dat, 
            inits = inits, 
            parameters.to.save = par, 
            n.iter = 2500, 
            model.file = model,
            n.chains = 2, 
            n.burn = 1000)

round(fit$BUGSoutput$summary[, -c(4,6)], 3)


#-- avec lme4

library(lme4)
fit_lme4 <- glmer(Anemones ~ Temp + I(Temp^2) + (1 | Transect), 
                  data = data, family = poisson)
fit_lme4

#-- avec brms

library(brms)
bayes.brms <- brm(Anemones ~ Temp + I(Temp^2) + (1 | Transect), 
                  data = data,
                  family = poisson("log"))
summary(bayes.brms)
plot(bayes.brms)
waic(bayes.brms)

