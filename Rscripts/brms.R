library(brms)

#---------------- beta-binomial example

y <- 19
n <- 57
dat <- data.frame(success = y, size = n)
dat

# intuition
19/57

# max lik estimate
mle <- glm(cbind(success, size - success) ~ 1, family = binomial("logit"), data = dat)
mle
coef(mle) # logit scale
plogis(coef(mle))

# bayesian w/ mcmc and default prior
bayes.brms <- brm(success | trials(size) ~ 1, family = binomial("logit"), data = dat)
summary(bayes.brms)
fixef(bayes.brms)
plogis(fixef(bayes.brms)[1])
plot(bayes.brms)

draws_fit <- as_draws_matrix(bayes.brms)
draws_fit
posterior::summarize_draws(plogis(draws_fit[,1]))

# bayesian w/ mcmc and prior specification by hand
nlprior <- prior(normal(0, 1.5), class = "Intercept")
bayes.brms <- brm(success | trials(size) ~ 1, family = binomial("logit"), data = dat, prior = nlprior)
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
mle <- glm(cbind(nbchicks, nbpairs - nbchicks) ~ temp + rain, family = binomial("logit"), data = dat)
mle
summary(mle)

# bayesian w/ mcmc and default prior
bayes.brms <- brm(nbchicks | trials(nbpairs) ~ temp + rain, family = binomial("logit"), data = dat)
summary(bayes.brms)
fixef(bayes.brms)
plot(bayes.brms)


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

