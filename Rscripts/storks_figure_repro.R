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

# load R2jags
library(R2jags)

# run Jags
storks <- jags(data = datax,
               inits = inits,
               parameters.to.save = parameters,
               model.file = logistic, 
               n.chains = 2,
               n.iter = 12000, # includes burn-in!
               n.burnin = 2000,
               n.thin = 1)

# display results
storks

#--- figure de la prob de repro fn de la pluie
#-- on utilise les parametres estimes pour reconstituer la proba de repro

# on recupere les valeurs tirees dans la distribution a posterior de chaque parametre
intercept <- storks$BUGSoutput$sims.matrix[,"a"]
effet.pluie <- storks$BUGSoutput$sims.matrix[,"b.rain"]
effet.temp <- storks$BUGSoutput$sims.matrix[,"b.temp"]

# on forme la prob de succes fonction de la pluie, pour une valeur moyenne de temperature
# comme la temperature est standardisee, cette moyenne est 0 ; on fait ça pour chaque iteration
n.iter <- length(intercept)
n.year <- length(rain)
logit.repro <- matrix(NA, nrow = n.iter, ncol = n.year)
for (i in 1:n.iter){ # pour chaque iteration
  for (j in 1:n.year){ # pour chaque annee
    logit.repro[i,j] <- intercept[i] + effet.pluie[i] * (rain[j] - mean(rain))/sd(rain)
  }
}
# on applique le logit inverse pour obtenir la prob de repro entre 0 et 1
repro.mcmc <- plogis(logit.repro) # dim(repro) est n.iter x n.year

# on calcule les moyennes
repro <- apply(repro.mcmc, 2, mean)

# on visualise la relation repro vs. pluie en moyenne
library(tidyverse)

data.frame(pluie = (rain - mean(rain))/sd(rain), 
           repro = repro) %>%
  ggplot() + 
  aes(x = pluie, y = repro) +
  geom_line()

# une autre visualisation
data.frame(pluie = (rain - mean(rain))/sd(rain), 
           repro = repro) %>%
  ggplot() + 
  aes(x = pluie, y = repro) +
  geom_point(size = 2) +
  geom_line() +
  geom_point(color = "white") +
  theme_light()

# on ajoute un intervalle de credibilite

borne.inf <- apply(repro.mcmc, 2, quantile, probs = 2.5/100)
borne.sup <- apply(repro.mcmc, 2, quantile, probs = 97.5/100)

data.frame(pluie = (rain - mean(rain))/sd(rain), 
           repro = repro,
           borne.inf = borne.inf,
           borne.sup = borne.sup) %>%
  ggplot(aes(x = pluie, y = repro)) + 
  geom_ribbon(aes(ymin = borne.inf,
                  ymax = borne.sup),
              fill = "grey70") + 
  geom_line() +
  theme_light()


# plutot que d'utiliser les valeurs observees de la covariable standardisee pluie,
# on peut creer une grille de valeurs auxquelles on calcule la proba de repro estimee

pluie_standardisee <- (rain - mean(rain))/sd(rain)
grid.pluie <- seq(min(pluie_standardisee), max(pluie_standardisee), length = 100)
logit.repro <- matrix(NA, nrow = n.iter, ncol = length(grid.pluie))

for (i in 1:n.iter){ # pour chaque iteration
  for (j in 1:length(grid.pluie)){ # pour chaque annee
    logit.repro[i,j] <- intercept[i] + effet.pluie[i] * grid.pluie[j]
  }
}
# on applique le logit inverse pour obtenir la prob de repro entre 0 et 1
repro.mcmc <- plogis(logit.repro) # dim(repro) est n.iter x n.year

# on calcule les moyennes aux valeurs de la grille pour la pluie
repro <- apply(repro.mcmc, 2, mean)
borne.inf <- apply(repro.mcmc, 2, quantile, probs = 2.5/100)
borne.sup <- apply(repro.mcmc, 2, quantile, probs = 97.5/100)

data.frame(pluie = grid.pluie, 
           repro = repro,
           borne.inf = borne.inf,
           borne.sup = borne.sup) %>%
  ggplot(aes(x = pluie, y = repro)) + 
  geom_ribbon(aes(ymin = borne.inf,
                  ymax = borne.sup),
              fill = "grey70") + 
  geom_line() +
  theme_light()


