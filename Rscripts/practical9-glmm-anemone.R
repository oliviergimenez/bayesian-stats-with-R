# fixe la graine du generateur de donnees
set.seed(2021)

# charge package R2jags
library(R2jags)

# simule les donnees
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

# transforme en data.frame
data <- data.frame(Transect = data[,1],
                   Temperature = data[,2],
                   Anemones = data[,3])
data

#----- glm avec effet lineaire de la temperature de l'eau sur effectifs anemones
reg_poisson_lineaire <- function(){
  for(i in 1:n){
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- a + bun * x[i]
  }
  bun ~ dnorm(0, 0.001)
  a ~ dnorm(0, 0.001)
}

datax <- list(n = nrow(data),
              y = data$Anemones,
              x = (data$Temperature - mean(data$Temperature))/sd(data$Temperature))

init1 <- list(a = -0.5, bun = -0.5)
init2 <- list(a = -0.5, bun = 0.5)
inits <- list(init1,init2)

params <- c("a", "bun")

mod1 <- jags(data = datax,
             inits = inits,
             parameters.to.save = params,
             model.file = reg_poisson_lineaire,
             n.chains = 2,
             n.iter = 5000,
             n.burnin = 2500,
             n.thin = 1)

mod1

# glm avec effet quadratique de la temperature de l'eau sur effectifs anemones
reg_poisson_quadratique <- function(){
  for(i in 1:n){
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- a + bun * x[i] + bdeux * x[i] * x[i]
  }
  bun ~ dnorm(0, 0.001)
  bdeux ~ dnorm(0, 0.001)
  a ~ dnorm(0, 0.001)
}

datax <- list(n = nrow(data),
              y = data$Anemones,
              x = (data$Temperature - mean(data$Temperature))/sd(data$Temperature))

init1 <- list(a, bun = -0.5, bdeux = -0.5)
init2 <- list(a, bun = 0.5, bdeux = 0.5)
inits <- list(init1, init2)

params <- c("a", "bun", "bdeux")

mod2 <- jags(data = datax,
             inits = inits,
             parameters.to.save = params,
             model.file = reg_poisson_quadratique,
             n.chains = 2,
             n.iter = 5000,
             n.burnin = 2500,
             n.thin = 1)

mod2


# glm avec effet quadratique de la temperature de l'eau sur effectifs anemones
# et effet aleatoire transect (GLMM)
reg_poisson_quadratique_randomtransect <- function(){
  for(i in 1:n){
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- a[transect[i]] + bun * x[i] + bdeux * x[i] * x[i]
  }
  bun ~ dnorm(0, 0.001)
  bdeux ~ dnorm(0, 0.001)
  for (j in 1:nbtransect){
    a[j] ~ dnorm(mu.a, tau.a)
  }
  mu.a ~ dnorm(0, 0.001)
  tau.a <- 1 / (sd.a * sd.a)
  sd.a ~ dunif(0, 100)
}

datax <- list(n = nrow(data),
              y = data$Anemones,
              x = (data$Temperature - mean(data$Temperature))/sd(data$Temperature),
              transect = data$Transect,
              nbtransect = 10)

init1 <- list(mu.a = -0.5, bun = -0.5, bdeux = -0.5)
init2 <- list(mu.a = 0.5, bun = 0.5, bdeux = -0.5)
inits <- list(init1, init2)

params <- c("mu.a", "bun", "bdeux", "sd.a")

mod3 <- jags(data = datax,
     inits = inits,
     parameters.to.save = params,
     model.file = reg_poisson_quadratique_randomtransect,
     n.chains = 2,
     n.iter = 5000,
     n.burnin = 2500,
     n.thin = 1)

mod3


