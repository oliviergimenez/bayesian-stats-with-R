# Bayesian statistics for ecology

This repository holds the source materials used at https://oliviergimenez.github.io/bayesian-stats-with-R/
 
## Reuse

Text and figures are licensed under Creative Commons Attribution [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Any computer code (R, HTML, CSS, etc.) in slides and worksheets, including in slide and worksheet sources, is also licensed under [MIT](https://github.com/oliviergimenez/bayesian-stats-with-R/blob/master/LICENSE.md).

## To-do list

+ Add a section on posterior predictive checks (https://m-clark.github.io/bayesian-basics/diagnostics.html#predictive-accuracy-model-comparison and https://stats.stackexchange.com/questions/115157/what-are-posterior-predictive-checks-and-what-makes-them-useful), to comply with the 3 steps of a Bayesian analysis as defined by Gelman (set up a probabilistic model, inference and model checking; iterate to improve model).

+ More details on confidence, credible and HPD intervals.

+ Add a section on LOO, and discuss complementarity with WAIC.

+ Properly introduce GLMs.

+ Switch to Nimble. 

+ Finish up writing that book. 

+ Add equivalent analysis in brms so that non-coders can still use bayes stats. 

+ Clean up section on convergence diagnostics. Make figure reproducible.  

+ Add a plot with several lines from posterior distribution of regression parameters to a plot of mean response function of a covariate; then get the credible interval on the prediction. 

+ Prior predictive check for logistic storks and lmm plants
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

+ Add another Metropolis example, with adaptation, with the beta-binomial example, and discuss several levels of acceptance. Metropolis RW sur binomial avec adaptatif et burnin https://bayesball.github.io/BOOK/simulation-by-markov-chain-monte-carlo.html. Maybe do a flexdashboard. 

+ Use ggplot throughout (MCMC diagnostics library(bayesplot), https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/). Add [short introduction to the `Tidyverse`](https://github.com/oliviergimenez/intro_tidyverse).

+ Add animation joyplots Rasmus Baath http://www.sumsar.net/blog/2018/12/visualizing-the-beta-binomial/ ou https://relaxed-beaver-4b4dc8.netlify.app/exercises_part1.html 

+ Typos: 
    + Beta distribution: use $a, b$ or $\alpha, \beta$ throughout
    + End of the first stops at incorporating info in prior capture-recapture example

+ Add something on equivalence w/ MLE: say binomial lik $Bin(n,k)$ and beta prior $Beta(a,b)$ then posterior is beta $Beta(a+k, b+n-k)$; posterior mean is $(a+k)/(a+b+n)$ which can be written $(1-w)(a/a+b) + w k/n$. Posterior mean is weighted average of prior mean and MLE. When sample size is big, $n$ tends to infinity and posterior mean tends to MLE, whatever the prior. Same reasoning with variance shows that Bayes gives reasonable results, even w/ small sample size. 

+ More (ecological) interpretation 
+ ~~Add a script to plot stuff in white stork example~~
+ ~~Add a script to TP 9 to show how we can build models of increasing complexity~~
+ Besides (or instead of) wAIC use DIC which is given by JAGS (unpopular opinion)
+ Explain how to add qual explan variable (more than two levels)
+ Explain offset
+ ~~Update website~~
+ Record again videos using M. Lajeunesse setup

+ Include all four case studies that I used for Master exam

+ Aller jusqu’au bout de l’exemple survie, en faisant test d’hypothèse

+ Pourquoi ne pas illustrer dans le contexte du point précédent, le calcul des Bayes factors? Voir https://rstudio-pubs-static.s3.amazonaws.com/358672_09291d0b37ce43f08cf001cfd25c16c2.html, https://stackoverflow.com/questions/60278806/bayes-factor-in-r-with-jaggs, https://www.martinmodrak.cz/2021/03/28/three-ways-to-compute-a-bayes-factor/, http://yourdomain.com/statistics,/modeling/2017/07/07/BF_computation.html ou encore https://michael-franke.github.io/statistics,/modeling/2017/07/07/BF_computation.html

+ Pas vraiment des prior predictive checks

+ Passer tout en nimble - ca résout pb calcul WAIC

+ Montrer le burn-in en faisant tourner le script avec burning = 0 et nb.iter = 250

+ Réconcilier interprétation des estimations pour les modèles plants no pooling / complete pooling et partial pooling ; check calcul du DIC qui donne modèle nul (sans cov) comme meilleur modèle

+ Montrer comment on reconstruit les a[j] pour partial pooling model

+ Régler pb de mise en page

+ Basculer des éléments de mes chapitres de bouquin dans ce cours

+ Add random-intercept random-slope example? See Alford et al Global amphibian
population declines as suggested by B. Bolker?
