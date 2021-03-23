# Bayesian statistics for ecology

This repository holds the source materials used at https://oliviergimenez.github.io/bayesian-stats-with-R/
 
## Reuse

Text and figures are licensed under Creative Commons Attribution [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). Any computer code (R, HTML, CSS, etc.) in slides and worksheets, including in slide and worksheet sources, is also licensed under [MIT](https://github.com/oliviergimenez/bayesian-stats-with-R/blob/master/LICENSE.md).

## To-do list

+ Mettre de l’ordre sur diagnostics convergence. Refaire figure. 

+ Add a plot with several lines from posterior distribution of regression parameters to a plot of mean response function of a covariate; then get the credible interval on the prediction. 

+ Quelque chose sur posterior predictive checks (https://m-clark.github.io/bayesian-basics/diagnostics.html#predictive-accuracy-model-comparison et https://stats.stackexchange.com/questions/115157/what-are-posterior-predictive-checks-and-what-makes-them-useful); add a section on posterior predictive checks, to comply with the 3 steps of a Bayesian analysis as defined by Gelman (set up a probabilistic model, inference and model checking; iterate to improve model).

+ Say something about confidence, credible and HPD intervals

+ Properly introduce GLMs

+ Faire prior predictive check pour logistic storks et lmm plants
sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

+ Add a section on LOO, and discuss complementarity with WAIC.

+ Add another Metropolis example, with adaptation, with the beta-binomial example, and discuss several levels of acceptance. Metropolis RW sur binomial avec adaptatif et burnin https://bayesball.github.io/BOOK/simulation-by-markov-chain-monte-carlo.html

+ Tout passer en ggplot (MCMC diagnostics library(bayesplot), https://www.tjmahr.com/plotting-partial-pooling-in-mixed-effects-models/). Do all plots with `ggplot2`; add [short introduction to the `Tidyverse`](https://github.com/oliviergimenez/intro_tidyverse).

+ Rajouter animation joyplots Rasmus Baath http://www.sumsar.net/blog/2018/12/visualizing-the-beta-binomial/ ou https://relaxed-beaver-4b4dc8.netlify.app/exercises_part1.html 

+ Typos: 
    + Beta distribution: use $a, b$ or $\alpha, \beta$ throughout
    + End of the first stops at incorporating info in prior capture-recapture example

+ Add something on equivalence w/ MLE: exemple de la vraisemblance binomiale $Bin(n,k)$ et du prior beta $Beta(a,b)$. On a vu que la distribution a posteriori de la prob de succès est une beta $Beta(a+k, b+n-k)$. La moyenne a posteriori est $(a+k)/(a+b+n)$ qu'on peut aussi écrire $(1-w)(a/a+b) + w k/n$. Autrement dit, la moyenne a posteriori est la moyenne pondérée par $w$ de la moyenne de prior et du MLE. Quand on a un gros échantillon, $n$ tend vers l'infini pour faire court, et la moyenne a posteriori tend vers le MLE, et ce quelque soit le prior. Same reasonning with variance shows that Bayes gives reasonable results, even w/ small sample size. 

+ Replace Metropolis example with deer example, makes the link with what precedes, also real examples that involves data, whereas the current example is about approximating a discrete distribution. 

