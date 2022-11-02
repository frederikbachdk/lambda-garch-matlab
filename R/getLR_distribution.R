#############################################################################
### load libraries and settings ###
#############################################################################
library(tidyverse)
library(tictoc)
library(gridExtra) # gridExtra is loaded for figures

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

# import functins
source('R/utils/simulationFunctions.R')

# define the "true" DGP and calculate the log-likelihood
set.seed(2022)
ARCH1_true <- ARCH1(T = 10000, x0 = 0, omega = 1, alpha = 0)
loglik_true <- ARCH1_loglikelihood(omega = 1, alpha = 0, y = ARCH1_true$x)

N <- 1000 # number of simulations

simulations <- tibble(
  sim = seq(1,N),
  LR  = rep(0, N)
)

for(n in (1:N)){
  
  # count simulation number
  simulations$sim[n] <- n  
  
  print(paste0('Series ', n, ' out of ', N))
  
  # simulate ARCH and maximize log likelihood function
  ARCH1_sim <- ARCH1(T = 10000, x0 = 0, omega = 1, alpha = 0)
  solution <- optim(par = 0.2,
                    fn = negative_loglik,method = "L-BFGS-B",
                    lower = 0,
                    hessian = TRUE) 
  
  loglik_sim <- -solution$value
  
  # calculate LR statistic for alpha = 0
  simulations$LR[n] <- -2*(loglik_sim - loglik_true)
  }




