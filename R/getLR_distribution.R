#############################################################################
### load libraries and settings ###
#############################################################################
library(tidyverse)
library(gridExtra) # gridExtra is loaded for figures
library(EnvStats)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

# import functins
source('R/utils/simulationFunctions.R')

# define the "true" DGP and calculate the log-likelihood
set.seed(2022)

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
  ARCH1_sim <- ARCH1(T = 2000, x0 = 0, omega = 1, alpha = c(0.5,0))
  y <- ARCH1_sim$x
  
  # calculate MLE for unrestricted theta
  solution <- optim(par = c(1,0.5,0.01),
                    fn = negative_loglik,method = "L-BFGS-B",
                    lower = c(0,1e-10,1e-10),
                    hessian = TRUE) 
  
  # calculate MLE for restricted theta
  loglik_unr <- -solution$value
  
  loglik_res <- ARCH1_loglikelihood(theta = c(solution$par[1],solution$par[2],0), y)
  
  # calculate LR statistic for alpha = 0
  simulations$LR[n] <- -2*(loglik_res - loglik_unr)
  }

epdfPlot(simulations$LR)
quantile(simulations$LR, 0.95)

