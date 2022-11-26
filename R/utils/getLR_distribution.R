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
source('R/utils/simulationFunctions_2.R')

set.seed(2022)
N <- 1000 # number of simulations

simulations <- tibble(
  sim = seq(1,N),
  LR  = rep(0, N)
)

theta0 <- c(1.00) # theta0

for(n in (1:N)){
  
  # count simulation number
  simulations$sim[n] <- n  
  
  print(paste0('Series ', n, ' out of ', N))
  
  # simulate ARCH(0) and maximize log likelihood function
  ARCH_sim <- ARCH(T = 1000, x0 = 0, theta = theta0)
  y <- ARCH_sim$x
  
  # calculate unrestricted log-likelihood for ARCH(1) process
  solution_unres <- optim(par = c(1/3, 1/10),
                        fn = ARCH1_negative_loglik,
                        method = "L-BFGS-B",
                        lower = c(1e-16,0),
                        hessian = TRUE) 
  
  loglik_unres <- -solution_unres$value
  
  # calculate MLE for restricted theta (H0: alpha1=0)
  solution_res <- optim(par = c(1/3),
                    fn = ARCH0_negative_loglik,
                    method = "L-BFGS-B",
                    lower = c(1e-16),
                    upper = c(Inf),
                    hessian = TRUE) 
  
  loglik_res <- -solution_res$value
  
  # calculate LR statistic for alpha = 0
  simulations$LR[n] <- -2*(loglik_res - loglik_unres)
  }

epdfPlot(simulations$LR)       # plot empirical density
quantile(simulations$LR, 0.95) # find empirical 95% quantile
