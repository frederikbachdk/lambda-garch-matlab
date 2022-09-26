## This script simulates the lambda GARCH(X) for different specifications
library(MASS)
library(tidyverse)
library(tictoc)
library(pracma)

# turn off scinumbers, clear console and memory
options(scipen=999)
cat('\014')
rm(list=ls())
set.seed(187)

source('utils/functions.R')
source('utils/matrix_functions.R')

#############################################################################
### Define the true model parameters ###
#############################################################################
p <- n <- 2    # dimensions of the lambda-GARCH

mu <- rep(0,p)          # first moment of eta
Sigma <- diag(rep(1,p)) # second moment of eta

# Define parameters
phi <- 0.323
V <- matrix(c(cos(phi), -sin(phi), 
              sin(phi), cos(phi)), 
              nrow=p, ncol=p)  # Rotation matrix
W <- matrix(c(0.08, 0.03), nrow=p, ncol=1)
A <- matrix(c(0.03, 0.03, 0.003, 0.03), nrow=p, ncol=p)      # ARCH
B <- matrix(c(0.7, 0.5, 0.35, 0.27), nrow=p, ncol=p)         # GARCH
#G <- matrix(c(0.88, 0.12, 6.08e-10, 0.5), nrow=2, ncol=2)   # GARCH-X
max(abs(eigen(A+B)$values))                                  # Stationarity condition

#############################################################################
### simulate 1000 stationary lambda-GARCH processes ###
#############################################################################
N <- 2000  # length of time series
M <- 1000 # number of simulations
x1sims <- matrix(NA, N+1, M)
x2sims <- matrix(NA, N+1, M)
tic('simulation')


for(sim in 1:M){
  
  # initialize tibble for simulation
  GARCHsim <- tibble(t = seq(0,N),
                     eta1 = rnorm(N+1,0,1),
                     eta2 = rnorm(N+1,0,1),
                     X1 = 0,
                     X2 = 0,
                     lambda1 = 0,
                     lambda2 = 0) %>% as.matrix()
  
  # simulate dynamic evolution of X_t
  for(t in 2:N+1){
    
    # calculate conditional eigenvalues, lambda_t
    GARCHsim[t,6:7] <- t(W + A %*% GARCHsim[t-1,4:5] + B %*% GARCHsim[t-1,6:7])
    
    # calculate realization X_t = V*Lambda^(1/2)*eta_t
    GARCHsim[t,4:5] <- t(V %*% diag(as.vector(sqrt(GARCHsim[t,6:7]))) %*% GARCHsim[t, 2:3])
    
  }
  
  # store X1 and X2
  x1sims[,sim] <- GARCHsim[,4]
  x2sims[,sim] <- GARCHsim[,5]
  
  print(sim)
  
}

toc()

x1sims_test <- x1sims %>% as_tibble() %>% mutate(t = seq(0,N)) %>% 
  select(t, everything()) %>% 
  pivot_longer(cols='V1':'V100', names_to='sim',values_to='ret')


x1sims_test %>% ggplot() + 
  aes(x = t, y = ret, color = sim) + 
  geom_line() +
  theme(legend.position='none')

#############################################################################
### estimate parameters of the 1000 stationary lambda-GARCH processes ###
#############################################################################


# set optimizer bounds for constrained optimization
theta0 <- rep(0.05, p*(p-1)/2+p+2*n*p+(p-n)*n)
lb <- c(rep(1e-10,p*(p-1)/2), 
        rep(1e-10,p), 
        rep(1e-10,n*p), 
        rep(1e-10,n*p), 
        rep(1e-10,(p-n)*n))

ub <- c(rep(pi/2, p*(p-1)/2), 
        rep(100, p), 
        rep(100, n*p), 
        rep(100, n*p),
        rep(0, (p-n)*n))


# estimate model parameters
# x <- GARCHsim[,4:5] %>% t()
# 
# # solution
# print('Model: Simulated λ-GARCH. Maximizing the log-likelihood function... ')
# tic('optimization procedure')
# solution <- fmincon(theta0, EigenARCH_loglikelihood,
#                     lb = lb,
#                     ub = ub,
#                     tol = 1e-12, 
#                     maxfeval = 3*10e4, 
#                     maxiter = 10e5)
# toc()
# print('maximization complete')

GARCHsim %>% select(t, X1) %>% ggplot() + aes(x = t, y = X1) + geom_line()
#GARCHsim %>% select(t, eta1) %>% ggplot() + aes(x = t, y = eta1) + geom_line()

#############################################################################
### simulate and estimate 1000 standard stationary lambda-GARCH thetas ###
#############################################################################
