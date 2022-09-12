## This script simulates the lambda GARCH(X) for different specifications
library(MASS)
library(tidyverse)

# turn off scinumbers, clear console and memory
options(scipen=999)
cat('\014')
rm(list=ls())
set.seed(2022)

source('utils/matrix_functions.R')

#############################################################################
### Define the true model parameters ###
#############################################################################
p <- 2    # dimensions of the lambda-GARCH
N <- 5000 # length of time series

mu <- rep(0,p)          # first moment of eta
Sigma <- diag(rep(1,p)) # second moment of eta

# Define parameters
phi <- 0.323
V <- matrix(c(cos(phi), -sin(phi), 
              sin(phi), cos(phi)), 
              nrow=p, ncol=p)  # Rotation matrix
W <- matrix(c(0.08, 0.03), nrow=p, ncol=1)
A <- matrix(c(0.02, 0.02, 0.02, 0.02), nrow=p, ncol=p)       # ARCH
B <- matrix(c(0.7, 0.5, 0.35, 0.27), nrow=p, ncol=p)        # GARCH
max(abs(eigen(A+B)$values))                                  # Stationarity condition

#G <- matrix(c(0.88, 0.12, 6.08e-10, 0.5), nrow=2, ncol=2)       # GARCH-X


#############################################################################
### simulate a standard stationary lambda-GARCH ###
#############################################################################

# initialize tibble for simulation
GARCHsim <- tibble(t = seq(0,N)) %>% 
  cbind(mvrnorm(n = N+1, mu, Sigma)) %>% # draw iid from N(0,I_p)
  rename(t = 1,
         "eta1" = 2,
         "eta2" = 3) %>%
  mutate(X1 = 0,
         X2 = 0,
         lambda1 = 0,
         lambda2 = 0)

GARCHsim$eta1[1] <- GARCHsim$eta2[1] <- 0

# simulate dynamic evolution of X_t
for(t in 2:N+1){
  
  # calculate conditional eigenvalues, lambda_t
  GARCHsim[t,6:7] <- W + A %*% t(GARCHsim[t-1,4:5]^2) + B %*% t(GARCHsim[t-1,6:7])
  
  # calculate realization X_t = V*Lambda^(1/2)*eta_t
  GARCHsim[t,4:5] <- V %*% diag(as.vector(sqrt(GARCHsim[t,6:7]))) %*% t(GARCHsim[t, 2:3])
}


GARCHsim %>% select(t, X2) %>% ggplot() + aes(x = t, y = X2) + geom_line()
