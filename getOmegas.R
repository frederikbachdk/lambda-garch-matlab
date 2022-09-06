# load libraries
library(tidyverse)
library(matlib)
library(tictoc)
library(alabama)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

# import functions
source('utils/functions_unconstrained.R')
source('utils/matrix_functions.R')

#############################################################################
### load data and estimated parameters ###
#############################################################################
data <- readxl::read_excel('data/12072022_embig_data.xlsx', sheet = 'Returns') %>%
  mutate(Date = as.Date(Date)) %>%
  select(-IG, -HY)

x <- data %>% select(-Date) %>% as.matrix()
p <- n <- ncol(x)

Lambda <- readxl::read_excel("matlab/condEigenvalues.xlsx") %>% as.matrix()
colnames(Lambda) <- c('lambda1','lambda2','lambda3','lambda4','lambda5')

# QMLE parameters from MATLAB
theta <- readxl::read_excel("matlab/theta_unrestricted.xlsx") %>% as.matrix() %>% t()

# Reparametrization of theta, save in individual matrices
parameters <- EigenARCH_repar_unconstrained(p, n, theta)
V <- parameters$eigenvectors
W <- parameters$omega
A <- parameters$alpha
B <- parameters$beta

#############################################################################
### create dataframes of conditional eigenvalues and covariance matrices ###
#############################################################################

# retrieve estimated lambdas
condEigenvals <- tibble(
  date = data$Date[1:4434]
) %>% cbind(Lambda %>% round(10))

# estimate lambdas out of sample
for(t in 4435:5868){

  # set date
  condEigenvals[t,1] <- data[t,1]
  
  # calculate conditional eigenvalues
  lambda_lag <- condEigenvals[t-1,] %>% select(-date) %>% as.matrix() %>% t()
  lambda_t <- W + A %*% (t(V) %*% x[t-1,])^2 + B %*% lambda_lag
  lambda_t <- lambda_t %>% t()

  # save in tibble
  condEigenvals[t,2:6] <- lambda_t
}

# estimate Omegas out of sample
condCovariances <- lapply(1:5868, function(t) V %*% diag(condEigenvals[t,2:6]) %*% inv(V)) 
names(condCovariances) <- data$Date

rm(list=setdiff(ls(), c('data','condCovariances','condEigenvals','p','n', 'x')))
