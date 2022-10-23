#############################################################################
### load libraries and settings ###
#############################################################################
library(tidyverse)
library(tictoc)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())
set.seed(2022)

# import functions
source('R/utils/estimationFunctionsExtended.R')

#############################################################################
### define sample period ###
#############################################################################
estimation_start <- as.Date('2010-01-01')
estimation_end <- as.Date('2018-12-31')

#############################################################################
### load data ###
#############################################################################
# choose covariate number [10=bcom, 16=oil, 17=10yr, 18=usd]
covar <- 18
p <- n <- 5

# import data
data <- readxl::read_excel('data/13102022_data.xlsx', 
                           sheet = 'DATA_CLEAN') %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= estimation_start)

# transform to matrix
datamat <- data %>% filter(Date <= estimation_end) %>%
  select(-Date) %>% as.matrix() %>% t()

# split in two and concatenate row-wise
x <- datamat[1:p,]
z <- datamat[(covar-1),]
X <- rbind(x,z)

# fetch full series for out of sample estimation
data_full <- data %>% select(-Date) %>% as.matrix() %>% t()
x_full <- data_full[1:p,]
z_full <- data_full[(covar-1),]

N <- ncol(x_full)
row_end <- which(data$Date == as.Date(estimation_end))

#############################################################################
### fetch parameters ###
#############################################################################

# from matlab optimizer
theta <- readxl::read_excel('MATLAB/estimates/theta5.xlsx',
                            col_names = FALSE) %>%
  as.matrix()

#Lambda <- readxl::read_excel("data/condEigenvalues.xlsx") %>% as.matrix()
L <- EigenARCH_loglikelihood(theta)
loglik <- EigenARCH_loglikelihood_cont(X, theta, n, p)
Lambda <- loglik$lambda %>% t()
colnames(Lambda) <- c('lambda1','lambda2','lambda3','lambda4','lambda5')

# Reparametrization of theta, save in individual matrices
parameters <- EigenARCH_repar(p, n, theta)
V <- parameters$eigenvectors
W <- parameters$omega
A <- parameters$alpha
B <- parameters$beta
C <- parameters$psi

#############################################################################
### create dataframes of conditional eigenvalues and covariance matrices ###
#############################################################################

# retrieve estimated lambdas
condEigenvals <- data %>% select(Date) %>% 
  filter(Date <= estimation_end) %>% 
  cbind(Lambda %>% round(10))

# estimate lambdas out of sample
for(t in (row_end+1):(ncol(x_full))){

  # set date
  condEigenvals[t,1] <- data[t,1]
  
  # calculate conditional eigenvalues
  lambda_lag <- condEigenvals[t-1,] %>% select(-Date) %>% as.matrix() %>% t()
  lambda_t <- W + A %*% (t(V) %*% x_full[,t-1])^2 + B %*% lambda_lag + C*z_full[t-1]^2
  lambda_t <- lambda_t %>% t()

  # save in tibble
  condEigenvals[t,2:(2+p-1)] <- lambda_t
}

# estimate Omegas out of sample
condCovariances <- lapply(1:ncol(x_full), 
                          function(t) {
                            V %*% diag(condEigenvals[t,2:(2+p-1)]) %*% t(V)
                                      }
                          ) 

names(condCovariances) <- data$Date

# calculate sample covariance matrix
sampleCovariance <- cov(t(x))
