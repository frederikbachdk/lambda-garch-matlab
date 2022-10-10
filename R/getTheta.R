#############################################################################
### load libraries and settings ###
#############################################################################
library(tidyverse)
library(matlib)
library(tictoc)
library(pracma)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())
set.seed(2022)

# import functions
source('utils/functions.R')
source('utils/matrix_functions.R')

#############################################################################
### define sample period ###
#############################################################################
estimation_start <- as.Date('2010-01-01')
estimation_end <- as.Date('2018-12-31')

#############################################################################
### load data ###
#############################################################################

data <- readxl::read_excel('data/07092022_embig_data.xlsx', sheet = 'Returns') %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Africa, Asia, Europe, 'Middle East', 'Latin America') %>%
  filter(Date >= estimation_start)

x <- data %>% filter(Date <= estimation_end) %>%
  select(-Date) %>% as.matrix() %>% t()

x_full <- data %>% select(-Date) %>% as.matrix() %>% t()
N <- ncol(x_full)
p <- n <- nrow(x)
row_end <- which(data$Date == as.Date(estimation_end))

#############################################################################
### estimate parameters ###
#############################################################################
theta0 <- rep(0.05, p*(p-1)/2+p+2*n*p+(p-n)*n)

# set optimizer bounds for constrained optimization
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

# solution
print('Model: λ-GARCH for the sample period: 2010-01-01 until 2018-12-31')
print('.............................................')
print('maximizing the log-likelihood function.......')
tic('optimization procedure')
solution <- fmincon(theta0, EigenARCH_loglikelihood,
                    lb = lb,
                    ub = ub,
                    tol = 1e-12, 
                    maxfeval = 3*10e4, 
                    maxiter = 10e5)

toc()
print('maximization complete')

# save theta vector
theta <- solution$par

