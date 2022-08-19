# load libraries
library(tidyverse)
library(janitor)
library(pracma)
library(stats)

# clear console and memory
cat("\014")
rm(list=ls())

# load functions
source("utils/functions.R")

#############################################################################
### load data ###
#############################################################################
# data <- readxl::read_excel("data/USBanks.xlsx") %>%
#   clean_names() %>%
#   mutate(date <- as.Date(period)) %>%
#   select(period, bac_us_equity, jpm_us_equity, wfc_us_equity, c_us_equity)

data <- readxl::read_excel("data/12072022_embig_data.xlsx", sheet = 'Returns') %>%
  mutate(Date = as.Date(Date)) %>%
  select(-IG, -HY)

#############################################################################
### specify initial options and parameter values ###
#############################################################################
N <- nrow(data) # length of time series
#T_start = 1511  # start of time series: 3 Jan 2006
#T_end   = 4531  # end of time series

p <- 5          # of variables
n <- 5          # of time-varying eigenvalues

#x <- data[T_start:T_end, 2:(1+p)] %>% t()
x <- data[T_start:T_end, 2:(1+p)] %>% t()
  
#############################################################################
### initial likelihood ###
#############################################################################

# initial parameter vector for optimization
theta0 = 0.05*rep(1,(p*(p-1)/2+p+2*n*p+(p-n)*n)) 

# calculate likelihood based on initial parameter vector
loglikelihood_init <- EigenARCH_loglikelihood(theta0)


#############################################################################
### maximize likelihood ###
#############################################################################

# set optimizer bounds
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

solution <- fmincon(theta0, EigenARCH_loglikelihood,
                    lb = lb,
                    ub = ub,
                    tol = 1e-10,
                    maxiter = 1000000,
                    maxfeval = 300000)

L_con <- -N*solution$value
params <- EigenARCH_repar(p,n,solution$par)

