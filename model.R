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
data <- readxl::read_excel("data/USBanks.xlsx") %>%
  clean_names() %>%
  mutate(date <- as.Date(period)) %>%
  select(period, bac_us_equity, jpm_us_equity, wfc_us_equity, c_us_equity)

#############################################################################
### specify initial options and parameter values ###
#############################################################################
T <- nrow(data) # length of time series
T_start = 1511  # start of time series: 3 Jan 2006
T_end   = 4531  # end of time series

p <- 3          # of variables
n <- 3          # of time-varying eigenvalues

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
lb <- c(rep(0,p*(p-1)/2), 
       rep(0,p), 
       rep(0,n*p), 
       rep(0,n*p), 
       rep(0,(p-n)*n))
  
ub <- c(rep(pi/2, p*(p-1)/2), 
        rep(100, p), 
        rep(100, n*p), 
        rep(100, n*p),
        rep(0, (p-n)*n))

