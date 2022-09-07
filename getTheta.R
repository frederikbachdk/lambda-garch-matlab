# load libraries
library(tidyverse)
library(matlib)
library(tictoc)
library(pracma)
library(alabama)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())
set.seed(2022)

# import functions
source('utils/functions.R')
source('utils/matrix_functions.R')

#############################################################################
### load data ###
#############################################################################
data <- readxl::read_excel('data/12072022_embig_data.xlsx', sheet = 'Returns') %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, IG, HY) %>%
  filter(between(Date, as.Date('2010-01-04'), as.Date('2016-12-31')))

x <- data %>% select(-Date) %>% as.matrix() %>% t()
p <- n <- nrow(x) 

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
solution <- fmincon(theta0, EigenARCH_loglikelihood,
                    lb = lb,
                    ub = ub,
                    tol = 1e-8, 
                    maxfeval = 3*10e4, 
                    maxiter = 10e5)

pars <- EigenARCH_repar(p,n,solution$par)
