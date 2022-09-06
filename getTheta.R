# load libraries
library(tidyverse)
library(matlib)
library(tictoc)
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
  select(-IG, -HY) %>%
  filter(between(Date, as.Date('2000-01-04'), as.Date('2005-12-31')))

x <- data %>% select(-Date) %>% as.matrix() %>% t()
p <- n <- nrow(x) 

#############################################################################
### estimate parameters ###
#############################################################################
theta0 <- rep(0.05, p*(p-1)/2+p+2*n*p+(p-n)*n)
pars <- length(theta0)

ub = c(pi/2*rep(1,p*(p-1)/2),
       100*rep(1,p),
       100*rep(1,n*p),
       100*rep(1,n*p))


hin <- function(x) {
  # set lower bound (zero for all parameters)
  h <- x
  
  # set upper bound (pi/2)
  for(par in 1:(p*(p-1)/2)){
    h[pars + par] <- pi/2 - x[par]
  }
  
  # set upper bound (100)
  for(par in (p*(p-1)/2+1):pars){
    h[pars + par] <- 100 - x[par]
  }
  h
}

tic('alabama optimizer')
ans <- constrOptim.nl(par=theta0, 
                      fn=EigenARCH_loglikelihood, 
                      hin=hin) 
toc()


