# load libraries
library(tidyverse)
library(matlib)
library(tictoc)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

source('getOmegas.R')

#############################################################################
### portfolio optimization ###
#############################################################################

# start portfolio optimization at 2017-01-02 with 100 USD wealth
w_initial <- 100

# define risk free rate etc.. to do - maybe USTs
iota <- rep(1,p)

Omega <- condCovariances[[4433]]
minimumVariance <- (inv(Omega) %*% iota)/sum(inv(Omega) %*% iota)

# calculate rolling returns incl. drift adjustment in weight vector
portfolioEvaluation <- tibble(
  date = as.Date(NA),
  Africa_w = NA,
  Asia_w = NA,
  Europe_w = NA,
  LatAm_w = NA,
  MiddleEast_w = NA
)

portfolioEvaluation$date[1] <- data$Date[4435]
portfolioEvaluation[1,2:6] <- t(minimumVariance)

for(t in 2:1432){
  date <- data$Date[(t+4432)]
  weights <- portfolioEvaluation[(t-1),2:6]*(rep(1,5) + x[t+4432,]/100)
  update <- cbind(date, weights)
  portfolioEvaluation <- rbind(portfolioEvaluation, update)
}

