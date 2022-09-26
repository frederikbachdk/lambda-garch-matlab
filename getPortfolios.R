# load libraries
library(tidyverse)
library(matlib)
library(tictoc)
library(quadprog)
library(scales)
library(alabama)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

source('getOmegas.R')
source('getWeights.R')

#############################################################################
### portfolio optimization ###
#############################################################################
# define initial wealth
wealth_initial <- 100

# define trading window
trading_start <- which(data$Date == as.Date('2019-01-02'))
trading_end <- which(data$Date == as.Date('2022-06-30'))

# define sample 
x_trading <- data[trading_start:trading_end,]
trading_length <- nrow(x_trading)

# initialize tibble
trading <- matrix(NA,
                  nrow = trading_length,
                  ncol = 6)

colnames(trading) <- c("africa", "asia", "europe", "mideast", "latam", "return")

trading <- list(
  "EW" = trading,
  "MVP" = trading,
  "TAN" = trading
)

# initialize portfolio weights
trading$EW[1,1:5] <- 1/5
trading$MVP[1,1:5] <- minimumVarWeights(condCovariances[['2019-01-02']], n)
trading$TAN[1,1:5] <- 1/5 # update later

# initialize portfolio returns
trading$EW[1,6] <- trading$EW[1,1:5] %*% t(as.matrix(x_trading[1,2:6]/100))
trading$MVP[1,6] <- trading$MVP[1,1:5] %*% t(as.matrix(x_trading[1,2:6]/100))
trading$TAN[1,6] <- trading$TAN[1,1:5] %*% t(as.matrix(x_trading[1,2:6]/100))


for(t in 2:trading_length){
  
  # equal weighted  
  w_ew <- trading$EW[t-1,1:5] * (1 + x_trading[t-1,2:6]/100)
  w_ew <- w_ew/sum(w_ew)
  ret_ew <- as.matrix(w_ew) %*% t(as.matrix(x_trading[t-1,2:6]/100))
  trading$EW[t, 1:6] <- cbind(w_ew, ret_ew)
}

