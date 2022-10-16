# load libraries
library(tidyverse)
library(matlib)
library(tictoc)
library(quadprog)
library(scales)
library(alabama)
library(lubridate)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

source('getOmegas.R')
#condCovariances <- readRDS(file = "condVariances.rds")
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

# define benchmark return
benchmark <- readxl::read_excel('data/07092022_embig_data.xlsx', sheet = 'Returns') %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, EMBIG) %>%
  filter(Date >= '2019-01-02') # NB!

# indicate first trading day of month
x_trading <- x_trading %>%
  mutate(rebalance = case_when(
            month(Date) > month(lag(Date)) ~ 1,
            month(Date) <= month(lag(Date)) ~ 0,
            TRUE ~ 0
           )) 

# initialize tibble
trading <- matrix(NA,
                  nrow = trading_length,
                  ncol = 6)

colnames(trading) <- c("africa", "asia", "europe", "mideast", "latam", "return")

trading <- list(
  "EW" = trading,
  "MVP" = trading,
  "TAN" = trading,
  "NTC" = trading
)

# initialize portfolio weights
Omega_int <- condCovariances[[trading_start]]
mu <- rep(as.matrix(benchmark[1,2]/100), 5)

trading$EW[1,1:5]  <- equalWeights(n = 5)
trading$MVP[1,1:5] <- minimumVarWeights(Omega_int)
trading$TAN[1,1:5] <- tangentWeights(Omega_int, mu = mu, gamma = 4)
trading$NTC[1,1:5] <- tangentNTCWeights(Omega_int, mu = mu, gamma = 4, beta = 50)

# initialize portfolio returns
trading$EW[1,6]  <- trading$EW[1,1:5] %*% t(as.matrix(x_trading[1,2:6]/100))
trading$MVP[1,6] <- trading$MVP[1,1:5] %*% t(as.matrix(x_trading[1,2:6]/100))
trading$TAN[1,6] <- trading$TAN[1,1:5] %*% t(as.matrix(x_trading[1,2:6]/100))
trading$NTC[1,6] <- trading$NTC[1,1:5] %*% t(as.matrix(x_trading[1,2:6]/100))

count <- 0
# generate portfolio weights and returns
for(t in 2:trading_length){
  
  if(x_trading$rebalance[t] == 1){
    rebalance_date <- which(data$Date == as.Date(x_trading$Date[t]))
    Omega_int <- condCovariances[[rebalance_date]]
    mu <- rep(as.matrix(benchmark[t,2]/100), 5) # required return is the benchmark return
    
    # rebalance portfolio weights
    w_ew <- equalWeights(n = 5)
    w_mvp <- minimumVarWeights(Omega_int)
    w_tan <- tangentWeights(Omega_int, mu = mu, gamma = 4)
    w_ntc <- tangentNTCWeights(Omega_int, mu = mu, gamma = 4, beta = 50)
    
    # store weights in tibble
    trading$EW[t,1:5]  <- w_ew
    trading$MVP[t,1:5] <- w_mvp
    trading$TAN[t,1:5] <- w_tan
    trading$NTC[t,1:5] <- w_ntc
    
    # portfolio returns
    trading$EW[t,6]  <- evaluate_performance(w = w_ew, w_previous = trading$EW[t-1,1:5], next_return = x_trading[t,2:6]/100)
    trading$MVP[t,6] <- evaluate_performance(w = w_mvp, w_previous = trading$MVP[t-1,1:5], next_return = x_trading[t,2:6]/100)
    trading$TAN[t,6] <- evaluate_performance(w = w_tan, w_previous = trading$TAN[t-1,1:5], next_return = x_trading[t,2:6]/100)
    trading$NTC[t,6] <- evaluate_performance(w = w_ntc, w_previous = trading$NTC[t-1,1:5], next_return = x_trading[t,2:6]/100)
    
    count <- count + 1
    
  } else {
    
    # equal weighted  
    w_ew <- trading$EW[t-1,1:5] * (1 + x_trading[t-1,2:6]/100)
    w_ew <- as.numeric(w_ew / sum(as.matrix(w_ew)))
    return_ew <- w_ew %*% t(as.matrix(x_trading[t-1,2:6]/100))
    trading$EW[t,] <- c(w_ew, return_ew)
    
    # minimum variance
    w_mvp <- trading$MVP[t-1,1:5] * (1 + x_trading[t-1,2:6]/100)
    w_mvp <- as.numeric(w_mvp / sum(as.matrix(w_mvp)))
    return_mvp <- w_mvp %*% t(as.matrix(x_trading[t-1,2:6]/100))
    trading$MVP[t,] <- c(w_mvp, return_mvp)
    
    # efficient tangent
    w_tan <- trading$TAN[t-1,1:5] * (1 + x_trading[t-1,2:6]/100)
    w_tan <- as.numeric(w_tan / sum(as.matrix(w_tan)))
    return_tan <- w_tan %*% t(as.matrix(x_trading[t-1,2:6]/100))
    trading$TAN[t,] <- c(w_tan, return_tan)
    
    # efficient tangent net TC
    w_ntc <- trading$NTC[t-1,1:5] * (1 + x_trading[t-1,2:6]/100)
    w_ntc <- as.numeric(w_ntc / sum(as.matrix(w_ntc)))
    return_ntc <- w_ntc %*% t(as.matrix(x_trading[t-1,2:6]/100))
    trading$NTC[t,] <- c(w_ntc, return_ntc)
    
  }
}

