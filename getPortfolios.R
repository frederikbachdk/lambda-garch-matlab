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
# define initial wealth
wealth_initial <- 100

# define trading window and sample
trading_start <- which(data$Date == as.Date('2019-01-02'))
trading_end <- which(data$Date == as.Date('2022-06-30'))
x_trading <- data[trading_start:trading_end,]
trading_length <- nrow(x_trading)

# initialize return tibble
returns <- x_trading %>% select(Date) %>%
  mutate(portfolio = NA,
         weight_1 = NA,
         weight_2 = NA,
         weight_3 = NA,
         return = NA)

# initial portfolio
returns[1,2] <- 'naive'
returns[1,3:5] <- 1/3
returns[1,6] <- x_trading[1,2:4] %>% as.matrix() %*% c(1/3, 1/3, 1/3)

# loop over trading window
for(t in 2:trading_length){
  
  # naive portfolio
  returns[t,2] <- 'naive'
  
  diffused_weight <- (returns[t-1,3:5] %>% as.matrix()) *  
    (1 + x_trading[t-1,2:4] %>% as.matrix())             # diffused weights
  
    #for(asset in 1:3){
    #  if(diffused_weight[asset] <= 0) diffused_weight[asset] = 0 # ensure no short selling
    #}
  
  returns[t,3:5] <- diffused_weight/sum(diffused_weight) # normalize diffuse weights
  
  returns[t, 6] <-  (x_trading[t,2:4] %>% as.matrix()) %*% 
                    (returns[t,3:5] %>% as.matrix() %>% t()) # return
  
  print(t)
  
}


for(asset in 1:3){
  if(diffused_weight[asset] <= 0) diffused_weight[asset] = 0 # ensure no short selling
}
