source('r/utils/getWeights.R')
library(tidyverse)
library(lubridate)

## THIS FUNCTION CALCULATES OPTIMAL PORTFOLIO WEIGHTS AND RETURNS
getPortfolios <- function(model = 1,
                          trading_start = '2019-01-02', 
                          trading_end = '2022-06-30'){
  
  #############################################################################
  ### Define the dataframes to roll over ###
  #############################################################################
  df <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>% 
    mutate(Date = as.Date(Date)) %>%
    mutate_at(vars(-Date), ~./100)
  
  # define path based on model selection and read in the file
  path_conditionalDynamics <- paste0('data/conditionalDynamics',model,'.rds')
  path_params <- paste0('MATLAB/estimates/theta',model,'_constant.xlsx')
  condDynamics <- readRDS(path_conditionalDynamics)
  condCovariances <- condDynamics$condCovar
  mu <- readxl::read_excel(path_params, col_names = FALSE) %>% tail(5) %>%
    deframe()
  
  # split data into regions and index returns
  data <- df %>% 
    select(Date, Africa, Asia, Europe, 'Latin America', 'Middle East') %>%
    filter(Date >= as.Date(trading_start))
  
  benchmark <- df %>% select(Date, 'EMBIG Div') %>% 
    filter(Date >= as.Date(trading_start))
  
  # define trading window
  row_start <- which(data$Date == as.Date(trading_start))
  row_end <- which(data$Date == as.Date(trading_end))
  
  # define sample 
  x_trading <- data[row_start:row_end,]
  trading_length <- nrow(x_trading)
  
  #############################################################################
  ### portfolio optimization ###
  #############################################################################
 
  # indicate first trading day of month (= re-balance day)
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
  
  colnames(trading) <- c("Africa", "Asia", "Europe", "Latin America", "Middle East", "Return")
  
  # initialize list of tibbles for each strategy
  trading <- list(
    "EW" = trading,
    "MVP" = trading,
    "EFF" = trading,
    "EFF_TC" = trading
  )
  
  # initialize moments
  Omega_init <- condCovariances[[trading_start]]
  
  mu_init <- mu
  
  # initialize portfolios (note: no cost to buy the first portfolio => beta = 0)
  trading$EW[1,1:5]  <- equalWeights(n = 5)
  trading$MVP[1,1:5] <- minimumVarWeights(Omega_init)
  trading$EFF[1,1:5] <- efficientWeights_costOpt(Omega_init, mu_init, gamma = 4, w_init = equalWeights(n = 5), beta = 0.00)
  trading$EFF_TC[1,1:5] <- efficientWeights_costOpt(Omega_init, mu_init, gamma = 4, w_init = equalWeights(n = 5), beta = 0.00)
  
  # initialize portfolio returns
  trading$EW[1,6]  <- trading$EW[1,1:5] %*% t(as.matrix(x_trading[1,2:6]))
  trading$MVP[1,6] <- trading$MVP[1,1:5] %*% t(as.matrix(x_trading[1,2:6]))
  trading$EFF[1,6] <- trading$EFF[1,1:5] %*% t(as.matrix(x_trading[1,2:6]))
  trading$EFF_TC[1,6] <- trading$EFF_TC[1,1:5] %*% t(as.matrix(x_trading[1,2:6]))
  
  # generate portfolio weights and returns
  for(t in 2:trading_length){
    
    # CASE 1: FIRST TRADING DAY OF MONTH => REBALANCE PORTFOLIO
    if(x_trading$rebalance[t] == 1){
      
      rebalance_date <- as.character(x_trading$Date[t])
      
      # DEFINE THE CONDITIONAL DISTRIBUTION
      
      # Omega_t from MGARCH 
      Omega <- condCovariances[[rebalance_date]]
      
      # re-balance portfolio weights (beta = 50 bps)
      trading$EW[t,1:5]  <- equalWeights(n = 5)
      trading$MVP[t,1:5] <- minimumVarWeights(Omega)
      trading$EFF[t,1:5] <- efficientWeights_costOpt(Omega, mu, gamma = 4, w_init = trading$EFF[t-1,1:5], beta = 0.00)
      trading$EFF_TC[t,1:5] <- efficientWeights_costOpt(Omega, mu, gamma = 4, w_init = trading$EFF_TC[t-1,1:5], beta = 0.5)
      
      # portfolio returns (w, w_prev, next_return, beta = 0.05)
      trading$EW[t,6]  <- evaluate_performance(w = w_ew, w_prev = trading$EW[t-1,1:5], next_return = x_trading[t,2:6], beta = 0.05)
      trading$MVP[t,6] <- evaluate_performance(w = w_mvp, w_prev = trading$MVP[t-1,1:5], next_return = x_trading[t,2:6], beta = 0.05)
      trading$EFF[t,6] <- evaluate_performance(w = w_eff, w_prev = trading$EFF[t-1,1:5], next_return = x_trading[t,2:6], beta = 0.05)
      trading$EFF_TC[t,6] <- evaluate_performance(w = w_eff_tc, w_prev = trading$EFF_TC[t-1,1:5], next_return = x_trading[t,2:6], beta = 0.05)
      
    } else {
      
      # CASE 2: NOT FIRST TRADING DAY OF MONTH => KEEP PORTFOLIO
      
      # 1) equal weighted  
      w_ew <- trading$EW[t-1,1:5] * (1 + x_trading[t-1,2:6])
      w_ew <- as.numeric(w_ew / sum(as.matrix(w_ew)))
      return_ew <- w_ew %*% t(as.matrix(x_trading[t-1,2:6]))
      trading$EW[t,] <- c(w_ew, return_ew)
      
      # minimum variance
      w_mvp <- trading$MVP[t-1,1:5] * (1 + x_trading[t-1,2:6])
      w_mvp <- as.numeric(w_mvp / sum(as.matrix(w_mvp)))
      return_mvp <- w_mvp %*% t(as.matrix(x_trading[t-1,2:6]))
      trading$MVP[t,] <- c(w_mvp, return_mvp)
      
      # efficient tangent
      w_eff <- trading$EFF[t-1,1:5] * (1 + x_trading[t-1,2:6])
      w_eff <- as.numeric(w_eff / sum(as.matrix(w_eff)))
      return_eff <- w_eff %*% t(as.matrix(x_trading[t-1,2:6]))
      trading$EFF[t,] <- c(w_eff, return_eff)
      
      # efficient tangent net TC
      w_eff_tc <- trading$EFF_TC[t-1,1:5] * (1 + x_trading[t-1,2:6])
      w_eff_tc <- as.numeric(w_eff_tc / sum(as.matrix(w_eff_tc)))
      return_eff_ntc <- w_eff_tc %*% t(as.matrix(x_trading[t-1,2:6]))
      trading$EFF_TC[t,] <- c(w_eff_tc, return_eff_ntc)
      
    }
  }
  
  return(trading)
}







