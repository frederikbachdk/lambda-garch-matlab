library(tidyverse)

model = 5
trading_start = '2019-01-02'
trading_end = '2022-06-30'

path_conditionalDynamics <- paste0('data/conditionalDynamics',model,'.rds')
condDynamics <- readRDS(file = "data/conditionalDynamics1.rds")
condCovariances <- condDynamics$condCovar

df <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>% 
  mutate(Date = as.Date(Date))

## calculate the optimal weights based on the 30-06-2022 covariance matrix
Omega <- condCovariances[['2022-06-30']]

# 1) Equal weights
w_equal <- rep(1/5, 5)

# 2) Minimum variance
iota <- rep(1, ncol(Omega))
Omega_inv <- solve(Omega)
w_mvp <- (Omega_inv %*% iota) / sum(Omega_inv %*% iota)

# 3) Efficient