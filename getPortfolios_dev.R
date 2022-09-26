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

#############################################################################
### portfolio optimization ###
#############################################################################
# define initial wealth
wealth_initial <- 100

# define trading window and sample
trading_start <- which(data$Date == as.Date('2019-01-02'))
trading_end <- which(data$Date == as.Date('2022-06-30'))
x_trading <- data[trading_start:trading_end,]
benchmark <- data[trading_start:trading_end,]
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
  
  #print(t)
  
}


for(asset in 1:3){
  if(diffused_weight[asset] <= 0) diffused_weight[asset] = 0 # ensure no short selling
}

#############################################################################
# minimum variance portfolio
#############################################################################
Omega <- condCovariances[[trading_start]]
w_mvp <- inv(Omega) %*% rep(1, ncol(Omega))
w_mvp <- as.vector(w_mvp/sum(w_mvp))

# initialize return tibble
returns_mvp <- x_trading %>% select(Date) %>%
  mutate(portfolio = NA,
         weight_1 = NA,
         weight_2 = NA,
         weight_3 = NA,
         weight_4 = NA,
         weight_5 = NA,
         weight_sum = NA,
         return = NA)

# initial portfolio
returns_mvp[1,2] <- 'mvp'
returns_mvp[1,3:7] <- w_mvp %>% as_tibble() %>% t()
returns_mvp[1,8] <- sum(returns_mvp[1,3:7])
returns_mvp[1,9] <- x_trading[1,2:6] %>% as.matrix() %*% w_mvp

# define function that adjusts weights
adjust_weights <- function(w, next_return) {
  w_prev <- 1 + w * next_return
  as.numeric(w_prev / sum(as.vector(w_prev)))
}

# loop over trading window
for(t in 2:trading_length){
  
  # minimum variance portfolio
  returns_mvp[t,2] <- 'mvp'
  
  #w_prev <- adjust_weights(returns_mvp[t-1,3:5],x_trading[t-1,2:4])
  w_prev <- 1 + (returns_mvp[t-1,3:7] %>% as.matrix()) * (x_trading[t-1,2:6] %>% as.matrix())

  returns_mvp[t,3:7]  <- w_prev / sum(as.vector(w_prev))
  
  returns_mvp[t,8] <- sum(returns_mvp[t,3:7])
  
  returns_mvp[t,9] <- (x_trading[t,2:6] %>% as.matrix()) %*% 
    (returns_mvp[t,3:7] %>% as.matrix() %>% t()) # return

}
view(returns_mvp)

######################################################################
# efficient portfolio
######################################################################
# function to compute efficient weights
compute_efficient_weight <- function(Omega,
                                     mu,
                                     gamma = 2,
                                     beta = 0, # transaction costs
                                     w_prev = rep(
                                       1 / ncol(Omega),
                                       ncol(Omega)
                                     )) {
  iota <- rep(1, ncol(Omega))
  Omega_processed <- Omega + beta / gamma * diag(ncol(Omega))
  mu_processed <- mu + beta * w_prev
  
  Omega_inverse <- solve(Omega_processed)
  
  w_mvp <- Omega_inverse %*% iota
  w_mvp <- as.vector(w_mvp / sum(w_mvp))
  w_opt <- w_mvp + 1 / gamma *
    (Omega_inverse - w_mvp %*% t(iota) %*% Omega_inverse) %*%
    mu_processed
  return(as.vector(w_opt))
}

Omega <- condCovariances[[trading_start]]
mu <- x_trading %>% select(-Date) %>% colMeans() # required return
w_eff <- compute_efficient_weight(Omega, mu)

# initialize return tibble
returns_eff <- x_trading %>% select(Date) %>%
  mutate(portfolio = NA,
         weight_1 = NA,
         weight_2 = NA,
         weight_3 = NA,
         weight_4 = NA,
         weight_5 = NA,
         weight_sum = NA,
         return = NA)

# initial portfolio
returns_eff[1,2] <- 'efficient'
returns_eff[1,3:7] <- w_eff %>% as_tibble() %>% t()
returns_eff[1,8] <- sum(returns_eff[1,3:7])
returns_eff[1,9] <- x_trading[1,2:6] %>% as.matrix() %*% w_eff


for(t in 2:trading_length){

  # efficient portfolio
  returns_eff[t,2] <- 'efficient'
  
  #w_prev <- adjust_weights(returns_eff[t-1,3:5],x_trading[t-1,2:4])
  w_prev <- 1 + (returns_eff[t-1,3:7] %>% as.matrix()) * (x_trading[t-1,2:6] %>% as.matrix())
  
  returns_eff[t,3:7]  <- w_prev / sum(as.vector(w_prev))
  
  #returns_eff[t,3:5] <- w_prev %>% as_tibble() %>% t()
  
  returns_eff[t,8] <- sum(returns_eff[t,3:7])
  
  returns_eff[t,9] <- (x_trading[t,2:6] %>% as.matrix()) %*% 
    (returns_eff[t,3:7] %>% as.matrix() %>% t()) # return
  
}
view(returns_eff)


######################################################################
# efficient portfolio TC adjusted
######################################################################
compute_efficient_weight_L1_TC <- function(mu,
                                           Omega,
                                           gamma = 2,
                                           beta = 0,
                                           initial_weights = rep(
                                             1 / ncol(Omega),
                                             ncol(Omega)
                                           )) {
  objective <- function(w) {
    -t(w) %*% mu +
      gamma / 2 * t(w) %*% Omega %*% w +
      (beta / 10000) / 2 * sum(abs(w - initial_weights))
  }
  
  w_optimal <- constrOptim.nl(
    par = initial_weights,
    fn = objective,
    heq = function(w) {
      sum(w) - 1
    },
    control.outer = list(trace = FALSE)
  )
  
  return(w_optimal$par)
}


#############################################################################
### optimal allocation of weights ###
#############################################################################
Omega <- condCovariances[[trading_start]]
mu <- benchmark %>% colMeans()
#mu <- x_trading %>% select(-Date) %>% colMeans() * 0
n_regions <- x_trading %>% select(-Date) %>% ncol()

w_mvp_numerical <- solve.QP(
  Dmat = Omega,
  dvec = rep(0, n_regions),
  Amat = cbind(rep(1, n_regions)),
  bvec = 1,
  meq = 1
)

w_efficient_numerical <- solve.QP(
  Dmat = 2 * Omega,
  dvec = mu,
  Amat = cbind(rep(1, n_regions)),
  bvec = 1,
  meq = 1
)
w_efficient_numerical$solution

w_no_short_sale <- solve.QP(
  Dmat = 2 * Omega,
  dvec = mu,
  Amat = cbind(1, diag(n_regions)),
  bvec = c(1, rep(0, n_regions)),
  meq = 1
)
w_no_short_sale$solution


# store portfolio results in tibble
tibble(
  'Naive' = c(1/5, 1/5, 1/5, 1/5, 1/5),
  'No Short-Sale' = w_no_short_sale$solution,
  'MVP' = w_mvp_numerical$solution,
  'Efficient' = compute_efficient_weight(Omega, mu, gamma = 4, beta = 50),
  'Efficient (TC)' = compute_efficient_weight_L1_TC(mu, Omega, gamma = 4, beta = 50),
  Region = x_trading %>% select(-Date) %>% colnames()
) %>%
  pivot_longer(-Region,
               names_to = "strategy",
               values_to = "weights"
               ) %>%
  ggplot(aes(
    fill = strategy,
    y = weights,
    x = Region
  )) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(y = "Allocation weight", fill = NULL) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  theme(legend.position = "bottom")


#############################################################################
### rolling window portfolio optimization ###
#############################################################################
window_length <- 30
trading_length <- nrow(x_trading) - window_length

# define TC parameter and risk aversion
beta <- 50
gamma <- 2

# store portfolio performance
# performance_values <- matrix(NA,
#                              nrow = trading_length,
#                              ncol = 4
# )
# colnames(performance_values) <- c("date", "raw_return", "turnover", "net_return")
performance_values <- matrix(NA,
                             nrow = trading_length,
                             ncol = 3
)
colnames(performance_values) <- c("raw_return", "turnover", "net_return")

performance_values <- list(
  "Naive" = performance_values,
  "Efficient" = performance_values,
  "Efficient (TC)" = performance_values
)

# store portfolio weights
performance_weights <- x_trading %>% select(Date) %>%
  mutate(Europe = NA,
         'Middle East' = NA,
         Africa = NA,
         Asia = NA,
         'Latin America' = NA) %>% as.matrix()

performance_weights <- list(
  "Naive" = performance_weights,
  "Efficient" = performance_weights,
  "Efficient (TC)" = performance_weights
)

w_prev_1 <- w_prev_2 <- w_prev_3 <- rep(1 / n_regions, n_regions)


adjust_weights <- function(w, next_return) {
  w_prev <- 1 + w * next_return
  as.numeric(w_prev / sum(as.vector(w_prev)))
}


evaluate_performance <- function(w, w_previous, next_return, beta = 50){
  raw_return <- as.matrix(next_return) %*% w
  turnover <- sum(abs(w - w_previous))
  net_return <- raw_return - beta / 10000 * turnover
  c(raw_return, turnover, net_return)
}


for (p in 1:trading_length){
  #p <- 1
  
  returns_window <- x_trading[p:(p + window_length - 1), ]
  next_return <- x_trading[p + window_length, ] %>% select(-Date) %>% as.matrix()
  #date <- x_trading[p + window_length, ] %>% select(Date) %>% as.matrix()
  
  Omega <- condCovariances[[trading_start + (p - 1)]]
  mu <- benchmark[p:(p + window_length - 1),] %>% colMeans()
  mu <- rep(mu, n_regions)
  #mu <- x_trading %>% select(-Date) %>% colMeans() * 0
  
  
  # 1) Naive portfolio
  w_1 <- rep(1 / n_regions, n_regions)
  
  #performance_values[[1]][p, 1] <- date
  performance_values[[1]][p, ] <- evaluate_performance(w_1, w_prev_1, next_return)
  w_prev_1 <- adjust_weights(w_1, next_return)
  performance_weights[[1]][p, 2:6] <- w_prev_1
  
  
  # 2) Efficient portfolio
  w_2 <- compute_efficient_weight(
    Omega = Omega,
    mu = mu,
    gamma = gamma
  )
  
  #performance_values[[2]][p, 1] <- date
  performance_values[[2]][p, ] <- evaluate_performance(w_2, w_prev_2, next_return)
  w_prev_2 <- adjust_weights(w_2, next_return)
  performance_weights[[2]][p, 2:6] <- w_prev_2
  
  
  # 3) Efficient portfolio TC-adjusted
  w_3 <- compute_efficient_weight_L1_TC(
    mu = mu,
    Omega = Omega,
    beta = beta,
    gamma = gamma,
    initial_weights = w_prev_3
  )
  
  #performance_values[[3]][p, 1] <- date
  performance_values[[3]][p, ] <- evaluate_performance(w_3, w_prev_3, next_return, beta = beta)
  w_prev_3 <- adjust_weights(w_3, next_return)
  performance_weights[[3]][p, 2:6] <- w_prev_3
  
}

#############################################################################
### evaluate performance net of TC ###
#############################################################################
performance <- lapply(
  performance_values,
  as_tibble) %>%
  bind_rows(.id = "strategy")


performance %>%
  group_by(strategy) %>%
  summarize(
    Mean = 12 * mean(100 * net_return),
    SD = sqrt(12) * sd(100 * net_return),
    `Sharpe ratio` = if_else(Mean > 0,
                             Mean / SD,
                             NA_real_
    ),
    Turnover = 100 * mean(turnover)
  )

performance %>%
  ggplot(aes(x = ., y = net_return, color = factor(strategy))) +
  geom_line()

mtcars <- mtcars
