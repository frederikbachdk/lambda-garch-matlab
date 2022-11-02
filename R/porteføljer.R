library(tidyverse)
library(quadprog)

# import data 
data <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>% 
  filter(Date >= '2010-01-01') %>%
  mutate(Date = as.Date(Date)) %>%
  rename('EMBIG' = 'EMBIG Div') %>% 
  select(Date:EMBIG)

# define conditional distribution  
condDynamics5 <- readRDS('data/conditionalDynamics5.rds')
condVar5 <- condDynamics5$condCovar
Omega <- condVar5[['2020-01-02']] * 250
mu <- data %>%
  select(-Date, -EMBIG) %>% colMeans() %>% as.matrix() * 250

# calculate optimal weights

# 1) MVP
w_mvp <-  solve.QP(
  Dmat = Omega,
  dvec = rep(0, 5),
  Amat = cbind(rep(1, 5)),
  bvec = 1,
  meq = 1
)

# 2) Efficient
w_efficient <- solve.QP(
  Dmat = 2 * Omega,
  dvec = mu,
  Amat = cbind(rep(1, 5)),
  bvec = 1,
  meq = 1
)




compute_efficient_frontier <- function(Omega,mu){
  
  # Compute the minimum variance portfolio weights 
  iota = rep(1, ncol(Omega))
  wmvp <- solve(Omega) %*% iota 
  wmvp <- wmvp / sum(wmvp)
  
  # Compute the efficient portfolio weights
  mu_bar <- mu + 0.5 # return target 
  C <- as.numeric(t(iota)%*%solve(Omega)%*%iota)
  D <- as.numeric(t(iota)%*%solve(Omega)%*%mu)
  E <- as.numeric(t(mu)%*%solve(Omega)%*%mu)
  lambda_tilde <- as.numeric(2*(mu_bar - D/C)/(E - D^2/C))
  weff <- wmvp + lambda_tilde/2*(solve(Omega)%*%mu - D/C*solve(Omega)%*%iota)
  
  # Calculate efficient frontier
  c <- seq(from = -2, to = 2, by = 0.01)
  res <- tibble(c = c, 
                mu = NA,
                sd = NA)
  
  # Compute all linear combinations of the two portfolios
  for(i in seq_along(c)){
    w <- (1-c[i])*wmvp + c[i]*weff 
    res$mu[i] <- t(w) %*% mu 
    res$sd[i] <- sqrt(t(w) %*% Omega %*% w) 
  }
  
  return(res)
}

# save effecient frontier computed from the function
results <- compute_efficient_frontier(Omega,mu)

# visualize the efficient frontier
ggplot(results, aes(x = sd, y = mu)) + 
  geom_point() +
  geom_point(data = results %>% filter(c %in% c(0,1)), 
             color = "red",
             size = 4) +              # locate the mvp and efficient portfolio
  geom_point(data = tibble(mu = mu, sd = sqrt(diag(Omega))), 
             aes(y = mu, x  = sd), color = "blue", size = 1) + 
  labs(y = 'Annualized return (%)', x = 'Annualized standard deviation (%)') +
  
  # plot the individual assets
  
  theme(plot.title = element_text(hjust=0.5, color = "black", size = 12, 
                                  face = "bold"),
        plot.caption = element_text(color = "black", face = "italic"),
        panel.background = element_rect(fill="white", colour="grey", 
                                        linetype="solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  theme_classic()

