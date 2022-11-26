library(tidyverse)
library(quadprog)

# import data 
data <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>% 
  filter(Date >= as.Date('2019-01-01'), Date <= as.Date('2020-01-01')) %>%
  mutate(Date = as.Date(Date)) %>%
  rename('EMBIG' = 'EMBIG Div') %>% 
  select(Date:EMBIG)

# define conditional distribution  
condDynamics5 <- readRDS('data/conditionalDynamics5.rds')
condVar5 <- condDynamics5$condCovar
Omega <- condVar5[['2020-01-02']] * 250
colnames(Omega) <- c('Africa', 'Asia', 'Europe', 'Latin America', 'Middle East')

mu <- data %>%
  select(-Date, -EMBIG) %>% colMeans() %>% as.matrix() * 250
colnames(mu) <- 'mu'

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
  w_efficient <- solve.QP(
    Dmat = 2 * Omega,
    dvec = mu,
    Amat = cbind(rep(1, 5)),
    bvec = 1,
    meq = 1
  )
  
  weff <- w_efficient$solution
  
  # Calculate efficient frontier
  c <- seq(from = -1.5, to = 1.5, by = 0.001)
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
regions <- cbind(mu, sd = sqrt(diag(Omega))) %>% data.frame()
minvar <- results %>% filter(c == c(0)) %>% data.frame() %>%
  select(-c) 
rownames(minvar) <- 'Min. Var.'

eff <- results %>% filter(c == c(1)) %>% data.frame() %>%
  select(-c) 
rownames(eff) <- 'Eff. Port.'


# visualize the efficient frontier
regions %>% ggplot() +
  aes(x = sd, y = mu) + 
  geom_point(colour = 'steelblue', size = 2.5) +
  geom_point(data = results, aes(x = sd, y = mu)) +
  geom_point(colour = 'steelblue', size = 2.5) +
  geom_point(data = minvar, aes(x = sd, y = mu),colour = 'red', size = 3) +
  geom_point(data = eff, aes(x = sd, y = mu),colour = 'red', size = 3) +
  geom_text(hjust=0, vjust=1.5, aes(label = rownames(regions)), size = 5) +
  geom_text(data = minvar, hjust=-0.2, vjust=-0.3, aes(label = rownames(minvar)), size = 5) +
  geom_text(data = eff, hjust=0.4, vjust=-1.5, aes(label = rownames(eff)), size = 5) +
  labs(y = 'Annualized return (%)', x = 'Annualized standard deviation (%)') +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        ) 

