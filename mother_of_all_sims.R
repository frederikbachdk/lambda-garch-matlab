library(tidyverse)
library(rugarch)
library(EnvStats)
source('R/utils/simulationFunctions.R')

set.seed(2022)
options(scipen=999) 

N <- 10000 # number of simulations
T = 1000  # length of time series
x0 = 0    # initial value
theta0 = c(1, 0.5) # true parameters

# initialize tibble 
simulations <- tibble(
  sim = seq(1,N),
  LR  = rep(0, N)
)

# define ARCH models
model <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(6, 0)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm"
)

model_res <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(6, 0)),
  mean.model = list(armaOrder = c(0,0), include.mean = FALSE),
  distribution.model = "norm",
  fixed.pars = list(alpha2 = 0, alpha3 = 0, alpha4 = 0, alpha5 = 0, alpha6 = 0)
)

for(n in (1:N)){
  
  # count simulation number
  simulations$sim[n] <- n  
  
  print(paste0('Series ', n, ' out of ', N))
  
  # simulate an ARCH process under H0: omega = 1, alpha1 = 0.5
  ARCH_sim <- ARCH(T, x0, theta0)
  y <- ARCH_sim$x
  
  modelfit <- ugarchfit(spec = model, data = y)
  loglik_unres <- modelfit@fit$LLH
  
  modelfit_res <- ugarchfit(spec = model_res,data = y)
  loglik_res <- modelfit_res@fit$LLH
  
  # calculate LR statistic for alpha = 0
  simulations$LR[n] <- -2*(loglik_res - loglik_unres)
}

LR <- simulations %>% select(LR) %>% filter(LR<100) %>% deframe()

epdfPlot(LR)       # plot empirical density
critval <- quantile(simulations$LR, 0.95) # find empirical 95% quantile


simulations <- read.csv('simulated_LR_big.csv') 
chi2 <- tibble(chi2 = rchisq(10^6, 5))


simulations %>% filter(LR<100) %>% ggplot() + aes(x = LR) + 
  geom_density(color = 'tomato') +
  geom_density(data = chi2, aes(x = chi2)) +
  theme_classic() 

