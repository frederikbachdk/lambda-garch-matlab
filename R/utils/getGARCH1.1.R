#############################################################################
### load libraries and settings ###
#############################################################################
library(tidyverse)
library(tictoc)
library(rugarch)
library(aTSA)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())
set.seed(2022)

#############################################################################
### define sample period ###
#############################################################################
estimation_start <- as.Date('2010-01-01')
estimation_end <- as.Date('2018-12-31')

#############################################################################
### load data ###
#############################################################################

# import data
data <- readxl::read_excel('data/13102022_data.xlsx', 
                           sheet = 'DATA_CLEAN') %>%
  select(Date,
         bbg_commodity_index,
         brent_crude_oil,
         us_10_yr_yield,
         dollar_strength_index) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= estimation_start,
         Date <= estimation_end) 

#############################################################################
### estimate GARCH(1,1) models and assess stationarity ###
#############################################################################

model <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

modelfit1 <- ugarchfit(spec = model,
                      data = data %>% 
                        select(bbg_commodity_index) %>%
                        as.matrix())
par1 <- tibble(model = 'BCOM',
               alpha = modelfit1@fit$coef[2],
               beta = modelfit1@fit$coef[3])

misspec1 <- tibble(model = 'BCOM',
                   lag5_aut = Box.test(modelfit1@fit$residuals, lag = 5, type = "Ljung")[3] %>% as.double(),
                   lag10_aut = Box.test(modelfit1@fit$residuals, lag = 10, type = "Ljung")[3] %>% as.double(),
                   lag15_aut = Box.test(modelfit1@fit$residuals, lag = 15, type = "Ljung")[3] %>% as.double(),
                   lag5_arch = Box.test((modelfit1@fit$residuals)^2, lag = 1, type = "Ljung")[3] %>% as.double(),
                   lag10_arch = Box.test(modelfit1@fit$residuals^2, lag = 10, type = "Ljung")[3] %>% as.double(),
                   lag15_arch = Box.test(modelfit1@fit$residuals^2, lag = 15, type = "Ljung")[3] %>% as.double(),
                   )

modelfit2 <- ugarchfit(spec = model,
                       data = data %>% 
                         select(brent_crude_oil) %>%
                         as.matrix())
par2 <- tibble(model = 'BRENT',
               alpha = modelfit2@fit$coef[2],
               beta = modelfit2@fit$coef[3])


modelfit3 <- ugarchfit(spec = model,
                       data = data %>% 
                         select(us_10_yr_yield) %>%
                         as.matrix())
par3 <- tibble(model = 'US10YR',
               alpha = modelfit3@fit$coef[2],
               beta = modelfit3@fit$coef[3])


modelfit4 <- ugarchfit(spec = model,
                       data = data %>% 
                         select(dollar_strength_index) %>%
                         as.matrix())
par4 <- tibble(model = 'USD',
               alpha = modelfit4@fit$coef[2],
               beta = modelfit4@fit$coef[3])

pars <- rbind(par1, par2, par3, par4) %>%
  mutate(sum = alpha + beta)
