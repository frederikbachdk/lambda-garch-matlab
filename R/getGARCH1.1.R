#############################################################################
### load libraries and settings ###
#############################################################################
library(tidyverse)
library(tictoc)
library(rugarch)

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
  filter(Date >= as.Date(estimation_start),
         Date <= as.Date(estimation_end)) %>%
  select(-Date)

#############################################################################
### estimate GARCH(1,1) models and check stationarity ###
#############################################################################
model <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

estimate_garch <- function(x){
  
  modelfit <- ugarchfit(spec = model,
                        data = x %>% as.matrix())
  
  return(modelfit@fit$residuals)
  
}

residuals = tibble(res1 = sapply(data %>% select(bbg_commodity_index), estimate_garch),
                   res2 = sapply(data %>% select(brent_crude_oil), estimate_garch),
                   res3 = sapply(data %>% select(us_10_yr_yield), estimate_garch),
                   res4 = sapply(data %>% select(dollar_strength_index), estimate_garch),
) %>% as.matrix()

colnames(residuals) <- c('bcom','brent','us10yr','usd')

Box.test(residuals[,1]^2, 'Ljung-Box', lag = 15)
