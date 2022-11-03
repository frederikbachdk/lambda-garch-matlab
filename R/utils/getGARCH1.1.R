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
         wti_crude_oil,
         us_10_yr_yield,
         dollar_strength_index) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= estimation_start) 

#############################################################################
### estimate GARCH(1,1) models and check stationarity ###
#############################################################################

model <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "norm"
)

modelfit <- ugarchfit(spec = model,
                      data = data %>% 
                        select(bbg_commodity_index) %>%
                        as.matrix())
