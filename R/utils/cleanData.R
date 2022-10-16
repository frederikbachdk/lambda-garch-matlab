library(tidyverse)
library(janitor)
library(xlsx)
library(bizdays)

load_quantlib_calendars("UnitedStates/GovernmentBond",
                        from = "2010-01-01",
                        to = "2022-06-30")

prices <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'PRICES_RAW') %>%
  clean_names() %>%
  filter(dates >= as.Date('2010-01-01'),
         is.bizday(dates, 'QuantLib/UnitedStates/GovernmentBond') == TRUE)

covariates <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'COVARIATES_RAW') %>%
  clean_names() %>%
  filter(dates >= as.Date('2010-01-01'),
         is.bizday(dates, 'QuantLib/UnitedStates/GovernmentBond') == TRUE)

