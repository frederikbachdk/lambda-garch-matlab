library(tidyverse)
library(janitor)
library(xlsx)
library(bizdays)
library(openxlsx)

data <- loadWorkbook("data/13102022_data.xlsx")

# Use US govies trading calendar
load_quantlib_calendars("UnitedStates/GovernmentBond",
                        from = "2010-01-01",
                        to = "2022-06-30")

# clean price data
prices <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'PRICES_RAW') %>%
  clean_names() %>%
  filter(dates >= as.Date('2010-01-01'),
         is.bizday(dates, 'QuantLib/UnitedStates/GovernmentBond') == TRUE)

# clean covariate data
covariates <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'COVARIATES_RAW') %>%
  clean_names() %>%
  filter(dates >= as.Date('2010-01-01'),
         is.bizday(dates, 'QuantLib/UnitedStates/GovernmentBond') == TRUE)

# Write data to existing sheet
writeData(data, sheet = "PRICES_CLEAN", prices, colNames = TRUE)
writeData(data, sheet = "COVARIATES_CLEAN", covariates, colNames = TRUE)
saveWorkbook(data,"13102022_data.xlsx", overwrite = TRUE)


