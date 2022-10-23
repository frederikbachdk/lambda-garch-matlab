import_packages <- function(){
  suppressMessages(library(tidyverse))
  suppressMessages(library(matlib))
  suppressMessages(library(tictoc))
  suppressMessages(library(pracma))
  suppressMessages(library(quadprog))
  suppressMessages(library(scales))
  suppressMessages(library(alabama))
  suppressMessages(library(lemon))
  suppressMessages(library(lubridate))
}

import_functions <- function(){
  source('R/getPortfolios.R')
}

import_data <- function(){
  data <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>%
    mutate(Date = as.Date(Date))
    return(data)
}

import_theta <- function(){
  theta <- readxl::read_excel('thetahat.xlsx') %>% as.matrix()
  return(theta)
}