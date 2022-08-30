# IMPORT PACKAGES
library(tidyverse)
library(janitor)
library(gridExtra)
library(lemon)
library(ggplotify)
library(forecast)
source('utils/functions.R')

### IMPORT DATA ###
regions <- c('period', 'Asia', 'Africa', 'Middle East', 'Latin America')
data <- readxl::read_excel("data/USBanks.xlsx")
data$Period <- as.Date(data$Period)
colnames(data) <- regions

### DATA VISUALIZATIONS ###
data_viz <- data %>% pivot_longer(cols = Asia:'Latin America', 
                                  names_to = 'region',
                                  values_to = 'log_ret')

# Figure 1) Daily return line plots
data_viz %>% 
  ggplot() + 
  geom_line(mapping = aes(x = period, y = log_ret), color = 'steelblue') +
  ylim(-25, 25) + 
  facet_rep_wrap(~ region, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = '') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.background = element_blank()) + 
  scale_x_date(breaks = scales::breaks_pretty(10))


# Figure 2) Autocorrelation functions for log returns

# Figure 3) Autocorrelation functions for squared log returns

# Figure 4) Density plots
data_viz %>% 
  ggplot() + 
  geom_histogram(mapping = aes(x = log_ret), 
                 bins = 250,
                 fill = 'steelblue') +
  geom_density(mapping = aes(x = log_ret), 
                 color = 'steelblue') +
  xlim(-15, 15) +
  facet_rep_wrap(~ region, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = '') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.background = element_blank())



###### SUMMARY STATS ###### 

# correlation matrix
cormat <- data %>% select(-period) %>% 
  cor(use = "complete.obs")

# covariance matrix
covmat <- data %>% select(-period) %>% 
  cov(use = "complete.obs")

# annualized mean and std. dev
moments <- apply(data %>% select(-period), 2, descriptive_stats) %>% 
  as.data.frame(row.names = c('mean', 'std. dev'))

# eigenvalues of the unconditional covariance matrix
eig <- eigen(covmat)
100*eig$values/sum(eig$values)
eig$vectors

