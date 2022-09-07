# IMPORT PACKAGES
library(tidyverse)
library(janitor)
library(gridExtra)
library(lemon)
library(ggplotify)
library(forecast)
source('plots/plots_functions.R')

### IMPORT DATA ###
regions <- c('Date', 'IG', 'HY','Africa', 'Asia', 'Europe', 'Latin America', 'Middle East')
data <- readxl::read_excel('data/12072022_embig_data.xlsx', sheet = 'Returns')
data <- data %>% mutate(Date = as.Date(data$Date))

colnames(data) <- regions

### PIVOT DATA FOR GGPLOT2 ###
data_viz <- data %>% 
  pivot_longer(cols = IG:'Middle East', 
               names_to = 'region',
               values_to = 'log_ret')

data_viz_region <- data_viz %>% filter(region %in% 
                                         c('Africa', 
                                           'Asia',
                                           'Europe',
                                           'Latin America',
                                           'Middle East'))

data_viz_rating <- data_viz %>% filter(region %in% 
                                         c('IG',
                                           'HY'))

###############################################################################
# visualize daily total return
###############################################################################

# Figure 1) Daily return line plots for regions
data_viz_region %>% filter(Date >= '2008-01-01') %>%
  ggplot() + 
  geom_line(mapping = aes(x = Date, y = log_ret), color = 'steelblue') +
  ylim(-8, 8) + 
  facet_rep_wrap(~ region, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = 'Total Return (%)') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.background = element_blank()) + 
  scale_x_date(breaks = scales::breaks_pretty(10))

# Figure 2) Daily return line plots for ratings
data_viz_rating %>% filter(Date >= '2008-01-01') %>%
  ggplot() + 
  geom_line(mapping = aes(x = Date, y = log_ret), color = 'steelblue') +
  ylim(-8, 8) + 
  facet_rep_wrap(~ region, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = 'Total Return (%)') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(strip.background = element_blank()) + 
  scale_x_date(breaks = scales::breaks_pretty(10))

###############################################################################
# autocorrelation function plots
###############################################################################

# Figures 3-5) ACF plots
acf_list <- list()
acf_list_sq <- list()
acf_list_abs <- list()
ctrys <- c('Africa', 'Asia', 'Europe', 'Latin America', 'Middle East')
for(i in 1:5){
  acf_list[[i]] <- acf_plot(x = data_viz_region, region = ctrys[i], lag.max = 20)
  acf_list_sq[[i]] <- acf_plot(x = data_viz_region %>%
                               mutate(log_ret = log_ret^2), 
                               region = ctrys[i], lag.max = 20)
  acf_list_abs[[i]] <- acf_plot(x = data_viz_region %>%
                                 mutate(log_ret = abs(log_ret)), 
                               region = ctrys[i], lag.max = 20)
}
do.call("grid.arrange", c(acf_list, ncol=2))
do.call("grid.arrange", c(acf_list_sq, ncol=2))
do.call("grid.arrange", c(acf_list_abs, ncol=2))

###############################################################################
# density plots
###############################################################################

# Figure 4) Density plots
x <- data %>% select(Europe) %>% filter(between(Europe, -2, 2))
ctry <- colnames(x)

x %>% ggplot() + aes(x = Europe) +
  geom_density() +
  ggtitle(paste0(ctry)) +
  ylab('Probability (%)') +
  xlab('Daily Total Return (%)') +
  xlim(-5,5) +
  ggtitle(paste0(ctry)) +
  theme_classic() +
  theme(text = element_text(color="black"),
        axis.text=element_text(color="black"),
        plot.title = element_text(hjust = 0.5))

###############################################################################
# summary statistics
###############################################################################

# annualized means and std. devs
data_viz %>%
  group_by(region) %>% 
  summarise_at(vars(log_ret),
               list(Mean = ~250*mean(.), 
                    Volatility = ~sqrt(250)*sd(.), 
                    Sharpe = ~sqrt(250)*mean(.)/sd(.))) %>%
  arrange(desc(Sharpe))

# correlation matrix
data %>% select(-Date, -IG, -HY) %>%  
  cor(use = "complete.obs")


# covariance matrix
covmat <- data %>% select(-period) %>% 
  cov(use = "complete.obs")




# eigenvalues of the unconditional covariance matrix
eig <- eigen(covmat)
100*eig$values/sum(eig$values)
eig$vectors

