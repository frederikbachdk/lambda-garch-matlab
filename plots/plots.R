library(tidyverse)
library(janitor)
library(gridExtra)
source('plots/plots_functions.R')

# load data
returns <- readxl::read_excel("data/12072022_embig_data.xlsx", sheet = "Returns") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(cols = c(-Date), 
               names_to = 'region',
               values_to = 'log_ret')

###############################################################################
# visualize daily total return
###############################################################################

# across regions
returns_regions <- returns %>% 
  filter(!(region %in%  c('HY','IG'))) %>%
  ggplot() + aes(x = Date, y = log_ret, color = region) +
  geom_line() +
  theme_classic() +
  ylab('Daily total log-return (%)') +
  xlab('') + 
  theme(legend.position="bottom",
  legend.margin=margin(t=-25),
  legend.title=element_blank(),
  legend.text=element_text(size=6.5))

# across rating categories
returns_ratings <- returns %>% 
  filter(region %in%  c('HY','IG')) %>%
  mutate(region = replace(region, 
                          region == 'HY', 'High Yield')) %>%
  mutate(region = replace(region, 
                         region == 'IG', 'Investment Grade')) %>%
  ggplot() + aes(x = Date, y = log_ret, color = region) +
  geom_line() +
  theme_classic() +
  ylab('Daily total log-return (%)') +
  xlab('') +
  theme(legend.position="bottom",
        legend.margin=margin(t=-25),
        legend.title=element_blank(),
        legend.text=element_text(size=6.5))

grid.arrange(returns_regions, returns_ratings, ncol = 2) 

###############################################################################
# autocorrelation function plots
###############################################################################

# example with returns, absolute and squared returns
acf_plot(x = returns, region = 'Asia', lag.max = 20)
acf_plot(x = returns %>% mutate(log_ret = abs(log_ret)), region = 'Europe', lag.max = 20)
acf_plot(x = returns %>% mutate(log_ret = log_ret^2), region = 'Europe', lag.max = 20)

###############################################################################
# density plots
###############################################################################

x <- returns %>% select(Europe) %>% filter(Europe %in% (-5:5))
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
