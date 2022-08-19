library(tidyverse)
library(janitor)
source('plots/plots_functions.R')

# load data
returns <- readxl::read_excel("data/12072022_embig_data.xlsx", sheet = "Returns") %>%
  mutate(Date = as.Date(Date)) %>%
  pivot_longer(cols = c(-Date), 
               names_to = 'Group',
               values_to = 'Return')

###############################################################################
# visualize daily total return
###############################################################################

# across regions
returns %>% 
  filter(!(Group %in%  c('HY','IG'))) %>%
  ggplot() + aes(x = Date, y = Return, color = Group) +
  geom_line() +
  theme_classic() +
  ylab('Daily total log-return (%)') +
  xlab('')

# across rating categories
returns %>% 
  filter(Group %in%  c('HY','IG')) %>%
  ggplot() + aes(x = Date, y = Return, color = Group) +
  geom_line() +
  theme_classic() +
  ylab('Daily total log-return (%)') +
  xlab('')


###############################################################################
# autocorrelation function plots
###############################################################################

# example with returns, absolute and squared returns
acf_plot(x = returns, region = 'Asia', lag.max = 20)
acf_plot(x = returns %>% mutate(Return = abs(Return)), region = 'Europe', lag.max = 20)
acf_plot(x = returns %>% mutate(Return = Return^2), region = 'Europe', lag.max = 20)

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
