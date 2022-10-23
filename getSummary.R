# IMPORT PACKAGES
library(tidyverse)
library(gridExtra)
library(ggplotify)
library(latex2exp)
library(lemon)
library(dplyr)
source('utils/plotsFunctions.R')
#source('getOmegas.R')
source('getOmegasExtended.R')


### IMPORT DATA ###
data <- readxl::read_excel('data/07092022_embig_data.xlsx', sheet = 'Returns') %>% 
  filter(Date >= '2010-01-01') %>%
  mutate(Date = as.Date(Date)) %>%
  rename('Investment Grade' = IG,
         'High Yield' = HY)

weights <- readxl::read_excel('data/07092022_embig_data.xlsx', sheet = 'Weights') %>% 
  filter(Date >= '2010-01-01') %>%
  mutate(Date = as.Date(Date))

### PIVOT DATA  ###
data_viz <- data %>% 
  pivot_longer(cols = Africa:'High Yield', 
               names_to = 'Group',
               values_to = 'Return')

data_viz_region <- data_viz %>% filter(Group %in% 
                                         c('EMBIG',
                                           'Africa', 
                                           'Asia',
                                           'Europe',
                                           'Latin America',
                                           'Middle East')) %>%
  rename('Region' = Group)

data_viz_rating <- data_viz %>% filter(Group %in% 
                                         c('Investment Grade',
                                           'High Yield'))

###############################################################################
# visualize daily total return
###############################################################################

# Figure 1) Daily return line plots for regions
data_viz_region$Region <- factor(data_viz_region$Region, 
                                levels=c('EMBIG', 
                                         'Africa',
                                         'Asia',
                                         'Europe',
                                         'Latin America',
                                         'Middle East'))
data_viz_region %>%
  ggplot() + 
  geom_line(mapping = aes(x = Date, y = Return), color = 'steelblue') +
  ylim(-2.5, 2.5) + 
  facet_rep_wrap(~ Region, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = 'Total Return (%)') + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10))

ggsave('regional_returns.png', dpi = 'retina',
       path = 'plots/')

# Figure 2) Daily return line plots for ratings
data_viz_rating %>%
  ggplot() + 
  geom_line(mapping = aes(x = Date, y = Return), color = 'steelblue') +
  ylim(-2.5, 2.5) + 
  facet_rep_wrap(~ Group, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = 'Total Return (%)') + 
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10))

ggsave('rating_returns.png', dpi = 'retina',
       path = 'plots/')

###############################################################################
# visualize weights
###############################################################################
weight_viz <- weights %>% 
  pivot_longer(cols = Europe:'Latin America', 
               names_to = 'Region',
               values_to = 'Weight')


# Figure 3) Weight line plots for regions
weight_viz %>%
  ggplot() + aes(x = Date, y = Weight, color = Region) + geom_line() +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909")) +
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  ylim(0,50) +
  theme_classic()

ggsave('regional_weights.png', dpi = 'retina',
       path = 'plots/')

###############################################################################
# autocorrelation function plots
###############################################################################

# Figure 4) ACF plots for regions
acf_list <- list()
acf_list_sq <- list()
acf_list_abs <- list()
ctrys <- c('EMBIG', 'Africa', 'Asia', 'Europe', 'Latin America', 'Middle East')

for(i in 1:6){
  acf_list[[i]] <- acf_plot(x = data_viz_region, region = ctrys[i], lag.max = 20)
  acf_list_sq[[i]] <- acf_plot(x = data_viz_region %>%
                               mutate(Return = Return^2), 
                               region = ctrys[i], lag.max = 20)
  acf_list_abs[[i]] <- acf_plot(x = data_viz_region %>%
                                 mutate(Return = abs(Return)), 
                               region = ctrys[i], lag.max = 20)
}
do.call("grid.arrange", c(acf_list, ncol=2))
ggsave('acf_returns', dpi = 'retina',
       path = 'plots/')

do.call("grid.arrange", c(acf_list_sq, ncol=2))
ggsave('acf_sq_returns', dpi = 'retina',
       path = 'plots/') 

do.call("grid.arrange", c(acf_list_abs, ncol=2))


###############################################################################
# density plots
###############################################################################

# Figure 5) Density plots
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
data_viz_region %>%
  group_by(Region) %>% 
  summarise_at(vars(Return),
               list(Mean = ~250*mean(.), 
                    Volatility = ~sqrt(250)*sd(.), 
                    Sharpe = ~sqrt(250)*mean(.)/sd(.))) %>%
  arrange(desc(Sharpe))

# correlation matrix
data %>% select(-Date, -'Investment Grade', -'High Yield', -EMBIG) %>%  
  cor(use = "complete.obs")

# covariance matrix
covmat <- data %>% select(-Date, -'Investment Grade', -'High Yield', -EMBIG) %>%  
  cov(use = "complete.obs") %>%
  round(digits=3)

# eigenvalues of the unconditional covariance matrix
eig <- eigen(covmat)
100*eig$values/sum(eig$values)
eig$vectors

###############################################################################
# correlation table & plot
###############################################################################
# import indeces
data_x <- readxl::read_excel('data/07092022_covariates.xlsx', sheet = 'Indices') %>%
  mutate(Date = as.Date(Date))
indeces <- c('Date','BCOM','BCOMTR','BCOMIN','BCOMAG','BCOMEN','BCOIL','WCOIL','USGG10YR','BBDXY','JPEIDIVR')
colnames(data_x) <- indeces

# covariates
exos <- data_x %>%
  select(Date, BCOIL, USGG10YR, BBDXY) %>%
  mutate(OIL = log(BCOIL) - log(lag(BCOIL)),
         '10YR' = log(USGG10YR) - log(lag(USGG10YR)),
         USD = log(BBDXY) - log(lag(BBDXY))) %>%
  filter(Date > '2010-01-01') %>%
  select(Date, OIL, '10YR', USD) %>%
  pivot_longer(cols = !Date, names_to = "index", values_to = "values")

# regions
returns <- data %>%
  filter(Date > '2010-01-01',
         Date <= '2022-09-06') %>%
  pivot_longer(cols = !Date, names_to = "region", values_to = "values") %>%
  mutate(values = values / 100)

# illustrate return time series
ggplot() + 
  geom_line(returns, mapping = aes(x = Date, y = values)) + 
  facet_rep_wrap(~ region, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position = "bottom") +
  labs(x = '', y = 'Log Returns (USD)', color = 'Covariate') + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  # scale_color_manual(labels=c('Oil','US 10 yr'),#,'USD'),
  #                      values=c('lightskyblue','olivedrab','lightpink')) +
  scale_color_jcolors(palette = "pal7") +
  geom_line(as.data.frame(exos), mapping = aes(x = Date, y = values, color = index))

ggsave('return_variance.png', dpi = 'retina', path = 'plots/')

# # 1. EMBIG return and commodity/10-yr rate/dollar
# data_x %>% select(JPEIDIVR, BCOM, USGG10YR, BBDXY) %>%
#  cor(use = "pairwise.complete.obs")
# 
# data_x_select1 <- data_x %>% select(JPEIDIVR, BCOM, USGG10YR, BBDXY)
# cor1 <- round(cor(data_x_select1, use = "pairwise.complete.obs"), 3)
# 
# # 2. Total commodity individual commodities 
# data_x_select2 <- data_x %>% select(BCOM, BCOMIN, BCOMAG, BCOMEN, BCOIL)
# cor2 <- round(cor(data_x_select2, use = "pairwise.complete.obs"), 3)



###############################################################################
# eigenvalue plots
###############################################################################

# create normalized eigenvalues
condEigenvals_norm <- condEigenvals %>% 
  mutate(row_sum = rowSums(select(., 2:6))) %>% 
  mutate_at(2:6, ~ ./row_sum) %>% 
  select(-row_sum) %>%
  pivot_longer(cols = lambda1:lambda5,
               names_to = 'Eigenvalue',
               values_to = 'Value')

condEigenvals_long <- condEigenvals %>% 
  pivot_longer(cols = lambda1:lambda5,
               names_to = 'Eigenvalue',
               values_to = 'Value')

# plot normalized eigenvalues
eigplot1 <- condEigenvals_long %>%
  ggplot() + aes(x = Date, y = Value, color = Eigenvalue) + geom_line() +
  labs(x = '', y = 'Eigenvalue') + 
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_jcolors(palette = "pal7")
  
eigplot2 <- condEigenvals_norm %>%
  ggplot() + aes(x = Date, y = Value, color = Eigenvalue) + geom_line() +
  labs(x = '', y = 'Eigenvalue (%)') + 
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) + 
  scale_color_jcolors(palette = "pal7") 

grid.arrange(eigplot1, eigplot2)
ggsave('eigenvals', dpi = 'retina',
       path = 'plots/')
