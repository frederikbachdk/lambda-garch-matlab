# IMPORT PACKAGES
library(tidyverse)
library(gridExtra)
library(ggplotify)
library(latex2exp)
library(psych)
library(lemon)
library(dplyr)
library(jcolors)
source('R/utils/plotsFunctions.R')
source('R/getOmegas.R')

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
dens_embig <- data %>% select(EMBIG) %>%
  ggplot() + aes(x = EMBIG) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$EMBIG), 
                            sd = sd(data$EMBIG))) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, 
                            sd = 1),
                color = 'red') +
  ggtitle('Panel A: EMBIG') + 
  labs(y = 'Density') + 
  xlim(-5,5) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

dens_Africa <- data %>% select(Africa) %>%
  ggplot() + aes(x = Africa) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$Africa), 
                            sd = sd(data$Africa))) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, 
                            sd = 1),
                color = 'red') +
  ggtitle('Panel B: Africa') + 
  labs(y = 'Density') + 
  xlim(-5,5) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

dens_Asia <- data %>% select(Asia) %>%
  ggplot() + aes(x = Asia) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$Asia), 
                            sd = sd(data$Asia))) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, 
                            sd = 1),
                color = 'red') +
  ggtitle('Panel C: Asia') + 
  labs(y = 'Density') + 
  xlim(-5,5) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

dens_Europe <- data %>% select(Europe) %>%
  ggplot() + aes(x = Europe) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$Europe), 
                            sd = sd(data$Europe))) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, 
                            sd = 1),
                color = 'red') +
  ggtitle('Panel D: Europe') + 
  labs(y = 'Density') + 
  xlim(-5,5) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

dens_LatAm <- data %>% select('Latin America') %>% 
  rename(LatAm = 'Latin America') %>%
  ggplot() + aes(x = LatAm) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$'Latin America'), 
                            sd = sd(data$'Latin America'))) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, 
                            sd = 1),
                color = 'red') +
  ggtitle('Panel E: Latin America') + 
  labs(y = 'Density') + 
  xlim(-5,5) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

dens_MidEast <- data %>% select('Middle East') %>%
  rename(MidEast = 'Middle East') %>%
  ggplot() + aes(x = MidEast) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$'Middle East'), 
                            sd = sd(data$'Middle East'))) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, 
                            sd = 1),
                color = 'red') +
  ggtitle('Panel F: Middle East') + 
  labs(y = 'Density') + 
  xlim(-5,5) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

grid.arrange(dens_embig,dens_Africa, dens_Asia, 
             dens_Europe, dens_LatAm, dens_MidEast, 
             nrow = 3)



###############################################################################
# tail distribution
###############################################################################
data %>% select(Europe) %>%
  ggplot() + aes(x = Europe) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white", 
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$Europe), 
                            sd = sd(data$Europe))) +
  stat_function(fun = dnorm, 
                args = list(mean = 0, 
                            sd = 1),
                color = 'red') +
  ggtitle('Europe') + 
  labs(y = 'Density', x = '') + 
  xlim(-13,-1) + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14)) 
  

###############################################################################
# summary statistics
###############################################################################

# annualized means and std. devs
data_viz_region %>%
  group_by(Region) %>% 
  summarise_at(vars(Return),
               list(Mean = ~250*mean(.), 
                    Volatility = ~sqrt(250)*sd(.), 
                    Skewness = skewness(.),
                    Kurtosis = kurtosis(.),
                    Sharpe = ~sqrt(250)*mean(.)/sd(.))) %>%
  arrange(desc(Sharpe))

data %>% filter(Date <= as.Date('2021-12-31')) %>% 
  select(Africa, Asia, Europe, 'Latin America', 'Middle East') %>%
  describe()
  
  describe(data %>% select(Africa, Asia, Europe, 'Latin America', 'Middle East'))

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
# correlation plots
###############################################################################
# import data
data_x <- readxl::read_excel('data/covariates.xlsx', sheet = 'Indeces')
data_x <- data_x %>% mutate(Date = as.Date(data_x$Date))
indeces <- c('Date','BCOM','BCOMTR','BCOMIN','BCOMAG','BCOMEN','BCOIL','WCOIL','USGG10YR','BBDXY','JPEIDIVR')
colnames(data_x) <- indeces

data_x$BBDXY <- na_if(data_x$BBDXY, 0)

# 1. EMBIG return and commodity/10-yr rate/dollar
#data_x %>% select(JPEIDIVR, BCOM, USGG10YR, BBDXY) %>%
#  cor(use = "pairwise.complete.obs")

data_x_select1 <- data_x %>% select(JPEIDIVR, BCOM, USGG10YR, BBDXY)
cor1 <- round(cor(data_x_select1, use = "pairwise.complete.obs"), 3)


# 2. Total commodity individual commodities 
data_x_select2 <- data_x %>% select(BCOM, BCOMIN, BCOMAG, BCOMEN, BCOIL)
cor2 <- round(cor(data_x_select2, use = "pairwise.complete.obs"), 3)


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


###############################################################################
# simulation plots - realization
###############################################################################
simulations <- readxl::read_excel('MATLAB/simulated_series.xlsx')  %>%
  mutate(index = seq(0,1000)) %>%
  select(index, everything()) %>%
  pivot_longer(cols = O:X2,
               values_to = 'sim',
               names_to = 'series')

sim1 <- simulations %>% filter(series == 'O') %>%
  ggplot() +  aes(x = index, y = sim) + geom_line(color = 'steelblue') +
  ylim(-5,5) +
  theme(legend.position="none") +
  labs(x = '', y = '') + 
  ggtitle('Panel A: Simulated exogenous ARCH(1) series') + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14)) 

sim2 <- simulations %>% filter(series == 'X1') %>%
  ggplot() +  aes(x = index, y = sim) + geom_line(color = 'steelblue') +
  ylim(-20,20) +
  theme(legend.position="none") +
  labs(x = '', y = '') + 
  ggtitle('Panel B: Simulated multivariate GARCH series 1') + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

sim3 <- simulations %>% filter(series == 'X2') %>%
  ggplot() +  aes(x = index, y = sim) + geom_line(color = 'steelblue') +
  ylim(-20,20) +
  theme(legend.position="none") +
  labs(x = '', y = '') + 
  ggtitle('Panel C: Simulated multivariate GARCH series 2') + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

grid.arrange(sim1, sim2, sim3, nrow=3)

###############################################################################
# estimation plots - theta - C
###############################################################################

simulations <- readxl::read_excel('MATLAB/theta_sims.xlsx') %>%
  mutate(
    phi = exp(phi)/(1+exp(phi))*pi/2 - 0.3,
    omega1 = exp(omega1) - 1,
    omega2 = exp(omega2) - 1,
    a11 = a11^2 - 1/3,
    a21 = a21^2 - 1/3,
    a12 = a12^2 - 1/4,
    a22 = a22^2 - 1/4,
    b11 = b11^2 - 0.1,
    b21 = b21^2 - 0.1,
    b12 = b12^2 - 0.15,
    b22 = b22^2 - 0.15,
    c1 = exp(c1) - 0.8,
    c2 = exp(c2) - 0.8)

c1 <- simulations %>% 
  ggplot() + aes(x = c1) + 
  geom_histogram(aes(y =..density..),
      colour = "black", 
      fill = "white",
      bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$c1), 
                            sd = sd(simulations$c1))) +
  ggtitle('Panel A: Density of MLE for first element in C') + 
  labs(y = 'Density') + 
  xlim(-2.5,2.5) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))
  
c2 <- simulations %>% 
  ggplot() + aes(x = c2) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$c2), 
                            sd = sd(simulations$c2))) +
  ggtitle('Panel B: Density of MLE for second element in C') +
  labs(y = 'Density') + 
  xlim(-2.5,2.5) +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

grid.arrange(c1, c2, nrow=2)

###############################################################################
# simulation plots - thetahat - A
###############################################################################

a11 <- simulations %>% 
  ggplot() + aes(x = a11) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$a11), 
                            sd = sd(simulations$a11))) +
  ggtitle('Panel A: Density of MLE for element (1,1) in A') + 
  labs(y = 'Density') + 
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

a12 <- simulations %>% 
  ggplot() + aes(x = a12) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$a12), 
                            sd = sd(simulations$a12))) +
  ggtitle('Panel A: Density of MLE for element (1,2) in A') + 
  labs(y = 'Density') + 
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

a21 <- simulations %>% 
  ggplot() + aes(x = a21) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$a21), 
                            sd = sd(simulations$a21))) +
  ggtitle('Panel A: Density of MLE for element (2,1) in A') + 
  labs(y = 'Density') + 
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

a22 <- simulations %>% 
  ggplot() + aes(x = a22) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$a22), 
                            sd = sd(simulations$a22))) +
  ggtitle('Panel A: Density of MLE for element (2,2) in A') + 
  labs(y = 'Density') +
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

grid.arrange(a11, a12, a21, a22, nrow=2)


###############################################################################
# simulation plots - thetahat - B
###############################################################################

b11 <- simulations %>% 
  ggplot() + aes(x = b11) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$b11), 
                            sd = sd(simulations$b11))) +
  ggtitle('Panel A: Density of MLE for element (1,1) in B') + 
  labs(y = 'Density') + 
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

b12 <- simulations %>% 
  ggplot() + aes(x = b12) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$b12), 
                            sd = sd(simulations$b12))) +
  ggtitle('Panel A: Density of MLE for element (1,2) in B') + 
  labs(y = 'Density') + 
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

b21 <- simulations %>% 
  ggplot() + aes(x = b21) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$b21), 
                            sd = sd(simulations$b21))) +
  ggtitle('Panel A: Density of MLE for element (2,1) in B') + 
  labs(y = 'Density') + 
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

b22 <- simulations %>% 
  ggplot() + aes(x = b22) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "white",
                 bins = 100) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(simulations$b22), 
                            sd = sd(simulations$b22))) +
  ggtitle('Panel A: Density of MLE for element (2,2) in B') + 
  labs(y = 'Density') +
  xlim(-1,1) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 14), 
    strip.background = element_blank(),
    strip.text = element_text(size=14),
    plot.title = element_text(hjust = 0.5, size = 14))

grid.arrange(b11, b12, b21, b22, nrow=2)
