# load libraries
library(tidyverse)
library(lubridate)
library(gridExtra)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

# import functions
source('R/getPortfolios.R')

# import data
benchmark <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>% 
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= as.Date('2019-01-02'), 
         Date <= as.Date('2022-06-30')) %>%
  select(Date, EMBIG = 'EMBIG Div')

# import data (alternative trading sample)
benchmark <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>% 
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= as.Date('2015-07-01'), 
         Date <= as.Date('2018-12-31')) %>%
  select(Date, EMBIG = 'EMBIG Div')

#############################################################################
### prepare portfolio performance dataframes ###
#############################################################################

# calculate portfolio weights for preferred model
trading <- getPortfolios(model = 5, 
                         trading_start = '2015-07-01', 
                         trading_end = '2018-12-31')

# combine lists
portfolios <- lapply(
  trading,
  as_tibble) %>%
  bind_rows(.id = "strategy") %>%
  select(strategy, Return) %>%
  transmute(strategy = strategy,
            return = Return * 100)

# append EMBIG return
performance <- benchmark %>%
  select(return = EMBIG) %>%
  mutate(strategy = "EMBIG") %>%
  bind_rows(portfolios) %>%
  group_by(strategy) %>%
  mutate(date = benchmark$Date) %>%
  pivot_wider(names_from = 'strategy',
              values_from = 'return')

#############################################################################
### PLOT 1 - EVALUATION OF DAILY RETURNS COMPARED TO BENCHMARK  ###
#############################################################################

ew_plot <- performance %>% ggplot() +
  aes(x = date, y = EMBIG) +
  geom_line(color = 'black', size = 1) +
  geom_line(data = performance, 
            aes(x = date, y = EW),
            color = 'tomato') +
  labs(x = '', y = 'Daily return (%)') +
  ggtitle('Panel A: Equal-weighted portfolio') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(12))
  

mvp_plot <- performance %>% ggplot() +
  aes(x = date, y = EMBIG) +
  geom_line(color = 'black', size = 1) +
  geom_line(data = performance, 
            aes(x = date, y = MVP),
            color = 'tomato') +
  labs(x = '', y = 'Daily return (%)') +
  ggtitle('Panel B: Minimum variance portfolio') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(12))

eff_plot <- performance %>% ggplot() +
  aes(x = date, y = EMBIG) +
  geom_line(color = 'black', size = 1) +
  geom_line(data = performance, 
            aes(x = date, y = EFF),
            color = 'tomato') +
  labs(x = '', y = 'Daily return (%)') +
  ggtitle('Panel C: Efficient portfolio') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(12))
  

eff_tc_plot <- performance %>% ggplot() +
  aes(x = date, y = EMBIG) +
  geom_line(color = 'black', size = 1) +
  geom_line(data = performance, 
            aes(x = date, y = EFF_TC),
            color = 'tomato') +
  labs(x = '', y = 'Daily return (%)') +
  ggtitle('Panel D: Cost-optimal efficient portfolio') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(12))

grid.arrange(ew_plot, mvp_plot, eff_plot, eff_tc_plot,
             nrow = 2)

##############################################################################
### PLOT 2 - EVALUATION OF ACCUMULATED WEALTH  ###
#############################################################################

wealth_initial <- 100

wealth <- performance %>% 
  mutate(wealth_bench  = (1 + EMBIG/100) * wealth_initial,
         wealth_ew     = (1 + EW/100) * wealth_initial,
         wealth_mvp    = (1 + MVP/100) * wealth_initial,
         wealth_eff    = (1 + EFF/100) * wealth_initial,
         wealth_eff_tc = (1 + EFF_TC/100) * wealth_initial)
 
for (i in 2:nrow(wealth)) {
  wealth$wealth_bench[i]    = (1 + wealth$EMBIG[i]/100) * wealth$wealth_bench[i-1]
  wealth$wealth_ew[i]       = (1 + wealth$EW[i]/100)    * wealth$wealth_ew[i-1]
  wealth$wealth_mvp[i]      = (1 + wealth$MVP[i]/100)   * wealth$wealth_mvp[i-1]
  wealth$wealth_eff[i]      = (1 + wealth$EFF[i]/100)   * wealth$wealth_eff[i-1]
  wealth$wealth_eff_tc[i]   = (1 + wealth$EFF_TC[i]/100)   * wealth$wealth_eff_tc[i-1]
}


wealth_plot <- wealth %>% select(date, starts_with("wealth")) %>%
  pivot_longer(cols = starts_with("wealth"), names_to = "strategy", values_to = "wealth") %>%
  ggplot(aes(x = date, y = wealth, color = strategy)) + 
  geom_line() +
  labs(x = '', y = 'Accumulated Wealth', color = 'Portfolio') + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    strip.text = element_text(size=14),
    legend.title = element_text(size=14),
    legend.text = element_text(size=14),
    legend.position = 'bottom',
    strip.background = element_blank(),) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) + 
  scale_color_manual(labels = c('EMBIG',
                                'Efficient', 
                                'Cost-optimized', 
                                'Equal-weighted', 
                                'Minimum variance'),
                     values = c('black','#F8766D','#7CAE00', '#00BFC4', '#C77CFF'))
  
##############################################################################
### PLOT 3 - EVALUATION OF EXCESS WEALTH  ###
#############################################################################

# excess return
excess_wealth <- wealth %>%
  mutate(excess_ew = wealth_ew - wealth_bench,
         excess_mvp = wealth_mvp - wealth_bench,
         excess_eff = wealth_eff - wealth_bench,
         excess_eff_tc = wealth_eff_tc - wealth_bench) %>% 
  select(date, starts_with("excess")) %>%
  pivot_longer(cols = starts_with("excess"), names_to = "strategy", values_to = "excess")

excess_wealth_plot <- excess_wealth %>%
  ggplot(aes(x = date, y = excess, color = strategy)) + 
  geom_line() +
  labs(x = '', y = 'Excess Wealth (%)', color = 'Portfolio') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12), 
    strip.text = element_text(size=12),
    legend.text = element_text(size=12),
    legend.position="bottom",
    strip.background = element_blank()) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_manual(labels = c('Efficient', 
                                'Cost-optimized', 
                                'Equal-weighted', 
                                'Minimum variance'),
                     values = c('#F8766D','#7CAE00', '#00BFC4', '#C77CFF'))

grid.arrange(wealth_plot, excess_wealth_plot, nrow = 2)

##############################################################################
### PLOT 4 - EVALUATION OF ACTIVE RETURN  ###
#############################################################################

# excess return
active_return <- performance %>%
  mutate(embig = EMBIG, 
         active_ew = EW - EMBIG,
         active_mvp = MVP - EMBIG,
         active_eff = EFF - EMBIG,
         active_eff_tc = EFF_TC - EMBIG) %>% 
  select(date, embig:active_eff_tc) %>%
  pivot_longer(cols = embig:active_eff_tc, names_to = "strategy", values_to = "active")

active_return %>%
  ggplot(aes(x = date, y = active, color = strategy)) + 
  geom_line() +
  labs(x = '', y = 'Active Return (%)', color = 'Portfolio') +
  theme(legend.position="bottom") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10))

active_return %>%
  group_by(strategy) %>% 
  summarise_at(vars(active),
               list(Mean            = ~250*mean(.), 
                    `Standard Dev.` = ~sqrt(250)*sd(.), 
                    `Sharpe Ratio`  = ~sqrt(250)*mean(.)/sd(.))) %>%
  arrange(desc(`Sharpe Ratio`))

##############################################################################
### EVALUATION OF RISK-ADJUSTED RETURN  ###
#############################################################################
performance %>% 
  pivot_longer(cols = EMBIG:EFF_TC, 
               names_to = "strategy", 
               values_to = "return") %>%
  group_by(strategy) %>% 
  summarise_at(vars(return),
               list(Mean            = ~250*mean(.), 
                    `Standard Dev.` = ~sqrt(250)*sd(.), 
                    `Sharpe Ratio`  = ~sqrt(250)*mean(.)/sd(.))) %>%
  arrange(desc(`Sharpe Ratio`))

performance %>% select(-date) %>% colMeans() * 250


##############################################################################
### COMPARISON OF MODELS  ###
#############################################################################
# ALTERNATIVE WINDOW!
#trading_start = '2015-07-02', 
#trading_end = '2018-12-31'

# STANDARD TRADING WINDOW!
#trading_start = '2019-01-02', 
#trading_end = '2022-06-30'

trading1 <- getPortfolios(model = 1, 
                          trading_start = '2015-07-01', 
                          trading_end = '2018-12-31')
trading2 <- getPortfolios(model = 2, 
                          trading_start = '2015-07-01', 
                          trading_end = '2018-12-31')
trading3 <- getPortfolios(model = 3, 
                          trading_start = '2015-07-01', 
                          trading_end = '2018-12-31')
trading4 <- getPortfolios(model = 4, 
                          trading_start = '2015-07-01', 
                          trading_end = '2018-12-31')
trading5 <- getPortfolios(model = 5, 
                          trading_start = '2015-07-01', 
                          trading_end = '2018-12-31')

portfolio1 <- lapply(
  trading1,
  as_tibble) %>%
  bind_rows(.id = "strategy") %>%
  select(strategy, Return) %>%
  transmute(strategy = strategy,
            return = Return * 100) %>%
  filter(strategy == 'EFF') %>%
  mutate(model = 'base') %>% select(-strategy)

portfolio2 <- lapply(
  trading2,
  as_tibble) %>%
  bind_rows(.id = "strategy") %>%
  select(strategy, Return) %>%
  transmute(strategy = strategy,
            return = Return * 100) %>%
  filter(strategy == 'EFF') %>%
  mutate(model = 'bcom') %>% select(-strategy)

portfolio3 <- lapply(
  trading3,
  as_tibble) %>%
  bind_rows(.id = "strategy") %>%
  select(strategy, Return) %>%
  transmute(strategy = strategy,
            return = Return * 100) %>%
  filter(strategy == 'EFF') %>%
  mutate(model = 'oil') %>% select(-strategy)

portfolio4 <- lapply(
  trading4,
  as_tibble) %>%
  bind_rows(.id = "strategy") %>%
  select(strategy, Return) %>%
  transmute(strategy = strategy,
            return = Return * 100) %>%
  filter(strategy == 'EFF') %>%
  mutate(model = '10yr') %>% select(-strategy)

portfolio5 <- lapply(
  trading5,
  as_tibble) %>%
  bind_rows(.id = "strategy") %>%
  select(strategy, Return) %>%
  transmute(strategy = strategy,
            return = Return * 100) %>%
  filter(strategy == 'EFF') %>%
  mutate(model = 'usd') %>% select(-strategy)

performance_eff <- benchmark %>%
  select(return = EMBIG) %>%
  mutate(model = "EMBIG") %>% 
  bind_rows(portfolio1, portfolio2, portfolio3, portfolio4, portfolio5) %>%
  group_by(model) %>%
  mutate(date = benchmark$Date) %>%
  pivot_wider(names_from = 'model',
              values_from = 'return')

daily_ret_eff <- performance_eff %>% 
  ggplot() + aes(x = date, y = base) + geom_line(color = '#F8766D') +
  geom_line(aes(x = date, y = bcom), color = '#00BFC4')   + 
  geom_line(aes(x = date, y = oil), color = '#C77CFF')    +
  geom_line(aes(x = date, y = `10yr`), color = '#7CAE00') +
  geom_line(aes(x = date, y = usd), color = 'steelblue') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12), 
    strip.text = element_text(size=12),
    legend.text = element_text(size=12),
    legend.position="off",
    strip.background = element_blank()) +
  labs(y = 'Daily return (%)', x = '') +
  scale_x_date(breaks = scales::breaks_pretty(12))

wealth_initial <- 100
wealth_eff <- performance_eff %>% 
  mutate(wealth_bench  = (1 + EMBIG/100) * wealth_initial,
         wealth_base   = (1 + base/100) * wealth_initial,
         wealth_bcom   = (1 + bcom/100) * wealth_initial,
         wealth_oil    = (1 + oil/100) * wealth_initial,
         wealth_10yr   = (1 + `10yr`/100) * wealth_initial,
         wealth_usd    = (1 + usd/100) * wealth_initial)

for (i in 2:nrow(wealth_eff)) {
  wealth_eff$wealth_bench[i] = (1 + wealth_eff$EMBIG[i]/100) * wealth_eff$wealth_bench[i-1]
  wealth_eff$wealth_base[i]  = (1 + wealth_eff$base[i]/100)  * wealth_eff$wealth_base[i-1]
  wealth_eff$wealth_bcom[i]  = (1 + wealth_eff$bcom[i]/100)  * wealth_eff$wealth_bcom[i-1]
  wealth_eff$wealth_oil[i]   = (1 + wealth_eff$oil[i]/100)   * wealth_eff$wealth_oil[i-1]
  wealth_eff$wealth_10yr[i]  = (1 + wealth_eff$`10yr`[i]/100) * wealth_eff$wealth_10yr[i-1]
  wealth_eff$wealth_usd[i]   = (1 + wealth_eff$usd[i]/100) * wealth_eff$wealth_usd[i-1]
}


wealth_eff_plot <- wealth_eff %>% 
  select(date, starts_with("wealth")) %>%
  pivot_longer(cols = starts_with("wealth"), names_to = "strategy", values_to = "wealth") %>%
  ggplot(aes(x = date, y = wealth, color = strategy)) + 
  geom_line() +
  labs(x = '', y = 'Accumulated Wealth', color = 'Model') + 
  theme_classic() +
  theme(
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 14), 
    strip.text = element_text(size=14),
    legend.text = element_text(size=14),
    legend.title = element_text(size=14),
    legend.position = 'bottom',
    strip.background = element_blank(),) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_manual(labels = c('US 10-year',
                                'Base Model', 
                                'Commodity Index', 
                                'EMBIG Index', 
                                'Brent Oil',
                                'USD Index'),
                     values = c('#7CAE00',
                                '#F8766D',
                                '#00BFC4', 
                                'black', 
                                '#C77CFF', 
                                'steelblue'))

grid.arrange(daily_ret_eff, wealth_eff_plot)

performance_eff %>% 
  pivot_longer(cols = EMBIG:usd, 
               names_to = "model", 
               values_to = "return") %>%
  group_by(model) %>% 
  summarise_at(vars(return),
               list(Mean            = ~250*mean(.), 
                    `Standard Dev.` = ~sqrt(250)*sd(.), 
                    `Sharpe Ratio`  = ~sqrt(250)*mean(.)/sd(.))) %>%
  arrange(desc(`Sharpe Ratio`))

active_return_eff <- performance_eff %>%
  mutate(embig = EMBIG, 
         active_base = base - EMBIG,
         active_bcom = bcom - EMBIG,
         active_oil  = oil - EMBIG,
         active_10yr = `10yr` - EMBIG,
         active_usd  = usd - EMBIG) %>% 
  select(date, embig:active_usd) %>%
  pivot_longer(cols = embig:active_usd, names_to = "strategy", values_to = "active")

active_return <- active_return_eff %>% group_by(strategy) %>% 
  summarise_at(vars(active),
               list(Mean            = ~250*mean(.), 
                    `Standard Dev.` = ~sqrt(250)*sd(.), 
                    `Sharpe Ratio`  = ~sqrt(250)*mean(.)/sd(.))) %>%
  arrange(desc(`Sharpe Ratio`))


#############################################################################
### calculate portfolio turnover ###
#############################################################################
EW_tib <- tibble(date = performance$date) %>% 
  cbind(trading$EW) %>%
  mutate(rebalance = case_when(
    month(date) > month(lag(date)) ~ 1,
    month(date) <= month(lag(date)) ~ 0,
    TRUE ~ 0
  ),
    turnover = abs(Africa - lag(Africa)) + 
    abs(Asia - lag(Asia)) + 
    abs(Europe - lag(Europe)) + 
    abs(`Latin America` - lag(`Latin America`)) +
    abs(`Middle East` - lag(`Middle East`))) %>%
  mutate(EW = 
           case_when(rebalance == 0 ~ 0,
                     rebalance == 1 ~ turnover)) %>%
  select(date, EW)

MVP_tib <- tibble(date = performance$date) %>% 
  cbind(trading$MVP) %>%
  mutate(rebalance = case_when(
    month(date) > month(lag(date)) ~ 1,
    month(date) <= month(lag(date)) ~ 0,
    TRUE ~ 0
  ),
  turnover = abs(Africa - lag(Africa)) + 
    abs(Asia - lag(Asia)) + 
    abs(Europe - lag(Europe)) + 
    abs(`Latin America` - lag(`Latin America`)) +
    abs(`Middle East` - lag(`Middle East`))) %>%
  mutate(MVP = 
           case_when(rebalance == 0 ~ 0,
                     rebalance == 1 ~ turnover)) %>%
  select(date, MVP)


EFF_tib <- tibble(date = performance$date) %>% 
  cbind(trading$EFF) %>%
  mutate(rebalance = case_when(
    month(date) > month(lag(date)) ~ 1,
    month(date) <= month(lag(date)) ~ 0,
    TRUE ~ 0
  ),
  turnover = abs(Africa - lag(Africa)) + 
    abs(Asia - lag(Asia)) + 
    abs(Europe - lag(Europe)) + 
    abs(`Latin America` - lag(`Latin America`)) +
    abs(`Middle East` - lag(`Middle East`))) %>%
  mutate(EFF = 
           case_when(rebalance == 0 ~ 0,
                     rebalance == 1 ~ turnover)) %>%
  select(date, EFF)

EFF_TC_tib <- tibble(date = performance$date) %>% 
  cbind(trading$EFF_TC) %>%
  mutate(rebalance = case_when(
    month(date) > month(lag(date)) ~ 1,
    month(date) <= month(lag(date)) ~ 0,
    TRUE ~ 0
  ),
  turnover = abs(Africa - lag(Africa)) + 
    abs(Asia - lag(Asia)) + 
    abs(Europe - lag(Europe)) + 
    abs(`Latin America` - lag(`Latin America`)) +
    abs(`Middle East` - lag(`Middle East`))) %>%
  mutate(EFF_TC = 
           case_when(rebalance == 0 ~ 0,
                     rebalance == 1 ~ turnover)) %>%
  select(date, EFF_TC)

tibble(EW = EW_tib$EW, MVP = MVP_tib$MVP, EFF = EFF_tib$EFF, EFF_TC = EFF_TC_tib$EFF_TC) %>% 
  colMeans() * 100

#############################################################################
### plot portfolio weights for the two efficent portfolios ###
#############################################################################

eff_weights <- cbind(Date = benchmark$Date, as_tibble(trading$EFF) %>% select(-Return)) %>%
  pivot_longer(-Date, names_to = 'Region', values_to = 'EFF') %>%
  left_join(rbind(cbind(Date = benchmark$Date, as_tibble(trading$EFF_TC) %>% select(-Return)) %>%
                    pivot_longer(-Date, names_to = 'Region', values_to = 'EFF_TC')),
            by = c('Date','Region'))
  
africa_w <- eff_weights %>% filter(Region == 'Africa') %>% 
  ggplot() +
  aes(x = Date, y = EFF * 100) +
  geom_line(color = '#F8766D', size = 1) +
  geom_line(data = eff_weights %>% filter(Region == 'Africa'), 
            aes(x = Date, y = EFF_TC * 100),
            color = '#7CAE00', size = 1) +
  labs(x = '', y = 'Weight (%)') +
  ggtitle('Panel A: Africa') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 12), 
    strip.background = element_blank(),
    strip.text = element_text(size=10))
 # scale_x_date(breaks = scales::breaks_pretty(12))

asia_w <- eff_weights %>% filter(Region == 'Asia') %>% 
  ggplot() +
  aes(x = Date, y = EFF * 100) +
  geom_line(color = '#F8766D', size = 1) +
  geom_line(data = eff_weights %>% filter(Region == 'Asia'), 
            aes(x = Date, y = EFF_TC * 100),
            color = '#7CAE00', size = 1) +
  labs(x = '', y = 'Weight (%)') +
  ggtitle('Panel B: Asia') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 12), 
    strip.background = element_blank(),
    strip.text = element_text(size=10))
  #scale_x_date(breaks = scales::breaks_pretty(12))

europe_w <- eff_weights %>% filter(Region == 'Europe') %>% 
  ggplot() +
  aes(x = Date, y = EFF * 100) +
  geom_line(color = '#F8766D', size = 1) +
  geom_line(data = eff_weights %>% filter(Region == 'Europe'), 
            aes(x = Date, y = EFF_TC * 100),
            color = '#7CAE00', size = 1) +
  labs(x = '', y = 'Weight (%)') +
  ggtitle('Panel C: Europe') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 12), 
    strip.background = element_blank(),
    strip.text = element_text(size=10))
  #scale_x_date(breaks = scales::breaks_pretty(12))

latam_w <- eff_weights %>% filter(Region == 'Latin America') %>% 
  ggplot() +
  aes(x = Date, y = EFF * 100) +
  geom_line(color = '#F8766D', size = 1) +
  geom_line(data = eff_weights %>% filter(Region == 'Latin America'), 
            aes(x = Date, y = EFF_TC * 100),
            color = '#7CAE00', size = 1) +
  labs(x = '', y = 'Weight (%)') +
  ggtitle('Panel D: Latin America') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none',
    axis.text = element_text(size = 12), 
    strip.background = element_blank(),
    strip.text = element_text(size=10))  
  #scale_x_date(breaks = scales::breaks_pretty(12))

mideast_w <- eff_weights %>% filter(Region == 'Middle East') %>% 
  ggplot() +
  aes(x = Date, y = EFF * 100) +
  geom_line(color = '#F8766D', size = 1) +
  geom_line(data = eff_weights %>% filter(Region == 'Middle East'), 
            aes(x = Date, y = EFF_TC * 100),
            color = '#7CAE00', size = 1) +
  labs(x = '', y = 'Weight (%)') +
  ggtitle('Panel E: Middle East') + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'bottom',
    axis.text = element_text(size = 12), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  #scale_x_date(breaks = scales::breaks_pretty(12)) +
  scale_color_manual(labels = c('Efficient', 
                                'Cost-optimized'),
                     values = c('#F8766D', '#7CAE00'))

grid.arrange(africa_w, asia_w, europe_w, latam_w, mideast_w)




