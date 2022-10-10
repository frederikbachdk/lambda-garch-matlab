# load libraries
library(tidyverse)
library(matlib)
library(tictoc)
library(quadprog)
library(scales)
library(alabama)
library(lemon)

# turn off scinumbers, clear console and memory
options(scipen=999) 
cat('\014')
rm(list=ls())

source('getPortfolios.R')

#############################################################################
### illustrate portfolio weights ###
#############################################################################

tibble(
  'Naive' = trading$EW[1,1:5],
  'MVP' = trading$MVP[1,1:5],
  'Tangent' = trading$TAN[1,1:5],
  'Tangent (NTC)' = trading$NTC[1,1:5],
  Region = x_trading %>% select(-Date, -rebalance) %>% colnames()
) %>%
  pivot_longer(-Region,
               names_to = "strategy",
               values_to = "weights"
  ) %>%
  ggplot(aes(
    fill = strategy,
    y = weights,
    x = Region
  )) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(y = "Allocation weight", fill = NULL) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  theme(legend.position = "bottom")


#############################################################################
### illustrate portfolio performance ###
#############################################################################
# combine lists
portfolios <- lapply(
  trading,
  as_tibble) %>%
  bind_rows(.id = "strategy") %>%
  select(strategy, return)

# append EMBIG return
performance <- benchmark %>%
  select(return = EMBIG) %>%
  mutate(strategy = "EMBIG",
         return = return / 100) %>%
  bind_rows(portfolios) %>%
  group_by(strategy) %>%
  mutate(date = benchmark$Date)

performance %>%
  subset(strategy != "EW") %>%
  ggplot(aes(x = date, y = return, color = strategy)) + 
  geom_line() + 
  theme(legend.position="none") +
  labs(x = '', y = 'Realized Return (USD)') +
  theme_classic() +
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10))

ggsave('portfolio_returns.png', dpi = 'retina',
       path = 'plots/')

#############################################################################
### illustrate portfolio allocated wealth w/ initial endowment ###
#############################################################################
# wealth_initial <- 100

test <- performance %>% 
  pivot_wider(names_from = strategy, values_from = return) %>%
  mutate(wealth_bench = (1 + EMBIG) * wealth_initial,
         wealth_ew = (1 + EW) * wealth_initial,
         wealth_mvp = (1 + MVP) * wealth_initial,
         wealth_tan = (1 + TAN) * wealth_initial,
         wealth_ntc = (1 + NTC) * wealth_initial)
 
for (i in 2:nrow(test)) {
  test$wealth_bench[i] = (1 + test$EMBIG[i]) * test$wealth_bench[i-1]
  test$wealth_ew[i] = (1 + test$EW[i]) * test$wealth_ew[i-1]
  test$wealth_mvp[i] = (1 + test$MVP[i]) * test$wealth_mvp[i-1]
  test$wealth_tan[i] = (1 + test$TAN[i]) * test$wealth_tan[i-1]
  test$wealth_ntc[i] = (1 + test$NTC[i]) * test$wealth_ntc[i-1]
}


test %>% select(date, starts_with("wealth")) %>%
  pivot_longer(cols = starts_with("wealth"), names_to = "strategy", values_to = "wealth") %>%
  ggplot(aes(x = date, y = wealth, color = strategy)) + 
  geom_line() + 
  #facet_rep_wrap(~ strategy, nrow = 4, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = 'Accumulated Wealth (USD)', color = 'Portfolio') + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_jcolors(palette = "pal7")

ggsave('portfolio_wealth.png', dpi = 'retina',
       path = 'plots/')

#############################################################################
### illustrate portfolio wealth excess EMBIG ###
#############################################################################
# excess return
excess_wealth <- test %>%
  mutate(excess_ew = wealth_ew - wealth_bench,
         excess_mvp = wealth_mvp - wealth_bench,
         excess_tan = wealth_tan - wealth_bench,
         excess_ntc = wealth_ntc - wealth_bench) %>% 
  select(date, starts_with("excess")) %>%
  pivot_longer(cols = starts_with("excess"), names_to = "strategy", values_to = "excess")

excess_wealth %>%
  ggplot(aes(x = date, y = excess, color = strategy)) + 
  geom_line() +
  #facet_rep_wrap(~ strategy, nrow = 2, repeat.tick.labels = TRUE) +
  theme(legend.position="none") +
  labs(x = '', y = 'Excess Wealth (USD)', color = 'Portfolio') + 
  theme_classic() +
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10)) + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_jcolors(palette = "pal7")

ggsave('excess_wealth.png', dpi = 'retina',
       path = 'plots/')
