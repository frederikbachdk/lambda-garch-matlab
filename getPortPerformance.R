# load libraries
library(tidyverse)
library(matlib)
library(tictoc)
library(quadprog)
library(scales)
library(alabama)

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
  Region = x_trading %>% select(-Date, -month, -rebalance) %>% colnames()
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
  mutate(strategy = "EMBIG") %>%
  select(strategy, EMBIG) %>%
  rename(return = EMBIG) %>%
  mutate(return = return / 100) %>%
  bind_rows(portfolios) %>%
  group_by(strategy) %>%
  mutate(id = 1:n())

performance %>%
  ggplot(aes(x = id, y = return, color = strategy)) + 
  geom_line() + 
  theme_classic()
