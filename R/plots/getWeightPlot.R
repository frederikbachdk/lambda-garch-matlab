library(tidyverse)
library(scales)
source('R/utils/getWeights.R')

condDynamics <- readRDS('data/conditionalDynamics5.rds')
Omega <- condDynamics$condCovar[['2019-01-02']]

data <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'DATA_CLEAN') %>%
  filter(Date >= as.Date('2018-12-01'),
         Date <= as.Date('2019-01-01')) %>%
  select(Africa:'EMBIG Div')

embig <- readxl::read_excel('data/13102022_data.xlsx', sheet = 'WEIGHTS_CLEAN') %>%
  filter(Date == as.Date('2019-01-02')) %>% select(-Date) %>% 
  as.numeric()

mu <- data %>% select(Africa:'Middle East') %>% colMeans() %>% as.matrix()
rm(condDynamics, data)

###############################################################################

w_ew <- equalWeights(n = 5)
w_mvp <- minimumVarWeights(Omega)
w_eff <- efficientWeights_costOpt(Omega, mu, gamma = 4, w_mvp, beta = 0)
w_eff_tc <- efficientWeights_costOpt(Omega, mu, gamma = 4, w_mvp, beta = 0.5)

tibble(
  `EMBIG` = embig/100,
  `Equal-Weighted` = w_ew,
  `Minimum Variance` = w_mvp,
  `Efficient` = w_eff,
  `Cost-Optimized` = w_eff_tc,
  Region = rownames(mu)
) |>
  pivot_longer(-Region,
               names_to = "Strategy",
               values_to = "weights"
  ) |>
  ggplot(aes(
    fill = Strategy,
    y = weights,
    x = Region
  )) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_flip() +
  labs(
    y = "Portfolio weight", fill = NULL
  ) + 
  scale_y_continuous(limits = c(-2,2),
                     labels = percent,
                     breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)) +
  theme_classic() +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        )

