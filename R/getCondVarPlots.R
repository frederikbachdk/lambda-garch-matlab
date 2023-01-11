source('R/utils/plotsFunctions.R')

###############################################################################
# conditional variances - construct dataframe
###############################################################################

# import conditional data as lists
condDynamics1 <- readRDS('data/conditionalDynamics1.rds')
condDynamics2 <- readRDS('data/conditionalDynamics2.rds')
condDynamics3 <- readRDS('data/conditionalDynamics3.rds')
condDynamics4 <- readRDS('data/conditionalDynamics4.rds')
condDynamics5 <- readRDS('data/conditionalDynamics5.rds')

# take out the conditional covariance matrices
condVar1 <- condDynamics1$condCovar
condVar2 <- condDynamics2$condCovar
condVar3 <- condDynamics3$condCovar
condVar4 <- condDynamics4$condCovar
condVar5 <- condDynamics5$condCovar

# remove redundant objects
rm(condDynamics1, condDynamics2, condDynamics3, condDynamics4, condDynamics5)

# transform each condVar list to tibble format
index <- c(1, 2, 3, 4, 5,
           7, 8, 9, 10,
           13, 14, 15,
           19, 20,
           25)

condVar1_tib <- names(condVar1) %>% tibble() %>% rename(Date = ".")
condVar2_tib <- names(condVar2) %>% tibble() %>% rename(Date = ".")
condVar3_tib <- names(condVar3) %>% tibble() %>% rename(Date = ".")
condVar4_tib <- names(condVar4) %>% tibble() %>% rename(Date = ".")
condVar5_tib <- names(condVar5) %>% tibble() %>% rename(Date = ".")


# Extract variances and covariances
for(i in index){
  iter1 <- map(condVar1, i) %>% as.matrix() %>% unlist()
  condVar1_tib <- cbind(condVar1_tib, iter1)
  
  iter2 <- map(condVar2, i) %>% as.matrix() %>% unlist()
  condVar2_tib <- cbind(condVar2_tib, iter2)

  iter3 <- map(condVar3, i) %>% as.matrix() %>% unlist()
  condVar3_tib <- cbind(condVar3_tib, iter3)

  iter4 <- map(condVar4, i) %>% as.matrix() %>% unlist()
  condVar4_tib <- cbind(condVar4_tib, iter4)

  iter5 <- map(condVar5, i) %>% as.matrix() %>% unlist()
  condVar5_tib <- cbind(condVar5_tib, iter5)
}

# Name tibbles
names(condVar1_tib) <- c('Date', 
                         'Var_Africa1', 'Cov_Africa_Asia1', 'Cov_Africa_Europe1', 'Cov_Africa_LatAm1', 'Cov_Africa_MidEast1',
                         'Var_Asia1', 'Cov_Asia_Europe1', 'Cov_Asia_LatAm1', 'Cov_Asia_MidEast1',
                         'Var_Europe1', 'Cov_Europe_LatAm1', 'Cov_Europe_MidEast1',
                         'Var_LatAm1', 'Cov_LatAm_MidEast1',
                         'Var_MidEast1')

names(condVar2_tib) <- c('Date', 
                         'Var_Africa2', 'Cov_Africa_Asia2', 'Cov_Africa_Europe2', 'Cov_Africa_LatAm2', 'Cov_Africa_MidEast2',
                           'Var_Asia2', 'Cov_Asia_Europe2', 'Cov_Asia_LatAm2', 'Cov_Asia_MidEast2',
                           'Var_Europe2', 'Cov_Europe_LatAm2', 'Cov_Europe_MidEast2',
                           'Var_LatAm2', 'Cov_LatAm_MidEast2',
                           'Var_MidEast2')

names(condVar3_tib) <- c('Date', 
                         'Var_Africa3', 'Cov_Africa_Asia3', 'Cov_Africa_Europe3', 'Cov_Africa_LatAm3', 'Cov_Africa_MidEast3',
                         'Var_Asia3', 'Cov_Asia_Europe3', 'Cov_Asia_LatAm3', 'Cov_Asia_MidEast3',
                         'Var_Europe3', 'Cov_Europe_LatAm3', 'Cov_Europe_MidEast3',
                         'Var_LatAm3', 'Cov_LatAm_MidEast3',
                         'Var_MidEast3')

names(condVar4_tib) <- c('Date', 
                         'Var_Africa4', 'Cov_Africa_Asia4', 'Cov_Africa_Europe4', 'Cov_Africa_LatAm4', 'Cov_Africa_MidEast4',
                         'Var_Asia4', 'Cov_Asia_Europe4', 'Cov_Asia_LatAm4', 'Cov_Asia_MidEast4',
                         'Var_Europe4', 'Cov_Europe_LatAm4', 'Cov_Europe_MidEast4',
                         'Var_LatAm4', 'Cov_LatAm_MidEast4',
                         'Var_MidEast4')
        
names(condVar5_tib) <- c('Date', 
                         'Var_Africa5', 'Cov_Africa_Asia5', 'Cov_Africa_Europe5', 'Cov_Africa_LatAm5', 'Cov_Africa_MidEast5',
                         'Var_Asia5', 'Cov_Asia_Europe5', 'Cov_Asia_LatAm5', 'Cov_Asia_MidEast5',
                         'Var_Europe5', 'Cov_Europe_LatAm5', 'Cov_Europe_MidEast5',
                         'Var_LatAm5', 'Cov_LatAm_MidEast5',
                         'Var_MidEast5')


# bind all tibbles and pivot
condVar_tib <- cbind(condVar1_tib, 
                     condVar2_tib %>% select(-Date),
                     condVar3_tib %>% select(-Date),
                     condVar4_tib %>% select(-Date),
                     condVar5_tib %>% select(-Date)) %>%
  pivot_longer(cols = Var_Africa1:Var_MidEast5,
               values_to = 'condVar',
               names_to  = 'model') %>%
  mutate(Date = as.Date(Date))

rm(list=setdiff(ls(), "condVar_tib"))

###############################################################################
# conditional variances - plot data
###############################################################################

condVar_tib %>% filter(grepl('Var_Africa', model),
                       Date <= as.Date('2019-12-31')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = 'Conditional Variance') + 
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    legend.position = 'bottom') +
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909"),
                     labels = unname(TeX(c("$\\lambda-GARCH$", 
                                           "$\\lambda-GARCH-X$, Commod.", 
                                           "$\\lambda-GARCH-X$ Oil",
                                           "$\\lambda-GARCH-X$ 10-year",
                                           "$\\lambda-GARCH-X$ USD"))),
                     name = 'Model') +
  guides(colour = guide_legend(override.aes = list(size=4)))




Var_Asia <- condVar_tib %>% filter(grepl('Var_Asia', model),
                                   Date <= as.Date('2019-12-31')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = '') +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    legend.position = 'none') +
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909")) +
  guides(colour = guide_legend(override.aes = list(size=4)))


Var_Europe <- condVar_tib %>% filter(grepl('Var_Europe', model),
                                     Date <= as.Date('2019-12-31')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = 'Conditional Variance') +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    legend.position = 'none') + 
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909")) +
  guides(colour = guide_legend(override.aes = list(size=4)))


Var_LatAm <- condVar_tib %>% filter(grepl('Var_LatAm', model),
                                     Date <= as.Date('2019-12-31')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = '') +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    legend.position = 'none') +
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909")) +
  guides(colour = guide_legend(override.aes = list(size=4)))


Var_MidEast <- condVar_tib %>% filter(grepl('Var_MidEast', model),
                                    Date <= as.Date('2019-12-31')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = '') +
  theme_classic() + 
  theme(
    axis.text = element_text(size = 10), 
    strip.background = element_blank(),
    strip.text = element_text(size=10),
    legend.position = 'none') +
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909"))



###############################################################################
# conditional variances - plot data for analysis
###############################################################################

# LATAM VARIANCE
condVar_tib %>% filter(grepl('Var_LatAm', model),
                       Date >= as.Date('2020-01-01'),
                       Date <= as.Date('2020-06-30')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = 'Conditional Variance') + 
  theme_classic() +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909"),
                     labels = unname(TeX(c("$\\lambda$-GARCH", 
                                           "$\\lambda$-GARCH-X, Commod.", 
                                           "$\\lambda$-GARCH-X Oil",
                                           "$\\lambda$-GARCH-X 10-year",
                                           "$\\lambda$-GARCH-X USD"))),
                     name = 'Model') +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size=16),
    legend.position = 'bottom',
    ) +
  scale_x_date(breaks = scales::breaks_pretty(10))

ggsave('condVar_LatAm.png', dpi = 'retina',
       path = 'plots/')


# MIDDLE EAST VARIANCE
condVar_tib %>% filter(grepl('Var_MidEast', model),
                       Date >= as.Date('2020-01-01'),
                       Date <= as.Date('2020-06-30')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = 'Conditional Variance') + 
  theme_classic() +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909"),
                     labels = unname(TeX(c("$\\lambda$-GARCH", 
                                           "$\\lambda$-GARCH-X, Commod.", 
                                           "$\\lambda$-GARCH-X Oil",
                                           "$\\lambda$-GARCH-X 10-year",
                                           "$\\lambda$-GARCH-X USD"))),
                     name = 'Model') +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text = element_text(size=16),
    legend.position = 'bottom',
  ) +
  scale_x_date(breaks = scales::breaks_pretty(10))

ggsave('condVar_MidEast.png', dpi = 'retina',
       path = 'plots/')


# EUROPE / MIDDLE EAST COVARIANCE
condVar_tib %>% filter(grepl('Cov_Europe_MidEast', model),
                       Date >= as.Date('2020-01-01'),
                       Date <= as.Date('2020-12-30')) %>%
  ggplot() + aes(x = Date, y = condVar, color = model) + geom_line() +
  labs(x = '', y = 'Conditional Covariance') + 
  xlim(as.Date('2020-01-01'), as.Date('2020-12-30')) +
  theme_classic() +
  scale_color_manual(values = c("#4682b4",
                                "#1b98e0",
                                "#3630ff",
                                "#085a05",
                                "#a00909"),
                     labels = unname(TeX(c("$\\lambda$-GARCH", 
                                           "$\\lambda$-GARCH-X, Commod.", 
                                           "$\\lambda$-GARCH-X Oil",
                                           "$\\lambda$-GARCH-X 10-year",
                                           "$\\lambda$-GARCH-X USD"))),
                     name = 'Model') +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.background = element_blank(),
    strip.text = element_text(size=16),
    legend.position = 'bottom',
  ) +
  scale_x_date(
    date_labels = "%m-%y",
    breaks = scales::breaks_pretty(10))

ggsave('condCov_LatAm_MidEast.png', dpi = 'retina',
       path = 'plots/')
