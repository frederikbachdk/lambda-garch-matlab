acf_plot <- function(x, region, lag.max = 30){
  
  # function to plot ACFs.
  # input: TBC
  # output: TBC
  x <- x %>% filter(Region == region) %>%
    select(Return)

  # filter data based on user choice
  acf <- x %>% na.omit() %>% 
    acf(lag.max = lag.max, plot = FALSE) # create ACF object
  
  # save ACF data
  x_acf <- acf$acf
  x_acf_N <- acf$n.used
  
  # calculate confidence bands
  acf_tib <- tibble(
    lag = seq(0,length(x_acf)-1),
    ACF = x_acf) %>%
    mutate(
      acf_lowerbound = - 2/sqrt(x_acf_N),
      acf_upperbound = 2/sqrt(x_acf_N))
  
  # plot 
  acf_tib %>% filter(lag > 0) %>% 
    ggplot() + aes(x = lag, y = ACF) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(y = (ACF + 0.0025), xend = lag, yend = 0),
                 size = 3.5,
                 color = 'black') +
    geom_segment(mapping = aes(y = (ACF - 0.0025), xend = lag, yend = 0),
                 size = 3.5,
                 color = 'black') +
    geom_segment(mapping = aes(xend = lag, yend = 0),
                 size = 2,
                 color = 'red') +
    geom_line(aes(x = lag, y = acf_lowerbound), 
              color = 'chartreuse3', 
              size = 1,) +
    geom_line(aes(x = lag, y = acf_upperbound),  
              color = 'chartreuse3',
              size = 1) + 
    scale_x_continuous(breaks = round(seq(0, lag.max, by = 2),1)) + 
    theme_classic() +
    ggtitle(paste0(region)) +
    xlab('Lag number') +
    theme(text = element_text(color="black"),
          axis.text=element_text(color="black"),
          plot.title = element_text(hjust = 0.5))
}
