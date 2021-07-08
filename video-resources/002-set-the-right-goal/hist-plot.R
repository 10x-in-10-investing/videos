

# Purpose:
#   create a density plot for BRK and S&P 500 annual log returns

# Data source:
#   data derived from 'https://berkshirehathaway.com/letters/2020ltr.pdf'



library(tidyverse)
library(magrittr)


d <- read_csv(here::here('brk-sp500-annualreturns.csv'))

# center returns
d %<>%
  gather(key = 'symbol',
         value = 'log_return',
         -Year) %>%
  group_by(symbol) %>%
  mutate(centered_log_return = log_return - mean(log_return)) %>%
  ungroup()

# create plot (facet_wrap)
fill_col <- '#ae6de4'
x10_year <- seq(from = 1965, to = 1994, by = 1)
gg <- d %>%
  dplyr::filter(Year %in% x10_year) %>%
  ggplot(mapping = aes(x = centered_log_return)) + 
  geom_histogram(fill = fill_col, color = 'black', size = 2) +
  ggpubr::theme_transparent() +
  facet_wrap(~ symbol, ncol = 1)

# save
ggsave(gg,
       filename = 'hist-plot-wrap.png',
       bg = 'transparent')




