# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose ----
#   To estimate the expected range of the number of 10x in 10 companies based
#   on the detrended fitted real price-to-earnings ratio to broadly support the
#   idea the number of 10x in 10 opportunities changes based on this metric.
#
# Detail on how the number of 10x in 10 companies was determined ----
#   Note: See R script, "10x-indicator-setup-not4github.R", for calculation of
#         10x in 10 stocks and the output of the summary values used as data in
#         this analysis.  The 10x in 10 data was generated using data available
#         on July 9, 2021.  This script is not available publicly as it depends
#         on data not licensed for redistribution.
#
#   The potential 10x in 10 company universe begins with all publicly traded common
#   stocks traded on the NYSE, NASDAQ and AMEX exchanges as of July 9, 2021,
#   excluding the following:
#
#     -- non-U.S. stocks
#     -- market capilization < $300M
#     -- utility and finance companies (other than real estate)
#     -- publically trade partnerships (PTP's)
#
#   In addition, minimum of 5-years of data required with a current close > $10.
#   Prices were adjusted for splits, but not dividends.  10x occurs any day
#   a closing price is 10 times more than any previous closing date during the
#   preceding 10 years.  A 10x in 10 stock is a stock that has 10x'd at least 21
#   times with at least 21 opportunities to exit.  In English, the stock must
#   have had at least 21 opportunities to get in and out of the investment.
#
#   Data analyzed goes back to 2001.  Therefore, the fully realized 10x in 10
#   periods run from March 2002 through March 2011.
#   
#   
#  Main dataset ----
#   The main dataset is a CSV file called, "003-10x-in-10-market-guide-data.csv'.
#   It has the following fields:
#     -- fiscal year (April - March)
#     -- detrended_fitted_pe_1946 (explained below)
#     -- detrended_fitted_pd_1972 (explained below)
#     -- n_10x (The is the number of 10x in 10 companies as defined above)
#
#   The P/E Ratio is calculated using the Excel workbook,
#   10x-in-10-market-guide-tool.xlsx".  See the Youtube video (https://youtu.be/ixSErDDgccM)
#   for details on the calculation.
#
# The output ----
#   The output consists of two plots summarizing the result of the Bayesian posterior
#   distribution estimating the range of the number of 10x in 10 companies given
#   the data (number of 10x in 10 companies | detrended fitted P/E).  The user
#   can define whether to save the plot to a *.png file.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries and data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
{
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  
  # read data
  d <- read_csv('003-10x-in-10-market-guide-data.csv')
  
  # data constants
  last_full_year <- 2011.03
  complete_data_only <- FALSE  # FALSE = use all data, including partially completed years
  year_start <- 1972           # c(1946, 1972)
  
  # Highest posterior density interval (HPDI)
  hpdi_prob <- 0.9
  
  # save plots?
  save_plots <- TRUE
  

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model (negative Binomial ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
{
  library(rstan)
  
  # setup 
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores() - 2)
  
  # stan model 
  model_name <- c('003-neg-binomial')
  filename <- paste(model_name, '.stan', sep = '')
  
  stan_mod <- stan_model(file = filename)
  
  # sampling parameters
  chains <- 10 
  iter <- 6000
  warmup <- 1000
  
  # control
  ctrl <- list(adapt_delta = 0.95,
               max_treedepth = 15)
  
  # fit
  train <- d %>%
    drop_na() %>%
    mutate(years_missing = pmax(0, fiscal_year - last_full_year) - 0.5)
  
  if(complete_data_only) {
    train %<>% filter(years_missing == -0.5)
  }
  
  pe_col <- paste('detrended_fitted_pe', year_start, sep = '_')
  
  plus_minus <- 15
  N_2fit <- plus_minus * 2 * 10 + 1
  x_2fit <- seq(from = -plus_minus, to = plus_minus, length.out = N_2fit)
  ls <- list(N = nrow(train),
             y = train$n_10x %>% as.integer(),
             x = train %>% select(all_of(pe_col), years_missing) %>% as.matrix(),
             
             N_2fit = N_2fit,
             x_2fit = matrix(c(x_2fit,
                             rep(-0.5, N_2fit)),
                             nrow = N_2fit, ncol = 2))
  
  fit <- stan(file = filename,
              data = ls,
              iter = iter,
              warmup = warmup,
              control = ctrl,
              chains = chains)
  
  # print summaries of posterior
  theta_fit <- rstan::extract(fit)
  
  summary(theta_fit$alpha) %>% print()
  summary(theta_fit$beta[, 1]) %>% print()
  summary(theta_fit$beta[, 2]) %>% print()
  summary(theta_fit$reciprocal_phi) %>% print()
  summary(theta_fit$phi) %>% print()
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate plots and save ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
{
  y_fit <- rstan::extract(fit, pars = "y_rep")
  
  y_fit_hpdi <- map_df(.x = 1:N_2fit,
                   .f = function(x) {
                     z <- rethinking::HPDI(y_fit$y_rep[, x], prob = hpdi_prob)
                     return(tibble(x_hat = x_2fit[x],
                                   ll = z[1],
                                   mean = mean(y_fit$y_rep[, x]),
                                   ul = z[2]))
                    
                   })
  
  y_observed <- train %>% dplyr::filter(years_missing == -0.5)
  
  
  main_col <- '#ae6de4'
  minor_col <- '#e9e8e2'
  
  gg <-  y_fit_hpdi %>%
    ggplot(aes(x = x_hat)) +
    geom_line(aes(y = ll), stat = 'smooth', color = main_col) +
    geom_line(aes(y = ul), stat = 'smooth', color = main_col) +
    geom_line(aes(y = mean), stat = 'smooth', color = minor_col) +
    ggtitle(label = '10x in 10 Market Guide',
            subtitle = '2002 - 2011') + 
    xlab(label = paste('Detrended Real Fitted P/E Ratio (', year_start, ')', sep = '')) +
    ylab(label = 'Number of 10x in 10 Companies') +
    theme_minimal() +
    theme(axis.text.x = element_text(color = main_col, size = 16),
          axis.text.y = element_text(color =  main_col, size = 16),
          axis.title.x = element_text(color = main_col, size = 18),
          axis.title.y = element_text(color = main_col, size = 18),
          title = element_text(color = main_col, size = 18),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA)) +
    scale_x_continuous(breaks = seq(from = -plus_minus, to = plus_minus, by = 5))
    
  gg_build <- ggplot_build(gg)
  
  df_gg <- tibble(x = gg_build$data[[1]]$x,
                  ymin = gg_build$data[[1]]$y,
                  ymax = gg_build$data[[2]]$y,
                  y = gg_build$data$data[[3]]$y)
  
  gg_final <- gg + geom_ribbon(data = df_gg,
                   aes(x = x, ymin = ymin, ymax = ymax),
                fill = minor_col, alpha = 0.05) +
    geom_point(data = tibble(x_obs = y_observed %>% pull(all_of(pe_col)),
                             y_obs = y_observed$n_10x,
                             fye = y_observed$fiscal_year),
               aes(x = x_obs, y = y_obs),
               color = main_col) +
    geom_text(data = tibble(x_obs = y_observed %>% pull(all_of(pe_col)),
                            y_obs = y_observed$n_10x,
                            fye = as.integer(y_observed$fiscal_year)),
              aes(x = x_obs, y = y_obs, label = fye),
              color = main_col, size = 4, hjust = -0.1) 
}

{
  # show plots
  gg_final +
    scale_y_log10() 
  
  gg_final
    
}

{
  if(save_plots) {
    
    (gg_final +
       scale_y_log10()) %>%
      ggsave(filename = paste('003-10x-guide-plot-log-',
                              year_start, '-',
                              if_else(complete_data_only, 'complete-only', 'all-data'),
                              '.png', sep = ''),
             width = 16,
             height = 9,
             units = 'in',
             device = 'png',
             bg = 'transparent')
    
    gg_final %>%
      ggsave(filename = 
               paste('003-10x-guide-plot-',
                     year_start, '-',
                     if_else(complete_data_only, 'complete-only', 'all-data'),
                     '.png', sep = ''),
             width = 16,
             height = 9,
             units = 'in',
             device = 'png',
             bg = 'transparent')
    
  }
  
  
}

{
  # print some key summary value 
  key_values <- seq(from = -plus_minus, to = plus_minus, by = 5)
  y_fit_hpdi %>% filter(x_hat %in% key_values) %>% print()
  
  
}




