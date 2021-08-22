# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose ----
#   To estimate the expected total excess return of the S&P 500 index using real
#   price to real normalized earnings as the predictor.
#
# The output ----
#   Plot showing where observed outcomes fit within the expected predicted range
#   along with the predicted future outcome based on the current values and
#   related posterior estimates.
#
# Overview ----
#   Total excess return is equal to the sum of the S%P 500 index return plus
#   the dividend yield less return on cash.  The return on cash is measured
#   as the 1-year U.S. Treasury rate.  Often, a shorter-term, such as the 30-day
#   or 90-day rate is used. Download Excel file, Historical-Treasury-Rates.xlsx
#   from 10x in 10 Investing Tools GitHub for source of 1-year rates
#   (https://github.com/10x-in-10-investing/tools).
#
#   (Assumed) observed values include nominal and real S&P 500 index values, 
#   nominal dividends, 1-year U.S. Treasury rates and real S&P 500 earnings.
#   Calculations that can be derived from these observations are considered
#   observed as well.  These calculations include nominal S&P 500 index returns,
#   dividend yields and total excess return of the S&P 500.
#
#   Normalized real earnings along with model parameters are unknown and estimated.
#
#   A single hierarchical linear model has been created.  First, there is a
#   definite steady increase in the real earnings growth rate (cause unknown);
#   so, a linear transformation of an exponential model is used to estimate the
#   the normalized real earnings.  This in turn infers a real price to normalized
#   real earning ratio, which is used to fit to observed total excess return
#   of the S&P 500 index
#   
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libraries and data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
{
  library(tidyverse)
  library(magrittr)
  library(rstan)
  #library(shinystan)
  #library(rethinking)
  
  # read data
  d <- read_csv('shiller-data-augmented.csv') %>%
    select(date = Date,
           price = P,
           dividend = D,
           earnings = E,
           year = Year,
           month = Month,
           real_earnings = `Real EPS`,
           real_price = `Real Price`)
  
  rfr <- read_csv('rfr-short-term.csv')
  
  d %<>%
    left_join(x = .,
              y = rfr,
              by = c('year' = 'Year',
                     'month' = 'Month')) %>%
    rename(cash_rate = Best_Est) %>%
    mutate(cash_rate = cash_rate / 100) %>%
    group_by(month) %>%
    mutate(index = row_number()) %>%
    mutate(log_index_return = if_else(index == 1, 0, log(price / lag(price))),
           log_dividend_yield = if_else(index == 1, 0, log(1 + dividend / price)),
           log_cash_yield = if_else(index == 1, 0, log(1 + lag(cash_rate)))) %>%
    mutate(agg_log_index_return = cumsum(log_index_return),
           agg_log_dividend_yield = cumsum(log_dividend_yield),
           agg_log_cash_yield = cumsum(log_cash_yield)) %>%
    select(index, everything()) %>%
    ungroup()
  
  
  # current (i.e. last) data point
  curr_d <- d %>%
    filter(index == max(index)) %>%
    filter(month == max(month)) %>%
    select(index, date, year, month, real_price)
  
  # Since current value is dropped from model as future return is unknown,
  # current index and current real price can be overridden.  Since
  # real_price = today's price, a date not too far from the last data point can
  # be used without needing to restate real prices and real earnings.  
  
  use_most_current <- TRUE # c(TRUE = Use most recent data below, FALSE)
  if(use_most_current) {
    curr_d$index <- curr_d$index + 4.5/12  # Period end is 8/13/2021 (which is 4.5 months past March 31st)
    curr_d$real_price <- 4394.12           # 1-month average closing index value
  }
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
{
  #library(rstan)
  {
    # setup 
    rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores() - 2)
    
    # stan model 
    model_name <- c('004D-model-exp-earnings-linear-ratio')
    filename <- paste(model_name, '.stan', sep = '')
    
    stan_mod <- stan_model(file = filename)
    
    # sampling parameters
    chains <- 10 
    iter <- 5000
    warmup <- 2000
    
    # control
    ctrl <- list(adapt_delta = 0.95,
                 max_treedepth = 15)
  }
  
  # fit
  {
    per_ahead <- 10  #c(1, 5, 10 ...)
    
    train <- d %>%
      filter(month == curr_d$month) %>%
      mutate(y_total_excess_return = exp((lead(agg_log_index_return, per_ahead) - agg_log_index_return +
                                            lead(agg_log_dividend_yield, per_ahead) - agg_log_dividend_yield -
                                            (lead(agg_log_cash_yield, per_ahead) - agg_log_cash_yield)) / per_ahead) - 1) %>%
      drop_na()
    
    ls <- list(N = nrow(train),
               index = train$index,
               y_earnings = log(train$real_earnings),
               y_price = log(train$real_price),
               y_futurereturn = log(1 + train$y_total_excess_return),
               
               # prior sigmas
               prior_alpha_mu = 2, prior_alpha_sig = 0.5,
               prior_beta_mu = 0, prior_beta_sig = 0.01,
               prior_sigma = 0.25,
               prior_alpha_return_mu = 0.5, prior_alpha_return_sig = 0.5,
               prior_beta_return_mu = 0, prior_beta_return_sig = 0.1,
               prior_sigma_return = 0.25,
               
               # predict current
               curr_index = curr_d$index,
               curr_price = log(curr_d$real_price)
    )
    
    
    fit <- stan(file = filename,
                data = ls,
                iter = iter,
                warmup = warmup,
                control = ctrl,
                chains = chains)
    
  }
  
  #shinystan::launch_shinystan(fit)
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate some plots ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
{
  
  {
    hpdi_prob = 0.9
    y_pred <- rstan::extract(fit, pars = c('y_return_pred', 'log_ratio', 'curr_return_pred', 'curr_log_ratio'))
    
    y_pred_hpdi <- map_df(.x = 1:ncol(y_pred$y_return_pred),
                          .f = function(x) {
                            z <- rethinking::HPDI(y_pred$y_return_pred[, x], prob = hpdi_prob)
                            
                            mu_return <- mean(y_pred$y_return_pred[, x])
                            
                            
                            d <- tibble(x = mean(y_pred$log_ratio[, x]),
                                        ll = z[1],
                                        ul = z[2],
                                        mean = mu_return)
                            return(d)
                            
                          })
    
    curr_pred_hpdi <- tibble(x = mean(y_pred$curr_log_ratio),
                             lb_x = rethinking::HPDI(y_pred$curr_log_ratio %>% as.numeric(), hpdi_prob)[1],
                             ub_x =rethinking::HPDI(y_pred$curr_log_ratio %>% as.numeric(), hpdi_prob)[2],
                             mean = mean(y_pred$curr_return_pred),
                             lb = rethinking::HPDI(y_pred$curr_return_pred %>% as.numeric(), hpdi_prob)[1],
                             ub = rethinking::HPDI(y_pred$curr_return_pred %>% as.numeric(), hpdi_prob)[2])
    
    curr_est <- tibble(x = y_pred$curr_log_ratio,
                       y = y_pred$curr_return_pred)
    
    
  }
  
  {
    y_observed <- train
    y_max <- round(max(abs(curr_est$y)) * 100 / 5 + 1) * (5 / 100)
    
    main_col <- '#ae6de4'
    minor_col <- '#e4ae6d'
    zero_col <- '#E4736D'
    
    gg <-  y_pred_hpdi %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = ll), stat = 'smooth', color = main_col, alpha = 0.75) +
      geom_line(aes(y = ul), stat = 'smooth', color = main_col, alpha = 0.75) +
      geom_line(aes(y = mean), stat = 'smooth', color = main_col, size = 2, alpha = 0.75) +
      geom_vline(aes(xintercept = mean(x)), color = minor_col, size = 1.5, alpha = 0.5, linetype = 'dashed') +
      geom_hline(aes(yintercept = mean(mean)), color = minor_col, size = 1.5, alpha = 0.5, linetype = 'dashed') +
      geom_point(aes(x = mean(x), y = mean(mean)), size = 6, color = minor_col, alpha = 1.0) +
      geom_hline(aes(yintercept = 0), color = zero_col, size = 1, alpha = 0.5) +
      ggtitle(label = paste(per_ahead,
                            '-Year Total Excess ',
                            if(per_ahead > 1) {'Average Annual '},
                            'Return of the S&P 500 Index',
                            sep = ''),
              subtitle = paste('As a Function of Real Price to Normalized Real Earnings (',
                               month.name[train$month[1]],
                               ' ',
                               train$year[1],
                               ' - ',
                               train$year[nrow(train)],
                               '))',
                               sep = '')) + 
      xlab(label = paste('Real Price to Normalized Real Earnings', sep = '')) +
      ylab(label = paste('Total Excess ',
                         if(per_ahead > 1) {'Average Annual '},
                         'Return of the S&P 500 Index', sep = '')) +
      theme_classic() +
      theme(axis.text.x = element_text(color = main_col, size = 12),
           axis.text.y = element_text(color =  main_col, size = 12),
           axis.title.x = element_text(color = main_col, size = 18),
           axis.title.y = element_text(color = main_col, size = 18),
           title = element_text(color = main_col, size = 18),
           panel.background = element_rect(fill = 'black', color = main_col),
           plot.background = element_rect(fill = 'black', color = main_col))
    
    gg_build <- ggplot_build(gg)
    
    df_gg <- tibble(x = gg_build$data[[1]]$x,
                    ymin = gg_build$data[[1]]$y,
                    ymax = gg_build$data[[2]]$y,
                    y = gg_build$data$data[[3]]$y)
    
    gg_final <- gg + geom_ribbon(data = df_gg,
                                 aes(x = x, ymin = ymin, ymax = ymax),
                                 fill = 'light gray', alpha = 0.25) +
      scale_y_continuous(labels = scales::percent,
                         breaks = seq(from = -min(0.8, y_max), to = min(0.8, y_max), by = if_else(y_max > 0.6, 0.05, 0.025)),
                         limits = c(-min(0.8, y_max), min(0.8, y_max))) +
      scale_x_continuous(breaks = seq(from = 0, to = round(max(max(y_pred_hpdi$x), rethinking::HPDI(as.numeric(curr_est$x), prob = 0.9)[2]) / 10 + 0.05, 0) * 10, by = 2.5),
                         limits = c(0, round(max(max(y_pred_hpdi$x), rethinking::HPDI(as.numeric(curr_est$x), prob = 0.9)[2]) / 10 + 0.05, 0) * 10)) + 
      geom_point(data = tibble(x_obs = y_pred_hpdi %>% pull(x),
                               y_obs = y_observed$y_total_excess_return,
                               fye = y_observed$year),
                 aes(x = x_obs, y = y_obs),
                 color = main_col) +
      geom_text(data = tibble(x_obs = y_pred_hpdi %>% pull(x),
                              y_obs = y_observed$y_total_excess_return,
                              fye = as.integer(y_observed$year)),
                aes(x = x_obs, y = y_obs, label = fye),
                color = main_col, size = 3.5, hjust = -0.1) +
      geom_point(data = curr_pred_hpdi, aes(x = x, y = mean), color = '#6DE4AE', size = 8) + 
      geom_point(data = curr_est,
                 aes(x = x, y = y),
                 color = '#6DE4AE',
                 alpha = 0.05)
    
    # show plots
    gg_final %>% print()
  }
  
  {
    x_max <- round(max(abs(curr_est$y)) * 100 / 5) * (5 / 100)
    gg_density <- curr_est %>%
      ggplot() +
      geom_density(mapping = aes(x = y), fill = 'light gray', alpha = 0.25, size = 1.0, color = main_col) +
      scale_x_continuous(labels = scales::percent,
                         breaks = seq(from = -x_max, to = x_max, by = 0.05)) +
      geom_vline(xintercept = 0, size = 1.0, color = zero_col) +
      geom_vline(xintercept = mean(curr_est$y), size = 1.0, color = main_col) +
      ggtitle(label = paste('Predicted ',
                            per_ahead,
                            '-Year Annualized S&P 500 Total Excess Return', sep = ''),
              subtitle = paste('Current Month = ',
                               curr_d$date,
                               '; Current Price = ',
                               format(round(curr_d$real_price, 0), big.mark = ','),
                               '; Mean Expected Outcome = ',
                               round(curr_pred_hpdi$mean * 100, 1),
                               '%', sep = '')) +
      theme_classic() +
      theme(title = element_text(color = main_col, size = 18),
            panel.background = element_rect(fill = 'black', color = main_col),
            plot.background = element_rect(fill = 'black', color = main_col),
            axis.title.x = element_blank(),
            axis.text.x = element_text(color = main_col, size = 12),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
      
    gg_density %>% print()
    
  }
    
}

# save plots
{
  gg_final %>%
    ggsave(filename = paste('model-fit-plot-',
                            per_ahead,
                            '-year-ahead',
                            if(use_most_current) {'-most-current'},
                            '.jpeg', sep = ''),
           width = 16*2,
           height = 9*2,
           units = 'in',
           device = 'jpeg')
  
  gg_density %>%
    ggsave(filename = paste('density-',
                            per_ahead,
                            '-year-ahead',
                            if(use_most_current) {'-most-current'},
                            '.jpeg', sep = ''),
           width = 16*2,
           height = 9*2,
           units = 'in',
           device = 'jpeg')
  
  
}

# video stats
{
  # 1-year return -- March 2020 -> MArch 2021
  active_obs <- y_observed %>% filter(year == 2020)
  y_pred_hpdi$x[active_obs$index]   # P/E
  active_obs$y_total_excess_return  # Return
  
  # average P/E and return
  mean(y_pred_hpdi$x)               # P/E
  mean(y_pred_hpdi$mean)            # return
  
  y_pred_hpdi %>%
    mutate(range = (ul - ll) / 2) %>%
    summary()
  
  # prob > 0 return by P/E
  prob_greater_than_zero <- apply(y_pred$y_return_pred,
                                  MARGIN = 2,
                                  FUN = function(x) {
                                    return(sum(x > 0) / length(x))
                                  })
  summary(prob_greater_than_zero)
  
  # pull out parameter posterior
  p <- rstan::extract(fit)
  
  # predicted p(y < 0) return given P/E
  x <- 35  #c(7, 35)

  log_ratio_mu = p$log_ratio_mu
  log_ratio_centered = x - log_ratio_mu;
  
  y_expected_value <- p$alpha_return + p$beta_return * log_ratio_centered
  y_pred_outcome <- rnorm(30000, mean = y_expected_value, sd = p$sigma_return)
  sum(y_pred_outcome < 0) / length(y_pred_outcome)   # prob of loss
  
  # current expected return and p(y < 0) and expected P/E
  mean(curr_est$x)
  mean(curr_est$y)
  sum(curr_est$y < 0) / length(curr_est$y)
  
  # HPDI of current P/E outcome
  rethinking::HPDI(curr_est$y %>% as.numeric(), prob = hpdi_prob) - mean(curr_est$y)
  
  # crash -- 
  #eval <- c( -0.05, -0.1, -0.15, -0.2)
  eval <- c( -0.10, -0.20, -0.30, -0.40, -0.50)
  for(n in 1:length(eval)) (sum(curr_est$y < eval[n]) / length(curr_est$y)) %>% print()
  
}



