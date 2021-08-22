
data {
  int<lower=0> N;                       // Number of observations
  vector[N] index;                      // Time indices
  real y_earnings[N];                   // S&P 500 earnings (log)
  
  vector[N] y_price;                    // S&P 500 index values (log)
  real y_futurereturn[N];               // future period return (log)
  
  // estimate current value
  real curr_index;                      // Current time index
  real curr_price;                      // Current S&P 500 index value (log)
  
  // priors
  real prior_alpha_mu;  real prior_alpha_sig;
  real prior_beta_mu;   real prior_beta_sig;
  real prior_sigma;
  
  real prior_alpha_return_mu; real prior_alpha_return_sig;
  real prior_beta_return_mu;  real prior_beta_return_sig;
  real prior_sigma_return;
  
}

parameters {
  // linear exponential fit of earnings ~ index
  real<lower=0> alpha;
  real beta;
  real<lower=0> sigma;
  
  // linear fit of future return ~ Normalized P/E
  real alpha_return;
  real beta_return;
  real<lower=0> sigma_return;

}

transformed parameters {
  vector[N] y_earnings_hat;
  vector[N] log_ratio;
  vector[N] log_ratio_centered;
  real log_ratio_mu;
  
  
  y_earnings_hat = log(alpha) + index * beta;
  
  log_ratio = exp(y_price  -  exp(y_earnings_hat));
  log_ratio_mu = mean(log_ratio);
  log_ratio_centered = log_ratio - log_ratio_mu;
  
}


model {
  // linear (exponential) fit of earnings
  target += normal_lpdf(log(y_earnings) | log(alpha) + index * beta, sigma);
  
  target += normal_lpdf(alpha | prior_alpha_mu, prior_alpha_sig);
  target += normal_lpdf(beta | prior_beta_mu, prior_beta_sig);
  target += exponential_lpdf(sigma | prior_sigma);
  
  
  // linear fit of P/E ratio
  target += normal_lpdf(y_futurereturn | alpha_return + log_ratio_centered * beta_return, sigma_return);
  
  target += normal_lpdf(alpha_return | prior_alpha_return_mu, prior_alpha_return_sig);
  target += normal_lpdf(beta_return | prior_beta_return_mu, prior_beta_return_sig);
  target += exponential_lpdf(sigma_return | prior_sigma_return);
}
  
generated quantities {
  vector[N] y_return_pred;
  vector[N] y_return_hat;
  
  real curr_return_pred;
  real curr_return_hat;
  real curr_earnings_hat;
  real curr_log_ratio_centered;
  real curr_log_ratio;
  
  
  y_return_hat = alpha_return + log_ratio_centered * beta_return;
  for(n in 1:N) {
    y_return_pred[n] = normal_rng(y_return_hat[n], sigma_return);
    y_return_pred[n] = exp( y_return_pred[n]) - 1;   // transform back to nominal values
  }
  
  
  // make current return estimate
  curr_earnings_hat = log(alpha) + curr_index * beta;
  curr_log_ratio = exp(curr_price  -  exp(curr_earnings_hat));
  curr_log_ratio_centered = curr_log_ratio - log_ratio_mu;
  curr_return_hat = alpha_return + curr_log_ratio_centered * beta_return;
  
  curr_return_pred = normal_rng(curr_return_hat, sigma_return);
  curr_return_pred = exp(curr_return_pred) - 1;       // tranform back to nominal values
  
}





