


data {
  int<lower=0> N;
  int<lower=0> y[N];
  matrix[N, 2] x;
  
  int<lower=0> N_2fit;
  matrix[N_2fit, 2] x_2fit;
  
}

parameters {
  real alpha;
  vector[2] beta;
  
  real<lower=0.0> reciprocal_phi;
  
}

transformed parameters{
  real phi;
  phi = 1 / reciprocal_phi;
  
}


model {
  target += neg_binomial_2_log_lpmf(y | alpha + x * beta, phi);
 
  target += normal_lpdf(alpha | 0, 2);
  target += normal_lpdf(beta | 0, 0.1);
  
  target += cauchy_lpdf(reciprocal_phi | 0, 1);

}

generated quantities{
  int<lower=0> y_rep [N_2fit];
  
  y_rep = neg_binomial_2_log_rng(alpha + x_2fit * beta, phi);
  
}

