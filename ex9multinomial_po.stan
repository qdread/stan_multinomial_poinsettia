data {
  int<lower=1> N;
  int<lower=1> N_growers;
  int<lower=1> N_varieties;
  array[N, 3] int<lower=0, upper=1> Rating; // 1=C 2=B 3=A. 2d array with 1 and two 0 on each row. C is worst rating, A is best.
  array[N] int<lower=1> Variety;
  array[N] int<lower=1> Grower;
}

parameters {
  // Intercepts (associated with the reference level, rating C)
  real int_0;
  real int_1;
  
  // Effects for varieties 1 and 2
  real tau_1;
  real tau_2;

  // Standard deviations of random effects
  real<lower=0> sd_g; // SD of random effect for each grower.
  real<lower=0> sd_gv; // SD of random effect for variety within each grower.
  
  // Individual random effects
  vector[N_growers] g; // Random effect for each grower.
  array[N_varieties] vector[N_growers] gv; // Random slope with respect to variety within each grower.
}

transformed parameters {
  // Linear predictors (there are two link functions because k=3 levels of Rating)
  vector[N] eta_0;
  vector[N] eta_1;
  // Parameters of the multinomial distribution (probabilities of each rating)
  vector[N] Pi_0;
  vector[N] Pi_1;
  vector[N] Pi_2;

  for (i in 1:N) {
    // Two linear predictors because there are three levels of rating.
    eta_0[i] = int_0 + tau_1*(Variety[i]==1) + tau_2*(Variety[i]==2) + g[Grower[i]] + gv[Variety[i]][Grower[i]];
    eta_1[i] = int_1 + tau_1*(Variety[i]==1) + tau_2*(Variety[i]==2) + g[Grower[i]] + gv[Variety[i]][Grower[i]];
    
    // Apply inverse link function to linear predictors. Ensure that Pi1 is not negative.
    Pi_0[i] = 1 / (1 + exp(-eta_0[i]));
    Pi_1[i] = max([0.0, 1 / (1 + exp(-eta_1[i])) - 1 / (1 + exp(-eta_0[i]))]);
    Pi_2[i] = 1 - Pi_0[i] - Pi_1[i];
  }
}

model {
  // Priors (weakly informative)
  sd_g ~ gamma(1, 1);
  sd_gv ~ gamma(1, 1);
  int_0 ~ normal(0, 10);
  int_1 ~ normal(0, 10);
  tau_1 ~ normal(0, 10);
  tau_2 ~ normal(0, 10);

  // Likelihood
  for (i in 1:N) {
    Rating[i] ~ multinomial([Pi_0[i], Pi_1[i], Pi_2[i]]');
  }
  
  g ~ normal(0, sd_g); 
  
  for (i in 1:N_varieties) {
    gv[i] ~ normal(0, sd_gv); 
  }
}

