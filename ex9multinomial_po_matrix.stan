data {
  int<lower=1> N;
  int<lower=1> N_growers;
  int<lower=1> N_varieties;
  int<lower=1> k; // Number of categories (ratings)
  array[N, k] int<lower=0, upper=1> Rating; // 1=C 2=B 3=A. 2d array with 1 and two 0 on each row. C is worst rating, A is best.
  matrix[N, N_varieties - 1] X; // Fixed effects design matrix (2 columns, one for each non-reference variety)
  matrix[N, N_growers] Zg; // Random effects design matrix for grower
  matrix[N, N_growers * N_varieties] Zgv; // Random effects design matrix for grower x variety
}

parameters {
  // Fixed effect parameters
  array[k - 1] real Intercept; // One intercept per link function
  vector[N_varieties - 1] Tau; // One vector of treatment parameters for all link functions (because it is prop odds model)
  
  // Standard deviations of random effects
  real<lower=0> sd_g; // SD of random effect for each grower.
  real<lower=0> sd_gv; // SD of random effect for variety within each grower.
  
  // Individual random effects
  vector[N_growers] g; // Random effect for each grower.
  vector[N_growers * N_varieties] gv; // Random slope with respect to variety within each grower.
}

transformed parameters {
  // Linear predictors (there are two link functions because k=3 levels of Rating)
  matrix[N, k - 1] eta;
  // Parameters of the multinomial distribution (probabilities of each rating)
  matrix[N, k] Pi;

  // Construct each of the k-1 linear predictors as sum of fixed and random terms
  for (i in 1:(k-1)) {
    eta[, i] = Intercept[i] + X * Tau + Zg * g + Zgv * gv;
  }

  // Apply inverse link function to linear predictors. Not sure how to convert this to matrix algebra or generalize to >3 levels
  Pi[, 1] = 1 / (1 + exp(-eta[, 1]));
  Pi[, 2] = fmax(0.0, 1 / (1 + exp(-eta[, 2])) - 1 / (1 + exp(-eta[, 1])));
  Pi[, 3] = 1 - Pi[, 2] - Pi[, 1];
  
}

model {
  // Priors (weakly informative)
  sd_g ~ gamma(1, 1);
  sd_gv ~ gamma(1, 1);
  for (i in 1:k-1) {
    Intercept[i] ~ normal(0, 10);
    Tau[i] ~ normal(0, 10);
  }

  // Likelihood
  for (i in 1:N) {
    Rating[i] ~ multinomial(Pi[i,]');
  }
  
  g ~ normal(0, sd_g); 
  gv ~ normal(0, sd_gv);

}
