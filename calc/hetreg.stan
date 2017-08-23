data {
  int<lower = 0> N;   // number of observations
  int<lower = 0> B;   // number of beta parameters predicting mu
  int<lower = 0> G;   // number of gamma parameters predicting sigma
  vector[N] y;        // dependent variable
  matrix[N, B] X;     // covariates predicting mu
  matrix[N, G] Z;     // covariates predicting sigma
  int<lower = 0> S;   // number of simulation scenarios
  matrix[S, G] Zpred; // covariates for simulation
}
parameters {
  real beta0;
  vector[B] beta;
  real gamma0;
  vector[G] gamma;
}
//transformed parameters{
//}
model {
  vector[N] sigma;
  sigma = exp(gamma0 + Z * gamma);
  y ~ normal(beta0 + X * beta, sigma);
}
generated quantities {
  vector[S] sigmahat;
  sigmahat = exp(gamma0 + Zpred  * gamma);
}

