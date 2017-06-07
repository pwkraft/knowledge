data {
  int<lower=2> K;              // num topics
  int<lower=2> V;              // num words
  int<lower=1> M;              // num docs
  int<lower=1> N;              // total word instances
  int<lower=1,upper=V> w[N];   // word n
  int<lower=1,upper=M> doc[N]; // doc ID for word n
  vector<lower=0>[V] beta;     // word prior
}
parameters {
  simplex[K] theta[M];         // topic dist for doc m
  simplex[V] phi[K];           // word dist for topic k
  vector<lower=0>[K] alpha[M]; // topic prior -> this has to be a matrix or array, and vary over M
  vector<lower=0>[M] psi;      // sophistication parameter
  real<lower=0> delta;         // scaling parameter for variance
}
model {
  psi ~ normal(0,.1);          // priors for testing, don't really make sense yet
  delta ~ normal(1,.1);        // priors for testing
  for (m in 1:M)  {
    alpha[m] ~ normal(psi[m], delta * psi[m]); // priors for testing
    theta[m] ~ dirichlet(alpha[m]);      // prior -> alpha varies over docs
  }
  for (k in 1:K)  
    phi[k] ~ dirichlet(beta);  // prior
  for (n in 1:N) {
    real gamma[K];
    for (k in 1:K) 
      gamma[k] = log(theta[doc[n],k]) + log(phi[k,w[n]]);
    target += log_sum_exp(gamma);  // likelihood
  }
}

