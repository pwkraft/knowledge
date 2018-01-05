data {
  int<lower=0> N;       // Number of observations
  int<lower=0> I;       // Number of items
  int<lower=0> w[N,I];  // Word count (weighted by topic diversity)
}

parameters {
  vector[I] mu;
  vector<lower=0>[I] sigma;
  vector<lower=0>[N] theta;
}

model {
  for (i in 1:I) {
    w[,i] ~ lognormal(theta*mu[i],sigma[i]);
  }
}
