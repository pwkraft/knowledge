data {
  int<lower=0> N;       // Number of observations
  int<lower=0> I;       // Number of items
  int<lower=0> w[N,I];  // Word count
}

parameters {
  vector<lower=0>[I] lambda;
  vector<lower=0,upper=1>[N] theta;
}

model {
  for (i in 1:I) {
    lambda ~ normal(10,10);
    w[,i] ~ poisson(theta*lambda[i]);
  }
}
