# stan models for ARM

# ch2
model_linreg = "
data {
  int<lower=0> N;
  /*vector<lower=0>[Ndata] mom_iq; // 
  vector<lower=0, upper=1>[Ndata] mom_hs; // binary
  vector<lower=0>[Ndata] kid_score; //
  */
  real<lower=0> mom_iq[Ndata]; // 
  int<lower=0, upper=1> mom_hs[Ndata]; // binary
  real<lower=0> kid_score[Ndata]; //
}
parameters {
  real b0; // parameter of linear predictor
  real b1; // 
  real b2; // 
  real b3; // 
  real<lower=0> s; // sigma lognormal constant cov
}
model {
  b0 ~ cauchy(0, 2.5);
  b1 ~ cauchy(0, 2.5); 
  b2 ~ cauchy(0, 2.5); 
  b3 ~ cauchy(0, 2.5); 
  s ~ cauchy(0, 2.5); // half cauchy
  for (n in 1:Ndata)
    kid_score[n] ~ normal(b0 + b1*mom_hs[n] + b2*mom_iq[n] + b3*mom_hs[n]*mom_iq[n], s);
}
"

# https://github.com/stan-dev/example-models/blob/master/ARM/Ch.3/kidiq_interaction.stan
model_ch2  = "
data {
  int<lower=0> N;
  vector[N] kid_score;
  vector[N] mom_hs;
  vector[N] mom_iq;
}
transformed data {           // interaction
  vector[N] inter;
  inter <- mom_hs .* mom_iq;
}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  kid_score ~ normal(beta[1] + beta[2] * mom_hs + beta[3] * mom_iq 
                     + beta[4] * inter, sigma);
}
"

# bernoullie-gamma truncated [0, 1]
# took so long
# note that gamma in stan takes shape(alpha) and rate(beta)
# mean = shape/rate, variance = shape/(rate**2) 
# mean = link(a+b*x) (either exp or inv_logit)
model_berngamma_logit = "
data {
  int<lower=0> Ndata ;
  vector<lower=0>[Ndata] x ; // MMI
  vector<lower=0, upper=1>[Ndata] y ; // fatality rate
}
parameters {
  real a; // parameter of linear predictor
  real b; // a+bx
  real c; // parameter of linear predictor
  real d; // c+dx
  real<lower=0> s; // sigma lognormal constant cov
}
model {
  a ~ cauchy(0, 2.5);
  b ~ cauchy(0, 2.5); 
  c ~ cauchy(0, 2.5); 
  d ~ cauchy(0, 2.5); 
  s ~ cauchy(0, 2.5); // shape, half cauchy

  for (n in 1:Ndata) {
  (y[n] == 0) ~ bernoulli_logit(c + d * x[n]);
  if (y[n] > 0)
    y[n] ~ gamma(s, s / inv_logit(a + b*x[n])); // shape, rate
    /* 
    y[n] ~ gamma(s, s / inv_logit(a + b*x[n]))  T[,1.0]; // shape, rate
    */
  }    
}
"

# bernoullie-gamma truncated [0, 1]
# note that gamma in stan takes shape(alpha) and rate(beta)
# mean = shape/rate, variance = shape/(rate**2) 
# mean = link(a+b*x) (either exp or inv_logit)
model_berngamma_log = "
data {
  int<lower=0> Ndata ;
  vector<lower=0>[Ndata] x ; // MMI
  vector<lower=0, upper=1>[Ndata] y ; // fatality rate
}
parameters {
  real a; // parameter of linear predictor
  real b; // a+bx
  real c; // parameter of linear predictor
  real d; // c+dx
  real<lower=0> s; // shape, half cauchy
}
model {
  a ~ cauchy(0, 2.5);
  b ~ cauchy(0, 2.5); 
  c ~ cauchy(0, 2.5); 
  d ~ cauchy(0, 2.5); 
  s ~ cauchy(0, 2.5); // half cauchy

  for (n in 1:Ndata) {
  (y[n] == 0) ~ bernoulli_logit(c + d * x[n]);
  if (y[n] > 0)
    y[n] ~ gamma(s, s / exp(a + b*x[n])); // shape, rate
    /*
    y[n] ~ gamma(s, s / exp(a + b*x[n])) T[,1.0]; // shape, rate
    */
  }    
}
"
