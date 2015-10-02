# stan models for ARM

# excercise 
model_ch3ex = "
data {
  int<lower=0> N;
  vector<lower=0>[N] mom_iq; // 
  vector<lower=0, upper=1>[N] mom_hs; // binary
  vector<lower=0>[N] kid_score; //
  /*
  real<lower=0> mom_iq[N]; // 
  int<lower=0, upper=1> mom_hs[N]; // binary
  real<lower=0> kid_score[N]; //
  */
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
  /*beta ~ cauchy(0, 2.5);
  sigma ~ cauchy(0, 2.5); // half cauchy
  */  
  /*for (n in 1:N)
    kid_score[n] ~ normal(beta[1] + beta[2]*mom_hs[n] + beta[3]*mom_iq[n] + 
      beta[4]*mom_hs[n]*mom_iq[n], sigma);
  */
  kid_score ~ normal(beta[1] + beta[2]*mom_hs + beta[3]*mom_iq + 
      beta[4]*inter, sigma);
}
"

model_ch3ex_z = "
data {
  int<lower=0> N;
  vector<lower=0>[N] mom_iq; // 
  vector<lower=0, upper=1>[N] mom_hs; // binary
  vector<lower=0>[N] kid_score; //
}
transformed data { //standarisation
  vector[N] z_mom_iq;
  vector[N] z_mom_hs;
  vector[N] z_inter;
  print(mean(mom_iq))
  print(sd(mom_iq))
  z_mom_iq <- (mom_iq - mean(mom_iq))/(2*sd(mom_iq));
  z_mom_hs <- (mom_hs - mean(mom_hs))/(2*sd(mom_hs));
  z_inter <- z_mom_hs .* z_mom_iq;
}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  /*beta ~ cauchy(0, 2.5);
  sigma ~ cauchy(0, 2.5); // half cauchy
  */  
  kid_score ~ normal(beta[1] + beta[2]*z_mom_hs + beta[3]*z_mom_iq + 
      beta[4]*z_inter, sigma);
}
"

# https://github.com/stan-dev/example-models/blob/master/ARM/Ch.3/kidiq_interaction.stan
model_ch3  = "
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

model_ch5ex = "
data {
  int<lower=0> N;
  vector<lower=0>[N] income; // 
  /* vector<lower=0, upper=1>[N] vote; // binary
  bernoulli_logit (int - vector)
  */
  int<lower=0, upper=1> vote[N]; //array
}
parameters {
  vector[2] beta;
}
model {
  vote ~ bernoulli_logit(beta[1] + beta[2]*income);
}
"
#https://github.com/stan-dev/example-models/blob/master/ARM/Ch.5/nes_logit.stan
model_ch5 = "
data {
  int<lower=0> N;
  vector[N] income;
  int<lower=0,upper=1> vote[N];
}
parameters {
  vector[2] beta;
}
model {
  vote ~ bernoulli_logit(beta[1] + beta[2] * income);
}
"

model_beetles="
data {
  int<lower=0> N;         //number of data
  int n[N];            //number of beetles
  int<lower=0> r[N];   //killed
  real x[N];            //concentration
}
parameters {
  vector[2] beta;
}
model {
  /*for (i in 1:N)
    r[i] ~ binomial(n[i], inv_logit(beta[1] + beta[2]*x[i]));
  */
  r ~ binomial_logit(n, beta[1] + beta[2]*x);    
}
"

model_beetles_vectorised="
data {
  int<lower=0> N;         //number of data
  vector[N] n;            //number of beetles
  int<lower=0>[N] r;   //killed
  vector[N] x;            //concentration
}
parameters {
  vector[2] beta;
}
model {
  r ~ binomial_logit(n, beta[1] + beta[2]*x);    
}
"

model_beetles_logit.stan = "
data {
    int<lower=0> N;
    int<lower=0> n[N];
    int<lower=0> r[N];
    vector[N] x;
}

transformed data {
    vector[N] centered_x;
    real mean_x;
    mean_x <- mean(x);
    centered_x <- x - mean_x;
}

parameters {
    real alpha_star;
    real beta;
}

transformed parameters {
    vector[N] m;
    m <- alpha_star + beta * centered_x;
}

model {
  alpha_star ~ normal(0.0, 1.0E4);  
  beta ~ normal(0.0, 1.0E4);
  r ~ binomial_logit(n, m);
}

generated quantities {
  real alpha; 
  real p[N];
  real llike[N];
  real rhat[N];
  for (i in 1:N)  {
    p[i] <- inv_logit(m[i]);
    llike[i]  <- r[i]*log(p[i]) + (n[i]-r[i])*log(1-p[i]);  
    rhat[i] <- p[i]*n[i];  // fitted values
  }
  alpha <- alpha_star - beta*mean_x;              
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
