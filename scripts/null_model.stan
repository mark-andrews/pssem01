data {
  int<lower=0> N;
  vector[N] read;
  vector[N] like;
  vector[N] thwa;
  vector[N] depr;
  vector[N] suic;
}

parameters {
  real beta_depr_suic;
  real beta_thwa_suic;
  real beta_like_suic;
  real beta_read_like;
  real<lower=0> sigma_depr;
  real<lower=0> sigma_suic;
  real<lower=0> sigma_thwa;
  real<lower=0> sigma_like;
}

model {
  
  depr ~ normal(0, sigma_depr);
  thwa ~ normal(0, sigma_thwa);
  like ~ normal(beta_read_like * read, sigma_like);
  
  suic ~ normal(beta_depr_suic * depr + 
                beta_thwa_suic * thwa + 
                beta_like_suic * like, 
                sigma_suic);
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N){
    log_lik[i] = normal_lpdf(suic[i] | beta_depr_suic * depr[i] + 
                                       beta_thwa_suic * thwa[i] + 
                                       beta_like_suic * like[i], 
                                       sigma_suic) +
                 normal_lpdf(depr[i] | 0, sigma_depr) +
                 normal_lpdf(thwa[i] | 0, sigma_thwa) + 
                 normal_lpdf(like[i] | beta_read_like * read[i], sigma_like);
  }
}

