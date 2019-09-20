library(rstan)
library(loo)

options(mc.cores = 1)
rstan_options(auto_write = TRUE)

# set seed ----------------------------------------------------------------

set.seed(10101)

# parameters --------------------------------------------------------------
mediation_effect_size = 0.1
standard_sigma <- 1.0
standard_beta <- 0.5

beta_read_like <- 0.4

beta_depr_suic <- standard_beta
beta_thwa_suic <- standard_beta
beta_like_suic <- standard_beta
beta_depr_suic <- standard_beta

beta_thwa_depr <- mediation_effect_size
beta_like_thwa <- mediation_effect_size


  
# variables ---------------------------------------------------------------
N <- 100
read <- rnorm(N, mean=0, sd=standard_sigma)
like <- beta_read_like * read + rnorm(N, mean=0, sd=standard_sigma);
thwa <- beta_like_thwa * like + rnorm(N, mean=0, sd=standard_sigma)
depr <- beta_thwa_depr * thwa + rnorm(N, mean=0, sd=standard_sigma)
suic <- beta_depr_suic * depr +
  beta_thwa_suic * thwa +
  beta_like_suic * like + rnorm(N, mean=0, sd=standard_sigma);


# data list  --------------------------------------------------------------

test_data <- list(N = N,
                  read = read,
                  like = like,
                  thwa = thwa,
                  depr = depr,
                  suic = suic
)

  
# model fit ---------------------------------------------------------------
  
fit_null <- stan(file = 'scripts/stan_examples_maybe/null_model.stan', data = test_data, chains = 4, refresh = 0)
fit_mediation <- stan(file = 'scripts/stan_examples_maybe/mediation_model.stan', data = test_data, chains=4, refresh = 0)
  
loo_null <- loo(fit_null)
loo_mediation <- loo(fit_mediation)
  
compare(loo_null, loo_mediation)

null_ll <- extract_log_lik(fit_null)
mediation_ll <- extract_log_lik(fit_mediation)

(waic1 <- waic(null_ll))
(waic2 <- waic(mediation_ll))
print(compare(waic1, waic2), digits = 2)
