# We assume the following causal model:
#   
#   z --> {m, y}, where z is an *unobserved* covariate of m and y
#   x --> y, direct effect
#   x --> m --> y, mediated effect

# The functional (parametric) form of these models does not
# change the nature of the problem, but we will choose some forms to obtain a
# concrete example.
# We assume x, y, m are binary, and that z is in (-oo, oo).

Pz <- function(N) rnorm(N, sd = 0.05)
Px <- function(N) as.numeric(runif(N) < ilogit(rnorm(N)))
# Pr of m given x and z
f_mxz <- function(x, z, N) {
  as.numeric(runif(N) < ilogit(b_0 * x + z))
}
# Pr of y given m, x, z
f_ymxz <- function(m, x, z, N){
  as.numeric(runif(N) < ilogit(b_1 * x + b_2 * m + z))
}

# Load packages -----------------------------------------------------------
library(mediation)
library(tidyverse)
library(magrittr)


# Simulations setup -------------------------------------------------------

N <- 1e5 # make it bigish
b_0 <- 1.25
b_1 <- 1.25
b_2 <- 1.75
ilogit <- plogis # Inverse logit 

# An observational study --------------------------------------------------

run_causalmed_study <- function(N = 1000){
  tibble(z = Pz(N), # unobserved covariates of m and y
         # observed x is the randomly assigned x
         x = round(runif(N)),
         # potential mediator if x = 0
         m_0 = f_mxz(x = 0, z, N),
         # potential mediator if x = 1
         m_1 = f_mxz(x = 1, z, N),
         # observed m
         m = m_0*(x==0) + m_1*(x==1),
         # potential outcome if m = 0, x = 0
         y_00 = f_ymxz(m = 0, x = 0, z, N),
         # potential outcome if m = 0, x = 1
         y_01 = f_ymxz(m = 0, x = 1, z, N),
         # potential outcome if m = 1, x = 0
         y_10 = f_ymxz(m = 1, x = 0, z, N),
         # potential outcome if m = 1, x = 1
         y_11 = f_ymxz(m = 1, x = 1, z, N),
         # observed y
         y = y_00*(m==0 & x==0) + 
             y_01*(m==0 & x==1) +
             y_10*(m==1 & x==0) +
             y_11*(m==1 & x==1)
  )
}


# Do some checks... -------------------------------------------------------

# when x is 0, then m is m_0 (m(x = 0))
run_causalmed_study(N) %>% 
  filter(x == 0) %>% 
  mutate(check = m == m_0) %>% 
  summarize(all(check))

# when x is 1, then m is m_1 (m(x = 1))
run_causalmed_study(N) %>% 
  filter(x == 1) %>% 
  mutate(check = m == m_1) %>% 
  summarize(all(check))

# when x is 0, then y is y(m = m(x = 0), x = 0)
run_causalmed_study(N) %>% 
  filter(x == 0) %>% 
  select(m, y_00, y_10, y) %>% 
  mutate(check = y == y_00*(m == 0) + y_10*(m == 1)) %>% 
  summarize(all(check))

# when x is 0, then y is y(m = m(x = 0), x = 0)
run_causalmed_study(N) %>% 
  filter(x == 0) %>% 
  select(m, y_00, y_10, y) %>% 
  mutate(check = y == y_00*(m == 0) + y_10*(m == 1)) %>% 
  summarize(all(check))


# Look at the data --------------------------------------------------------

run_causalmed_study(N) %>% 
  select(x, z, starts_with('m'), starts_with('y')) 


# Check independence assumptions ------------------------------------------

# independence of x and potential outcomes/mediators
run_causalmed_study(N) %>% 
  select(x, starts_with('m_'), starts_with('y_')) %>% 
  cor()

# *non*independence of potential mediators and potential outcomes, given x
# but x is independent of these potentials anyway, 

run_causalmed_study(N) %>% 
  select(x, starts_with('m_'), starts_with('y_')) %>% 
  cor()



# A causal mediation analysis ---------------------------------------------

Df <- run_causalmed_study(1000)

med_fit <- glm(m ~ x, data = Df, family = binomial(link = 'logit'))
out_fit <- glm(y ~ m + x,
               data = Df,
               family = binomial(link = 'logit'))

summary(med_fit)
summary(out_fit)

med_out <- mediate(med_fit, out_fit,
                   treat = "x",
                   mediator = "m",
                   robustSE = TRUE,
                   sims = 1000)

summary(med_out)



# Sanity checks ---------------------------------------------------------------

# Should be close to ACME (treated)
Df %>% filter(x == 1) %>% 
  mutate(q1 = ilogit(b_1 * x + b_2 * m_1),
         q0 = ilogit(b_1 * x + b_2 * m_0),
         cme = q1 - q0) %>% 
  summarise(mean(cme))

# Should be ACME (control)
Df %>% filter(x == 0) %>% 
  mutate(q1 = ilogit(b_1 * x + b_2 * m_1),
         q0 = ilogit(b_1 * x + b_2 * m_0),
         cme = q1 - q0) %>% 
  summarise(mean(cme))

  
# Should be close to ADE (treated)
Df %>% filter(x == 1) %>% 
  mutate(q1 = ilogit(b_1 * 1 + b_2 * m),
         q0 = ilogit(b_1 * 0 + b_2 * m),
         de = q1 - q0) %>% 
  summarise(mean(de))

# Should be close to ADE (control)
Df %>% filter(x == 0) %>% 
  mutate(q1 = ilogit(b_1 * 1 + b_2 * m),
         q0 = ilogit(b_1 * 0 + b_2 * m),
         de = q1 - q0) %>% 
  summarise(mean(de))


