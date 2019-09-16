# Understanding the role of random assignment -----------------------------

# We assume the following causal model:
#   
#   x <-- z --> y
#   x --> y 
# 
# We'll also assume we can measure x and y, but can not generally measure z.

# The functional (parametric) form of z --> x, (x,z) --> y, P(z) does not
# change the nature of the problem, but we will choose some forms to obtain a
# concrete example.

f_zx <- function(z) as.numeric(runif(length(z)) < ilogit(b_0 * z))
f_xzy <- function(x, z) as.numeric(runif(length(z)) < ilogit(b_1 * x + z))
Pz <- function() rnorm(N)

# Load packages -----------------------------------------------------------

library(tidyverse)

# Simulations setup -------------------------------------------------------

N <- 1e5 # make it bigish
b_0 <- 2.0
b_1 <- 2.0
ilogit <- plogis # Inverse logit 

# An observational study --------------------------------------------------

run_observational_study <- function(){
  tibble(z = Pz(),
         # observed x
         x = f_zx(z),
         # potential outcome if x = 0
         y_0 = f_xzy(x = 0, z),
         # potential outcome if x = 1
         y_1 = f_xzy(x = 1, z),
         # observed y
         y = y_0*(x==0) + y_1*(x==1)
  )
}

# Randomized experiment ---------------------------------------------------

run_experimental_study <- function(){
  tibble(z = Pz(),
         # observed x is the randomly assigned x
         x = round(runif(N)),
         # potential outcome if x = 0
         y_0 = f_xzy(x = 0, z),
         # potential outcome if x = 1
         y_1 = f_xzy(x = 1, z),
         # observed y
         y = y_0*(x==0) + y_1*(x==1)
  )
}


# Check independence of x and potential outcomes --------------------------

run_observational_study() %>%
  select(x, z, y_0, y_1) %>% 
  cor()

run_experimental_study() %>%
  select(x, z, y_0, y_1) %>% 
  cor()


# Esimating average treatment effects (ATE) --------------------------------

# The observed data here does not tell us ATE
run_observational_study() %>% 
  mutate(delta = y_1 - y_0) %>% 
  summarise(ate = mean(delta),
            yhat_0 = mean(y[x == 0]),
            yhat_1 = mean(y[x == 1]),
            ate_estimate = yhat_1 - yhat_0
  )

# Here, the observed data can estimate ATE
run_experimental_study() %>% 
  mutate(delta = y_1 - y_0) %>% 
  summarise(ate = mean(delta),
            yhat_0 = mean(y[x == 0]),
            yhat_1 = mean(y[x == 1]),
            ate_estimate = yhat_1 - yhat_0
  )
