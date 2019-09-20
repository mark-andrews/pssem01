library(tidyverse)
library(lavaan)
library(stringr)
library(lme4)

model <- ' i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
           s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4 '

M <- growth(model, data=Demo.growth)

summary(M)

demo_growth <- select(Demo.growth, t1, t2, t3, t4) %>% 
  mutate(subject = seq(nrow(.))) %>% 
  select(subject, everything()) %>% 
  gather(time, value, t1:t4) %>% 
  mutate(time = as.numeric(str_remove(time, 't'))) %>% 
  as_tibble()

M_ml <- lmer(value ~ time + (time|subject), data = demo_growth)


# a linear growth model with a time-varying covariate
model <- '
  # intercept and slope with fixed coefficients
    i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
  
  # regressions
    i ~ x1 + x2
    s ~ x1 + x2
  
  # time-varying covariates
    t1 ~ c1
    t2 ~ c2
    t3 ~ c3
    t4 ~ c4
'
fit <- growth(model, data = Demo.growth)
summary(fit)