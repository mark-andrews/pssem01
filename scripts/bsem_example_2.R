library(tidyverse)
library(blavaan)

N <- 100

b_m0 <- 1.25; b_mx <- 1.25; 
b_y0 <- -0.5; b_ym <- 1.75; b_yx <- 0.75;
sigma_m <- 1.5; sigma_y <- 2.0

mediation_df <- tibble(
  x = rnorm(N, sd = 2),
  m = b_m0 + b_mx * x + rnorm(N, sd = sigma_m),
  y = b_y0 + b_ym * m + b_yx * x + rnorm(N, sd = sigma_y)
)

mediation_model_spec_1 <- '
y ~ 1 + m + x
m ~ 1 + x
'

mediation_model_1 <- bsem(mediation_model_spec_1, data = mediation_df)

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- blavaan(HS.model, data=HolzingerSwineford1939,
               auto.var=TRUE, auto.fix.first=TRUE,
               auto.cov.lv.x=TRUE)
summary(fit)
coef(fit)
