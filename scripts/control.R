library(tidyverse)

rbin <- function(N) sample(c(0, 1), size = N, replace = T)
ilogit <- function(x) 1/(1 + exp(-x))
rbern <- function(p) 1*(runif(length(p)) < p)

N <- 1e5
a <- 1.0 
b <- 20
c <- 20

# Confounders -------------------------------------------------------------
# x <- z -> y
Df <- tibble(z = rbin(N),
             # y and x have common cause
             y = rbern(ilogit(a + b*z)),
             x = rbern(ilogit(a + c*z))
)

# Independence of x and y?
Df %>% select(x, y) %>% 
  cor()

# Control for z
psych::partial.r(Df, c('x' , 'y'), 'z')


# Causal chains

# x -> z -> y
Df <- tibble(x = rbin(N),
             # y and x have common cause
             z = rbern(ilogit(a + b*x)),
             y = rbern(ilogit(a + c*z))
)

# Independence of x and y?
Df %>% select(x, y) %>% 
  cor()

# Control for z
psych::partial.r(Df, c('x' , 'y'), 'z')



# Colliders ---------------------------------------------------------------
# x -> y <- z
Df <- tibble(x = rbin(N),
             z = rbin(N),
             # common effect of x and z
             y = rbern(ilogit(a + b * x - c * z))
)

# Independence of x and z?
Df %>% select(x, z) %>% 
  cor()

# Control for y
psych::partial.r(Df, c('x' , 'z'), 'y')


