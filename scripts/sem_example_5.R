library(tidyverse)
library(lavaan)
library(lme4)

model <- '
        level: 1
            Reaction ~ 1 + Days
        level: 2
            Reaction ~ 1
    '

M <- sem(model = model, 
         data = sleepstudy, 
         cluster = "Subject")

M_lm <- lmer(Reaction ~ Days + (1|Subject), data = sleepstudy)

summary(M)
