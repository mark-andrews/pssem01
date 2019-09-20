library(tidyverse)
library(lavaan)

model_spec <- 'visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'

M <- sem(model_spec, 
         data = HolzingerSwineford1939, 
         group = "school")

summary(M)
parameterEstimates(M) %>% filter(group == 1)

model_spec <- '
  visual =~ x1 + x2 + c(bv3, bv3)*x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

M <- sem(model_spec, 
         data = HolzingerSwineford1939, 
         group = "school")

model_spec <- 'visual =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'

M <- cfa(model_spec, 
         data = HolzingerSwineford1939, 
         group = "school",
         group.equal = c("loadings"))

parameterEstimates(M)
