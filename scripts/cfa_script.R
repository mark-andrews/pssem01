library(lavaan)
# scores on 9 tests
?HolzingerSwineford1939

model_spec <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'

model <- cfa(model_spec, data=HolzingerSwineford1939)
summary(model)
summary(model, fit.measures = T)
fitmeasures(model)

parameterestimates(model)
fitted(model)
residuals(model)
