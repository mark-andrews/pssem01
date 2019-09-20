library(lavaan)

model <- '
  # measurement model
    
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
'

M <- sem(model, data=PoliticalDemocracy)
summary(M)
summary(M, fit.measures = T)

parameterestimates(M)


M <- sem(model, data=PoliticalDemocracy, std.lv = T)
parameterestimates(M)
