mediation_models_specs <- within(list(),{
  
  model_a <- '
    x ~ 1
    m ~ 1
    y ~ 1
  '
  model_b <- '
    x ~ 1
    m ~ 1 + x
    y ~ 1
  '
  model_c <- '
    x ~ 1
    m ~ 1
    y ~ 1 + m
  '
  model_d <- '
    x ~ 1
    m ~ 1
    y ~ 1 + x
  '
  model_e <- '
    x ~ 1
    m ~ 1 + x
    y ~ 1 + m
  '
  model_f <- '
    x ~ 1
    m ~ 1 + x
    y ~ 1 + x
    
    # Force independence of y and m
    y ~~ 0*m
  '
  model_g <- '
    x ~ 1
    m ~ 1 
    y ~ 1 + x + m
  '
  model_h <- '
    x ~ 1
    m ~ 1 + x
    y ~ 1 + x + m
  '
})
