library(tidyverse)
library(lavaan)

model <- '
        level: 1
            fw =~ y1 + y2 + y3
            fw ~ x1 + x2 + x3
        level: 2
            fb =~ y1 + y2 + y3
            fb ~ w1 + w2
    '

M <- sem(model = model, 
         data = Demo.twolevel, 
         cluster = "cluster")

lavInspect(M, "icc")
