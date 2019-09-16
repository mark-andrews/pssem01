.ggpairs <- function(Df){
  
  # This is a bespoke function for doing cross-correlation plots
  
  triangle_f <- function(data, mapping, ...){
    ggplot(data = data, mapping = mapping) +
      geom_point(size=0.5) +
      geom_smooth(method=lm, se = F, color="red", ...)
  }
  
  diag_f <- function(data, mapping, ...){
    ggplot(data = data, mapping = mapping) +
      geom_histogram(color="white", bins=20, ...)
  }
  
  ggpairs(Df,
          diag = list(continuous = diag_f),
          lower = list(continuous = triangle_f)
  )
  
}