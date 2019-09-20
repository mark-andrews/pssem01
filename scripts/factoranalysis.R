library(psych)
library(tidyverse)

bfi %>% 
  select(A1:O5) %>% 
  # reverse code selected items
  mutate_at(c('A1', 'C4', 'C5', 'E1', 'E2', 'O2', 'O5'),
            ~ 7 - .) %>% 
  as.matrix() %>% 
  cor(use = 'complete.obs') %>% 
  as.data.frame() %>% 
  rownames_to_column('x') %>% 
  gather(y, cov, -x) %>% 
  ggplot(mapping = aes(x = x, y = y, fill = cov)) +
  geom_tile(colour = 'white') +
  scale_fill_gradient(low = "white", high = "steelblue")


# Factor analysis

fa.parallel(Thurstone,n.obs=213) 
f5 <- fa(bfi[1:25], 5, fm="pa")
summary(fit) # print variance accounted for


# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix

fit <- princomp(na.omit(bfi[1:25]), cor=TRUE)



loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)