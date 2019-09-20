library(tidyverse)
library(psych)

N <- 1000
b <- 5.25

data_df <- tibble(x = rnorm(N),
                  y = b * x + rnorm(N)
)

plot(data_df)

pca_fit <- princomp(~ x + y, data = data_df)

summary(pca_fit)

# loadings
loadings(pca_fit)

data_df %>% 
  ggplot(aes(x, y)) + geom_point() +
  geom_abline(intercept = 0.18, y)


basis_1 <- 5 * rbind(c(0, 0), 
                 loadings(pca_fit)[,'Comp.1'])

basis_2 <- rbind(c(0, 0), 
                 loadings(pca_fit)[,'Comp.2']) 


plot(data_df, xlim = c(-1, 1), ylim = c(-1, 1))
lines(basis_1, type='l', col='red') 
lines(basis_2, type='l', col='green')


# Some psych data ---------------------------------------------------------

data(bfi, package = 'psych')

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


bfi_df <- select(bfi, A1:O5) %>% na.omit()

fit <- princomp(bfi_df, cor=TRUE)
summary(fit)


# Exploratory Factor analysis ----------------------------------------------------------------

f5 <- fa(bfi_df , nfactors = 5, fm="minres", rotate = 'varimax')
print(loadings, cutoff = 0.3)

fa.diagram(f5)

f5 <- fa(bfi_df , nfactors = 5, fm="minres", rotate = 'oblimin')
print(loadings, cutoff = 0.3)

fa.diagram(f5)