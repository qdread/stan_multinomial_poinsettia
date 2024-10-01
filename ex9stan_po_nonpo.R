# Fit stan models PO and nonPO
library(dplyr)
library(cmdstanr)

# Import data and convert to long form with one rating per row
ex9 <- read.csv('Example9 Data.csv') %>%
  mutate(across(c(Grower, Variety), factor),
         Rating = ordered(Rating, levels = c('C', 'B', 'A')))

ex9long <- ex9 %>% 
  group_by(Grower, Variety) %>%
  reframe(Rating = rep(Rating, N_Plants)) %>%
  mutate(Rating = as.integer(Rating))

# Generate nx3 matrix of outcomes (ratings) for multinomial distribution
Rating_mat <- matrix(0, nrow = nrow(ex9long), ncol = 3)
for (i in 1:nrow(ex9long)) Rating_mat[i, ex9long$Rating[i]] <- 1

ex9standata <- with(ex9long, list(
  N = nrow(ex9long),
  N_growers = length(unique(Grower)),
  N_varieties = length(unique(Variety)),
  Grower = Grower, Variety = Variety, Rating = Rating_mat
))

ex9stanmod_po <- cmdstan_model('ex9multinomial_po.stan')
ex9stanmod_nonpo <- cmdstan_model('ex9multinomial_nonpo.stan')

ex9stanfit_po <- ex9stanmod_po$sample(data = ex9standata, seed = 111, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)
ex9stanfit_nonpo <- ex9stanmod_nonpo$sample(data = ex9standata, seed = 112, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)

ex9stanfit_po$summary(variables = c('int_0', 'int_1', 'tau_1', 'tau_2', 'sd_g', 'sd_gv'))
ex9stanfit_nonpo$summary(variables = c('int_0', 'int_1', 'tau_01', 'tau_12', 'tau_11', 'tau_12', 'sd_g', 'sd_gv'))
