# Fit Stan models that have matrix algebra
library(dplyr)
library(cmdstanr)

ex9 <- read.csv('Example9 Data.csv') %>%
  mutate(across(c(Grower, Variety), factor),
         Rating = ordered(Rating, levels = c('C', 'B', 'A')))
ex9long <- ex9 %>% 
  group_by(Grower, Variety) %>%
  reframe(Rating = rep(Rating, N_Plants)) %>%
  mutate(Grower_Variety = as.integer(interaction(Grower, Variety)),
         Grower_int = as.integer(Grower),
         Rating_int = as.integer(Rating),
         Variety_int = as.integer(Variety))

# Array for rating, design matrices for fixed effects (Variety) and random effects (Grower, Grower X Variety)

Rating_mat <- matrix(0, nrow = nrow(ex9long), ncol = length(unique(ex9long$Rating)))
Variety_mat <- matrix(0, nrow = nrow(ex9long), ncol = length(unique(ex9long$Variety)) - 1)
Grower_mat <- matrix(0, nrow = nrow(ex9long), ncol = length(unique(ex9long$Grower)))
GV_mat <- matrix(0, nrow = nrow(ex9long), ncol = with(ex9long, length(unique(Grower)) * length(unique(Variety))))

for (i in 1:nrow(ex9long)) {
  Rating_mat[i, ex9long$Rating_int[i]] <- 1
  Variety_mat[i, ex9long$Variety_int[i] - 1] <- 1
  Grower_mat[i, ex9long$Grower_int[i]] <- 1
  GV_mat[i, ex9long$Grower_Variety[i]] <- 1
}

standata_matrixversion <- with(ex9long, list(
  N = nrow(ex9long),
  N_growers = length(unique(Grower)),
  N_varieties = length(unique(Variety)),
  k = length(unique(Rating)),
  Rating = Rating_mat,
  X = Variety_mat,
  Zg = Grower_mat,
  Zgv = GV_mat
))

ex9stanmod_pomatrix <- cmdstan_model('ex9multinomial_po_matrix.stan')
ex9stanmod_nonpomatrix <- cmdstan_model('ex9multinomial_nonpo_matrix.stan')

ex9stanfit_pomatrix <- ex9stanmod_pomatrix$sample(data = standata_matrixversion, seed = 111, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)
ex9stanfit_nonpomatrix <- ex9stanmod_nonpomatrix$sample(data = standata_matrixversion, seed = 112, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000)
