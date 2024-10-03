# Cumulative logit multinomial Stan models with and without proportional odds

Example taken from Chapter 14 of 2nd edition of *Generalized Linear Mixed Models* (Stroup et al.)

Stan models are now generalizable to any number of levels and based on matrix computation instead of indexing parameter vectors (older version is in the `old` subdirectory).

- `ex9multinomial_po_matrix.stan`: Proportional odds model
- `ex9multinomial_nonpo_matrix.stan`: Model relaxing proportional odds assumption by allowing category-specific effects
- `ex9stan_po_nonpo_matrixversion.R`: R script to prepare data and fit stan models
- `Example9 Data.csv`: Example data
