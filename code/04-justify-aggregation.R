# justify_aggregation.R
library(here)
library(multilevel)
library(tidyverse)
library(lavaan)
library(irr)

here("data", "2023-03-26-search-interest-year-state") |> 
  read_rds() |> 
  filter(
    keyword == "vegan + vegetarian + dairy free + animal testing + cruelty free + grass fed + free range + cage free"
  ) ->
  search_interest_state_year_long

# > Rwg --------------------
search_interest_state_year_long |> 
  mutate(state_year = str_c(state, year)) ->
  search_interest_state_year_long

multilevel::rwg(
  x      = search_interest_state_year_long$search_interest,
  grpid  = search_interest_state_year_long$state_year,
  ranvar = 833.25 # (100 response options ^ 2 - 1) / 12
) |> 
  summary()
# The summary statistics of rwg values show that the agreement among the ten
# samples within each state-year is generally very high. The minimum rwg value
# is 0.9447, and the mean rwg value is 0.9950. This suggests that the ten
# samples are consistently providing similar search interest values within each
# state-year. Given this high level of agreement, it is reasonable to consider
# using a higher-order factor to represent the search interest for each state in
# a given year.

# > ICC ----------------------
search_interest_state_year_long |>  
  arrange(state, year, sample) |> 
  pivot_wider(
    id_cols = c(state, year),
    names_from = sample,
    names_prefix = "sample_",
    values_from = search_interest
  ) ->
  search_interest_state_year_wide

search_interest_state_year_wide |> 
  select(sample_1:sample_10) |> 
  irr::icc(type = "consistency", unit = "average", model = "twoway")

# The results are as follows:
# 1. ICC(C,10): The ICC value is 0.998, which is very close to 1, indicating a
# very high level of consistency among the ten samples' search interest ratings
# within each state-year.
# 2. F-Test: The F-test is used to test the null hypothesis (H0) that the ICC
# value is 0 (no reliability). The F-statistic is 569 with a p-value of 0,
# suggesting that we can reject the null hypothesis, and there is a significant
# level of consistency among the raters.
# These results suggest that there is a very high level of consistency among the
# ten samples in measuring search interest within each state-year. The high ICC
# value supports the idea of using a higher-order factor to represent the search
# interest for each state in a given year, as it indicates that the ten samples
# are providing reliable and consistent measurements.

# > CFA ---------------------
'level: 1
  f1 =~ sample_1 + sample_2 + sample_3 + sample_4 + sample_5 + sample_6 + sample_7 + sample_8 + sample_9 + sample_10
level: 2
  fb =~ sample_1 + sample_2 + sample_3 + sample_4 + sample_5 + sample_6 + sample_7 + sample_8 + sample_9 + sample_10' |> 
  lavaan::cfa(search_interest_state_year_wide, cluster = "state") |> 
  summary(fit.measures = TRUE, standardized = TRUE)

# The results suggest the model has an overall poor fit. The Comparative Fit
# Index (CFI) is 0.888 and the Tucker-Lewis Index (TLI) is 0.856, both of which
# are below the recommended threshold of 0.95 for a good model fit. The Root
# Mean Square Error of Approximation (RMSEA) is 0.215, which is above the
# recommended threshold of 0.06 for a good fit, indicating that the model
# doesn't fit the data well. Some of the variance estimates at the state level
# being negative is a warning sign that the model is misspecified.