library(here)
library(tidyverse)
library(janitor)
library(glue)
library(fixest)
library(lme4)
library(lmerTest)
library(performance)
library(broom.mixed)
library(Hmisc)

here("data", "2023-03-26-search-interest-year-state") |> 
  read_rds() |> 
  filter(
    keyword == 
      "vegan + vegetarian + dairy free + animal testing + cruelty free + grass fed + free range + cage free"
    # "vegan + vegetarian + dairy free"
    # "animal testing + cruelty free"
    # "grass fed + free range + cage free"
  ) |> 
  select(-keyword) ->
  search_interest_state_year


here("data", "2024-05-18-search-interest") |> 
  read_rds() |> 
  filter(
    keyword == 
      "vegan + vegetarian + dairy free + animal testing + cruelty free + grass fed + free range + cage free"
    # "vegan + vegetarian + dairy free"
    # "animal testing + cruelty free"
    # "grass fed + free range + cage free"
  ) |> 
  select(-keyword) |> 
  mutate(year = as.integer(year))->
  updated_data

search_interest_state_year_long <- bind_rows(search_interest_state_year, updated_data)

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

# Aggregate the data across samples
search_interest_state_year_long |>
  group_by(state, year) |>
  dplyr::summarize(search_interest = mean(search_interest), .groups = "drop") ->
  search_interest

gini_index <- read_rds(here("data", "2023-03-26-gini-index"))
covariate  <- read_rds(here("data", "2023-03-26-covariate"))

gini_index |> 
  select(-gini_se) |> 
  left_join(covariate, by = c("state", "year")) |> 
  left_join(search_interest, by = c("state", "year")) ->
  data

# Null model
null_mod <- lmer(search_interest ~ 1 + (1 | state) + (1 | year), data)
summary(null_mod)
# How much variance in search_interest do the levels within the state and year
# clusters explain?
performance::icc(null_mod, by_group = TRUE)

# Cluster-mean center (cwc) gini and the control variables by states and years
data |>  
  group_by(state) |> 
  mutate(
    gini_state = mean(gini),
    across(
      .cols    = c(gini, bachelors_or_higher_percent:population_density),
      .fns     = ~. - mean(.),
      .names   = "{.col}_cwc"
    )
  ) |> 
  group_by(year) |> 
  mutate(
    gini_year  = mean(gini),
    across(
      .cols    = ends_with("_cwc"),
      .fns     = ~. - mean(.)
    )
  ) |>
  ungroup() ->
  data

# Does the model with random intercepts for state and year fit meaningfully
# better than the model with only state random intercepts?
mod1 <- lmer(search_interest ~ gini_cwc + (1 | state) + (1 | year), data)
mod2 <- lmer(search_interest ~ gini_cwc + (1 | state), data)

# The AIC/BIC/logLik suggest sizable superiority of mod1. Therefore, let's
# include random effects for state and year in the model.
anova(mod1, mod2)

# By cluster-mean centering the variables, we remove between-cluster variance in
# them, so the coefficients in the following model represent the variables'
# within-cluster predictive effects. The intercept represents estimated search
# interest for an average state and in an average year.

data |> 
  select(
    search_interest,
    gini_cwc,
    age_median_cwc,
    bachelors_or_higher_percent_cwc,
    male_percent_cwc,
    white_percent_cwc,
    income_mean_cwc,
    population_density_cwc
  ) ->
  cor_data

# Function to format the correlation matrix with stars
cor_with_stars <- function(cor_matrix, pvalue_matrix, digits = 2) {
  # Helper function to add stars based on p-value
  star_adder <- function(x){
    if (is.na(x)) return("")
    if (x < .001) return("***")
    else if (x < .01) return("**")
    else if (x < .05) return("*")
    else return("")
  }
  
  # Round the correlation matrix
  cor_matrix <- round(cor_matrix, digits)
  
  # Apply the star_adder function to the p-value matrix
  stars <- matrix(apply(pvalue_matrix, c(1, 2), star_adder), ncol = ncol(cor_matrix), nrow = nrow(cor_matrix))
  
  # Create a matrix of correlation coefficients and stars
  cor_star_matrix <- matrix(paste(cor_matrix, stars, sep = ""), ncol = ncol(cor_matrix), nrow = nrow(cor_matrix))
  dimnames(cor_star_matrix) <- dimnames(cor_matrix)
  
  return(cor_star_matrix)
}

# Calculate the correlation and p-value matrices
rcorr_results <- rcorr(as.matrix(cor_data))

# Apply the cor_with_stars function
cor_with_stars(rcorr_results$r, rcorr_results$P) |> 
  as.data.frame() |> 
  write_csv(here("outputs", "correlation-table.csv"))

# Calculate means and standard deviations
data |> 
  select(
    search_interest,
    gini,
    age_median,
    bachelors_or_higher_percent,
    male_percent,
    white_percent,
    income_mean,
    population_density
  ) ->
  msd_data

msd_data |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) |> 
  summarise(
    mean = mean(value) |> round(2), 
    sd = sd(value) |> round(2), 
    .by = variable
  ) |> 
  write_csv(here("outputs", "means-sds.csv"))


lmerTest::lmer(
  search_interest ~ 
    gini_cwc +
    (1 | year) +
    (1 | state), 
  data
) ->
  main_effect

summary(main_effect)
logLik(main_effect) |> round(2)
AIC(main_effect) |> round(3)
BIC(main_effect) |> round(2)

tidy_results <- broom.mixed::tidy(main_effect, effects = "fixed")

# Add stars for significance levels
tidy_results$star <- ifelse(tidy_results$p.value < .001, "***", 
                            ifelse(tidy_results$p.value < .01, "**", 
                                   ifelse(tidy_results$p.value < .05, "*", " ")))

# Combine estimate, std.error and star into a single column
tidy_results$Estimates <- paste0(round(tidy_results$estimate, 2), 
                                 " (", round(tidy_results$std.error, 2), ")",
                                 tidy_results$star)

# Keep only the necessary columns
final_results <- tidy_results %>%
  select(term, Estimates) %>%
  rename("Variable Names" = term, "Estimates" = Estimates)

write_csv(final_results, here("outputs", "regression-table-main-effect.csv"))

lmerTest::lmer(
  search_interest ~ 
    gini_cwc +
    age_median_cwc +
    bachelors_or_higher_percent_cwc +
    male_percent_cwc +
    white_percent_cwc +
    income_mean_cwc +
    population_density_cwc +
    (1 | year) +
    (1 | state), 
  data
) ->
  model

AIC(model) |> round(2)
BIC(model) |> round(2)
logLik(model) |> round(2)

summary(model)

tidy_results <- broom.mixed::tidy(model, effects = "fixed")

# Add stars for significance levels
tidy_results$star <- ifelse(tidy_results$p.value < .001, "***", 
                            ifelse(tidy_results$p.value < .01, "**", 
                                   ifelse(tidy_results$p.value < .05, "*", " ")))

# Combine estimate, std.error and star into a single column
tidy_results$Estimates <- paste0(round(tidy_results$estimate, 2), 
                                 " (", round(tidy_results$std.error, 2), ")",
                                 tidy_results$star)

# Keep only the necessary columns
final_results <- tidy_results %>%
  select(term, Estimates) %>%
  rename("Variable Names" = term, "Estimates" = Estimates)

write_csv(final_results, here("outputs", "regression-table-humane.csv"))

# fixest::feols(
#   fml = search_interest ~ 
#     white_percent_cwc +
#     bachelors_or_higher_percent_cwc +
#     income_mean_cwc +
#     age_median_cwc +
#     male_percent_cwc +
#     population_density_cwc +
#     gini_cwc | 
#     state + year, 
#   se = "iid",
#   # How to handle clustering? Is clustering at the state and year level
#   # appropriate?
#   # cluster = c("state", "year"),
#   data = data
# ) |> 
#   summary()

# > ICC ----------------------
# search_interest_state_year_long |>  
#   arrange(state, year, sample) |> 
#   pivot_wider(
#     id_cols = c(state, year),
#     names_from = sample,
#     names_prefix = "sample_",
#     values_from = search_interest
#   ) ->
#   search_interest_state_year_wide
# 
# search_interest_state_year_wide |> 
#   select(sample_1:sample_10) |> 
#   irr::icc(type = "consistency", unit = "average", model = "twoway")

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

# lmerTest::lmer(
#   search_interest ~ 
#     age_median_cwc +
#     bachelors_or_higher_percent_cwc +
#     male_percent_cwc +
#     white_percent_cwc +
#     income_mean_cwc +
#     population_density_cwc +
#     (1 | year) +
#     (1 | state), 
#   data
# ) ->
#   controls
# 
# summary(controls)
# logLik(controls) |> round(2)
# AIC(controls) |> round(2)
# BIC(controls) |> round(2)
# 
# tidy_results <- broom.mixed::tidy(controls, effects = "fixed")
# 
# # Add stars for significance levels
# tidy_results$star <- ifelse(tidy_results$p.value < .001, "***", 
#                             ifelse(tidy_results$p.value < .01, "**", 
#                                    ifelse(tidy_results$p.value < .05, "*", " ")))
# 
# # Combine estimate, std.error and star into a single column
# tidy_results$Estimates <- paste0(round(tidy_results$estimate, 2), 
#                                  " (", round(tidy_results$std.error, 2), ")",
#                                  tidy_results$star)
# 
# # Keep only the necessary columns
# final_results <- tidy_results %>%
#   select(term, Estimates) %>%
#   rename("Variable Names" = term, "Estimates" = Estimates)
# 
# write_csv(final_results, here("outputs", "regression-table-controls.csv"))
