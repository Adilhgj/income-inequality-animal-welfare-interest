library(here)
library(tidyverse)
library(janitor)

# The data come from Social Explorer: www.socialexplorer.com/explore-tables
list.files(
  path       = here("data-raw", "gini-index-state"),
  full.names = TRUE
) |> 
  map_df(
    ~read_csv(
      file           = .,
      show_col_types = FALSE
    ) |> 
      clean_names()
  ) |> 
  select_if(negate(is.logical)) |> 
  unite(gini, starts_with("acs") & !ends_with("s"), na.rm = TRUE) |> 
  unite(gini_se, starts_with("acs") & ends_with("s"), na.rm = TRUE) |> 
  mutate(
    gini = as.numeric(gini) * 100, 
    gini_se = as.numeric(gini_se) * 100,
    year = rep(c(2006:2019, 2021:2022), each = 52)
  ) |> 
  filter(geo_name != "Puerto Rico") |> 
  rename(state = geo_name) |>
  dplyr::select(state, year, gini, gini_se) ->
  gini_index

write_rds(gini_index, here("data", "2023-03-26-gini-index"))
