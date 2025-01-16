library(tidyverse)
library(tigris)
library(tidycensus)
library(units)

# Get state sizes
states() |> 
  mutate(
    square_miles = ALAND |> 
      units::set_units(m^2) |> 
      units::set_units(mi^2) |> 
      as.numeric()
  ) |> 
  dplyr::select(NAME, square_miles) |>
  tibble() |> 
  dplyr::select(-geometry) ->
  state_size

# The variable names in the American Community Survey data change from one year
# to the next on occasion. Create a covariate-name crosswalk connecting the
# covariates we want to control for with their variable names in a given year. I
# used the tidycensus::load_variables() function to find the appropriate
# variable names. For example:
# load_variables(year = 2007, dataset = "acs1/profile", cache = TRUE)

covariate_name_crosswalk <- tribble(
  ~year, ~variable, ~name,
  2006, "income_mean", "DP03_0062",
  2007, "income_mean", "DP03_0064",
  2008, "income_mean", "DP03_0064",
  2009, "income_mean", "DP03_0064",
  2010, "income_mean", "DP03_0063",
  2011, "income_mean", "DP03_0063",
  2012, "income_mean", "DP03_0063",
  2013, "income_mean", "DP03_0063",
  2014, "income_mean", "DP03_0063",
  2015, "income_mean", "DP03_0063",
  2016, "income_mean", "DP03_0063",
  2017, "income_mean", "DP03_0063",
  2018, "income_mean", "DP03_0063",
  2019, "income_mean", "DP03_0063",
  2021, "income_mean", "DP03_0063",
  2022, "income_mean", "DP03_0063",
  2006, "income_median", "DP03_0061",
  2007, "income_median", "DP03_0063",
  2008, "income_median", "DP03_0063",
  2009, "income_median", "DP03_0063",
  2010, "income_median", "DP03_0062",
  2011, "income_median", "DP03_0062",
  2012, "income_median", "DP03_0062",
  2013, "income_median", "DP03_0062",
  2014, "income_median", "DP03_0062",
  2015, "income_median", "DP03_0062",
  2016, "income_median", "DP03_0062",
  2017, "income_median", "DP03_0062",
  2018, "income_median", "DP03_0062",
  2019, "income_median", "DP03_0062",
  2021, "income_median", "DP03_0062",
  2022, "income_median", "DP03_0062",
  2006, "age_median", "DP05_0017",
  2007, "age_median", "DP05_0017",
  2008, "age_median", "DP05_0017",
  2009, "age_median", "DP05_0017",
  2010, "age_median", "DP05_0017",
  2011, "age_median", "DP05_0017",
  2012, "age_median", "DP05_0017",
  2013, "age_median", "DP05_0017",
  2014, "age_median", "DP05_0017",
  2015, "age_median", "DP05_0017",
  2016, "age_median", "DP05_0017",
  2017, "age_median", "DP05_0018",
  2018, "age_median", "DP05_0018",
  2019, "age_median", "DP05_0018",
  2021, "age_median", "DP05_0018",
  2022, "age_median", "DP05_0018",
  2006, "bachelors_or_higher_percent", "DP02_0066",
  2007, "bachelors_or_higher_percent", "DP02_0066",
  2008, "bachelors_or_higher_percent", "DP02_0067",
  2009, "bachelors_or_higher_percent", "DP02_0067",
  2010, "bachelors_or_higher_percent", "DP02_0067P",
  2011, "bachelors_or_higher_percent", "DP02_0067P",
  2012, "bachelors_or_higher_percent", "DP02_0067P",
  2013, "bachelors_or_higher_percent", "DP02_0067P",
  2014, "bachelors_or_higher_percent", "DP02_0067P",
  2015, "bachelors_or_higher_percent", "DP02_0067P",
  2016, "bachelors_or_higher_percent", "DP02_0067P",
  2017, "bachelors_or_higher_percent", "DP02_0067P",
  2018, "bachelors_or_higher_percent", "DP02_0067P",
  2019, "bachelors_or_higher_percent", "DP02_0068P",
  2021, "bachelors_or_higher_percent", "DP02_0068P",
  2022, "bachelors_or_higher_percent", "DP02_0068P",
  # The 2006 ACS data do not include percent variables. Must calculate manually.
  2006, "management_percent", "DP03_0027",
  2006, "population_management_delete", "DP03_0026",
  2007, "management_percent", "DP03_0027P",
  2008, "management_percent", "DP03_0027P",
  2009, "management_percent", "DP03_0027P",
  2010, "management_percent", "DP03_0027P",
  2011, "management_percent", "DP03_0027P",
  2012, "management_percent", "DP03_0027P",
  2013, "management_percent", "DP03_0027P",
  2014, "management_percent", "DP03_0027P",
  2015, "management_percent", "DP03_0027P",
  2016, "management_percent", "DP03_0027P",
  2017, "management_percent", "DP03_0027P",
  2018, "management_percent", "DP03_0027P",
  2019, "management_percent", "DP03_0027P",
  2021, "management_percent", "DP03_0027P",
  2022, "management_percent", "DP03_0027P",
  # The 2006 ACS data do not include percent variables. Must calculate manually.
  2006, "male_percent", "DP05_0002",
  2007, "male_percent", "DP05_0002P",
  2008, "male_percent", "DP05_0002P",
  2009, "male_percent", "DP05_0002P",
  2010, "male_percent", "DP05_0002P",
  2011, "male_percent", "DP05_0002P",
  2012, "male_percent", "DP05_0002P",
  2013, "male_percent", "DP05_0002P",
  2014, "male_percent", "DP05_0002P",
  2015, "male_percent", "DP05_0002P",
  2016, "male_percent", "DP05_0002P",
  2017, "male_percent", "DP05_0002P",
  2018, "male_percent", "DP05_0002P",
  2019, "male_percent", "DP05_0002P",
  2021, "male_percent", "DP05_0002P",
  2022, "male_percent", "DP05_0002P",
  # The 2006 ACS data do not include percent variables. Must calculate manually.
  2006, "white_percent", "DP05_0032",
  2006, "population_white_delete", "DP05_0030",
  2007, "white_percent", "DP05_0032P",
  2008, "white_percent", "DP05_0032P",
  2009, "white_percent", "DP05_0032P",
  2010, "white_percent", "DP05_0032P",
  2011, "white_percent", "DP05_0032P",
  2012, "white_percent", "DP05_0032P",
  2013, "white_percent", "DP05_0032P",
  2014, "white_percent", "DP05_0032P",
  2015, "white_percent", "DP05_0032P",
  2016, "white_percent", "DP05_0032P",
  2017, "white_percent", "DP05_0037P",
  2018, "white_percent", "DP05_0037P",
  2019, "white_percent", "DP05_0037P",
  2021, "white_percent", "DP05_0037P",
  2022, "white_percent", "DP05_0037P",
  2006, "population", "DP05_0001",
  2007, "population", "DP05_0001",
  2008, "population", "DP05_0001",
  2009, "population", "DP05_0001",
  2010, "population", "DP05_0001",
  2011, "population", "DP05_0001",
  2012, "population", "DP05_0001",
  2013, "population", "DP05_0001",
  2014, "population", "DP05_0001",
  2015, "population", "DP05_0001",
  2016, "population", "DP05_0001",
  2017, "population", "DP05_0001",
  2018, "population", "DP05_0001",
  2019, "population", "DP05_0001",
  2021, "population", "DP05_0001",  
  2022, "population", "DP05_0001"  
)

covariate <- NULL
for(i in c(2006:2019, 2021:2022)) {
  tidycensus::get_acs(
    geography = "state",
    variables = c(
      income_mean = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "income_mean") |> 
        pull(name),
      income_median = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "income_median") |> 
        pull(name),
      age_median = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "age_median") |> 
        pull(name),
      male_percent = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "male_percent") |> 
        pull(name),
      white_percent = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "white_percent") |> 
        pull(name),
      bachelors_or_higher_percent = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "bachelors_or_higher_percent") |> 
        pull(name),
      management_percent = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "management_percent") |> 
        pull(name),
      population = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "population") |> 
        pull(name),
      population_management_delete = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "population_management_delete") |> 
        pull(name),
      population_white_delete = 
        covariate_name_crosswalk |> 
        filter(year == i, variable == "population_white_delete") |> 
        pull(name)
    ),
    survey = "acs1",
    year = i
  ) |> 
    mutate(year = i) |> 
    pivot_wider(
      id_cols = c("year", "NAME"), 
      names_from = "variable",
      values_from = "estimate"
    ) |>  
    left_join(state_size) |> 
    mutate(
      income_mean        = income_mean / 10000,
      population_density = (population / square_miles) / 100
    ) |> 
    dplyr::select(-square_miles) |>
    rename(state = NAME) ->
    this_covariate
  
  # The 2006 ACS data do not include percent vars. Must calculate manually.
  if(i == 2006) {
    
    this_covariate |> 
      mutate(
        management_percent = (management_percent / population_management_delete) 
        * 100,
        male_percent       = (male_percent / population) * 100,
        white_percent      = (white_percent / population_white_delete) * 100
      ) |> 
      dplyr::select(-contains("delete")) ->
      this_covariate
    
  }
  
  covariate |> 
    bind_rows(this_covariate) ->
    covariate
}

write_rds(covariate, here("data", "2023-03-26-covariate"))
