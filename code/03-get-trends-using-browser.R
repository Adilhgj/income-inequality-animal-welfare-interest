library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(here)
library(janitor)
library(tidyverse)
library(glue)


# The data come from Google Trends
search_interest <- list.files(
  path = here("data-raw", "search-interest"),
  full.names = TRUE
) |> 
  map_df(~{
    df <- read_csv(
      file = .,
      show_col_types = FALSE,
      skip = 2,
      col_names = TRUE
    )
    
    # Extract year and keyword from the first column name
    header <- names(df)[2]
    year <- str_extract(header, "\\d{4}")
    keyword <- str_remove(header, ":\\s*\\(\\d{4}\\)$")
    # Update keywords to align with previous format (2006:2019)
    keyword <- gsub('"', '', keyword)
    keywords <- str_split(keyword, " \\+ ")[[1]]
    final_keywords <- paste(keywords, collapse = " + ")
    # Rename columns
    names(df)[2] <- "search_interest"
    
    # Add year and keyword columns
    df <- df |> 
      mutate(
        year = year,
        keyword = final_keywords,
        sample = NA_integer_
      )
    
    return(df)
  }) |> 
  # Add sample columns
  group_by(year, keyword) |> 
  mutate(sample = rep(1:10, each = 51, length.out = n())) |> 
  ungroup() |> 
  # Reorder the columns
  select(year, state = Region, keyword, sample, search_interest)



write_rds(
  search_interest,
  here("data", glue("{today()}-search-interest"))
)


