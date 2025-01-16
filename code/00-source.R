library(here)
library(tidyverse)
library(janitor)
library(glue)
# packages specific to "02-use-get-acs-for-control-variables.R"
library(tidycensus)
library(tigris)
library(units)
# package specific to "03-get-trends.R"
library(trendyy)
library(gtrendsR)
# packages specific to "04-justify-aggregation.R"
library(multilevel)
library(lavaan)
library(irr)
# packages specific to "05-analyze-the-gini-search-data.R"
library(fixest)
library(lme4)
library(lmerTest)
library(performance)
library(broom.mixed)
library(Hmisc)

# Only necessary if collecting the Google Trends data "03-get-trends.R"
# dir.create(here("data", today()), showWarnings = FALSE)

source("01-wrangle-gini-index-data.R")
source("02-use-get-acs-for-control-variables.R")
source("03-get-trends.R")
source("04-justify-aggregation.R")
source("05-analyze-the-gini-search-data.R")

