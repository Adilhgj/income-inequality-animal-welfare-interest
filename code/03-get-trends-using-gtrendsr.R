library(here)
library(tidyverse)
library(janitor)
library(glue)
library(trendyy)
library(gtrendsR)

c(
  # animal testing
  # https://trends.google.com/trends/explore?date=2006-01-01%202019-12-31&geo=US&q=%22animal%20testing%22,%22cruelty%20free%22
  # URLdecode("%27animal%20testing%27"),
  # URLdecode("%27cruelty%20free%27"),
  # # substitute
  # # https://trends.google.com/trends/explore?date=2006-01-01%202019-12-31&geo=US&q=vegan,vegetarian,%22dairy%20free%22,%22plant%20based%22,%22faux%20leather%22
  # "vegan",
  # "vegetarian",
  # URLdecode("%27dairy%20free%27"),
  # URLdecode("%27plant%20based%27"),
  # URLdecode("%27faux%20leather%27"),
  # # humane
  # # https://trends.google.com/trends/explore?date=2006-01-01%202019-12-31&geo=US&q=%22grass%20fed%22,%22free%20range%22,%22cage%20free%22,%22pasture%20raised%22
  # URLdecode("%27grass%20fed%27"),
  # URLdecode("%27free%20range%27"),
  # URLdecode("%27cage%20free%27"),
  # URLdecode("%27pasture%20raised%27"),
  # full and subs
  URLdecode("vegan%2Bvegetarian%2B%27dairy%20free%27%2B%27animal%20testing%27%2B%27cruelty%20free%27%2B%27grass%20fed%27%2B%27free%20range%27%2B%27cage%20free%27"),
  URLdecode("vegan%2Bvegetarian%2B%27dairy%20free%27"),
  URLdecode("%27animal%20testing%27%2B%27cruelty%20free%27"),
  URLdecode("%27grass%20fed%27%2B%27free%20range%27%2B%27cage%20free%27")
  # URLdecode("%27animal%20testing%27%2B%27cruelty%20free%27%2B%27dairy%20free%27%2Bvegan%2Bvegetarian%2B%27plant%20based%27%2B%27grass%20fed%27%2B%27cage%20free%27"),
  # URLdecode("%27animal%20testing%27%2B%27cruelty%20free%27"),
  # URLdecode("%27dairy%20free%27%2Bvegan%2Bvegetarian%2B%27plant%20based%27"),
  # URLdecode("%27grass%20fed%27%2B%27cage%20free%27")
) ->
  search_terms

c(
  # "animal testing",
  # "cruelty free",
  # "vegan",
  # "vegetarian",
  # "dairy free",
  # "plant based",
  # "faux leather",
  # "grass fed",
  # "free range",
  # "cage free",
  # "pasture raised",
  "new jmr cocktail",
  "new jmr cocktail (substitute)",
  "new jmr cocktail (testing)",
  "new jmr cocktail (humane)"
) ->
  topic_name

year      <- 2021:2022
sample    <- 1:10
wait_time <- 60
 
search_interest_year_state                <- NULL
search_interest_year_related_search_topic <- NULL
search_interest_year_related_search_query <- NULL

for(k in 1:length(sample)) {
  
  for(j in 1:length(search_terms)) {
    
    for(i in 1:length(year)) {
      
      this_year <- year[i]
      this_from <- paste0(this_year, "-01-01")
      this_to <- paste0(this_year, "-12-31")
      this_search_term <- search_terms[j]
      this_topic <- topic_name[j]
      this_sample <- sample[k]
      this_pulled_at <- floor_date(now(), unit = "minutes")
      this_variance <- runif(1, 0, 10)
      
      glue(
        "Processing data for sample {this_sample} ",
        "and {this_topic} in {this_year} at {this_pulled_at}"
      ) |> 
        message()
      
      query <- NULL # Clear the query object
      
      # Until a success occurs, retry
      while(is.null(query[[1]][["interest_by_region"]])) {
        
        # Attempt to generate the query object
        tryCatch(
          expr  = 
            {
              gtrends(
                keyword = this_search_term,
                geo          = "US",
                time         = paste(this_from, this_to)
              ) ->
                query
            },
          error = function(e) {
            if (grepl("429", conditionMessage(e))) {
              message("Server response: 429 - too many requests.")
            } else {
              message(conditionMessage(e))
            }
            return(NULL)
          }
        )
        
        # If query fails, wait a random number of seconds above the wait_time
        if(is.null(query)) {
          
          glue("Waiting {round(wait_time + this_variance, 2)} seconds...") |> 
            message()
          
          Sys.sleep(wait_time + this_variance)
          
        } else{
          
          query |> 
            pluck("interest_by_region") |> 
            as_tibble() |> 
            mutate(
              year            = this_year,
              topic_name      = this_topic,
              pulled_at       = this_pulled_at,
              sample          = this_sample
            ) |> 
            select(
              year,
              state           = location, 
              keyword,
              topic_name,
              pulled_at,
              sample,
              search_interest = hits
            ) |> 
            mutate(search_interest = replace_na(search_interest, 0)) ->
            this_search_interest_year_state
          
          if(this_sample > 1) {
            
            search_interest_year_state |>
              filter(year == this_year, topic_name == this_topic) |>
              filter(pulled_at == max(pulled_at)) |> 
              select(-pulled_at, -sample) |>
              identical(
                this_search_interest_year_state |> 
                  select(-pulled_at, -sample)
              ) ->
              is_identical_sample
            
            search_interest_year_state |>
              filter(year == this_year, topic_name == this_topic) |>
              summarize(pulled_at_max = max(pulled_at)) |>
              pull(pulled_at_max) ->
              pulled_at_max
            
            its_been_more_than_a_day <- pulled_at_max < now() - days(1)
            
            if(!is_identical_sample | its_been_more_than_a_day) {
              
              this_search_interest_year_state |> 
                bind_rows(search_interest_year_state) ->
                search_interest_year_state
              
            } else{
              
              query <- NULL # Clear the query object
              
              # Vary the wait time between 30 and 60 minutes
              wait_time_new_sample <- runif(1, 30 * 60, 60 * 60)
              
              glue(
                "Identical sample detected. ",
                "Waiting {round(wait_time_new_sample / 60, 0)} ",
                "minutes to try again..."
              ) |> 
                message()
              
              Sys.sleep(wait_time_new_sample)
              
            }
            
          } else{
            
            this_search_interest_year_state |> 
              bind_rows(search_interest_year_state) ->
              search_interest_year_state
            
          }
          
        } 
        
      }
      
      if(!is.null(query[[1]][["related_topics"]][1])) {
        
        if(nrow(as.data.frame(query[[1]][["related_topics"]][1])) > 1) {
          
          suppressWarnings({
            
            query |> 
              pluck("related_topics") |> 
              as_tibble() |> 
              mutate(
                year            = this_year,
                topic_name      = this_topic,
                pulled_at       = this_pulled_at,
                sample          = this_sample,
                search_interest = if_else(
                  subject == "Breakout",
                  Inf,
                  subject |> 
                    str_remove_all("\\+|%") |> 
                    as.numeric()
                )
              ) |> 
              select(
                year,
                top_or_rising = related_topics,
                related_search_query = value,
                keyword,
                topic_name,
                category,
                pulled_at,
                sample,
                search_interest
              ) |> 
              bind_rows(search_interest_year_related_search_topic) ->
              search_interest_year_related_search_topic
            
          })
          
        }
        
      }
      
      if(!is.null(query[[1]][["related_queries"]])) {
        
        if(nrow(query[[1]][["related_queries"]][1]) > 1) {
          
          suppressWarnings({
            
            query |> 
              pluck("related_queries") |> 
              as_tibble() |> 
              # get_related_queries() |> 
              mutate(
                year            = this_year,
                topic_name      = this_topic,
                pulled_at       = this_pulled_at,
                sample          = this_sample,
                search_interest = if_else(
                  subject == "Breakout",
                  Inf,
                  subject |> 
                    str_remove_all("\\+|%") |> 
                    as.numeric()
                )
              ) |> 
              select(
                year,
                top_or_rising = related_topics,
                related_search_query = value,
                keyword,
                topic_name,
                category,
                pulled_at,
                sample,
                search_interest
              ) |> 
              bind_rows(search_interest_year_related_search_query) ->
              search_interest_year_related_search_query
            
          })
          
        }
        
      }
      
      Sys.sleep(wait_time + this_variance)
      
    }
    
  }
  
}

write_rds(
  search_interest_year_state,
  here("data", glue("{today()}-search-interest-year-state"))
)

write_rds(
  search_interest_year_related_search_topic,
  here("data", glue("{today()}-search-interest-year-related-search-topic"))
)

write_rds(
  search_interest_year_related_search_query,
  here("data", glue("{today()}-search-interest-year-related-search-query"))
)
