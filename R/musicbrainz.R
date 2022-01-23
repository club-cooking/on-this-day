library(httr2)
library(glue)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

dir.create("data")

url_root <- "https://musicbrainz.org/ws/2/"
entity_type <- "release"

month_day <- "01-27" # pick month-day
year_range <- 1990:2015 # pick years
release_dates <- glue("{year_range}-{month_day}") # build date range

date_releases <- map_dfr(release_dates, function(x) {
  
  # base request
  request <- request(url_root) %>% 
    req_url_path_append(entity_type) %>% 
    req_headers(
      Accept = "application/json"
    ) 
  
  # first search
  search <- request %>% 
    req_url_query(
      query = glue("date:{x}"), 
      limit = 100
    ) %>% 
    req_perform() %>% 
    resp_body_json() 
  
  # get no. records/pages
  n_results <- search$count
  n_pages <- ceiling(n_results / 100)
  
  # query full results set
  result <- map_dfr(seq_along(1:n_pages), function(y){
    
    n <- y - 1
    offset = 100 * n
    
    result <- request %>% 
      req_url_query(
        query = glue("date:{x}"), 
        limit = 100, offset = offset
      ) %>% 
      req_throttle(rate = 30 / 60) %>% 
      req_perform() %>% 
      resp_body_json() 
    
    result$releases %>% 
      purrr::transpose() %>% 
      tidyr::unchop() %>% 
      as_tibble()
  })
  
  # exact release date matches
  result %>% 
    dplyr::filter(date == x) %>%
    select(id, title, `artist-credit`, date, country, `label-info`)
})

# tidy up data
date_releases_df <- date_releases %>% 
  unnest(cols = `artist-credit`) %>% 
  mutate(artist_name = map_chr(`artist-credit`, "name")) %>% 
  select(-`label-info`, -`artist-credit`) %>% 
  mutate(across(everything(), as.character)) %>% 
  dplyr::filter(artist_name != "Various Artists")

# export
write_csv(date_releases_df, glue("data/{month_day}-releases.csv"))
