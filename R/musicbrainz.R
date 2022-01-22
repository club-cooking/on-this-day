library(httr2)
library(glue)
library(dplyr)
library(purrr)
library(tidyr)

url_root <- "https://musicbrainz.org/ws/2/"
entity_type <- "release"
release_date <- "2010-01-27"

# base request
request <- request(url_root) %>% 
  req_url_path_append(entity_type) %>% 
  req_headers(
    Accept = "application/json"
  ) 

# first search
search <- request %>% 
  req_url_query(
    query = glue("date:{release_date}"), 
    limit = 100
    ) %>% 
  req_perform() %>% 
  resp_body_json() 

# get no. records/pages
n_results <- search$count
n_pages <- ceiling(n_results / 100)

# query full results set
result <- map_dfr(seq_along(1:n_pages), function(x){
  
  n <- x - 1
  offset = 100 * n
  
  result <- request %>% 
    req_url_query(
      query = glue("date:{release_date}"), 
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
result_exact <- result %>% 
  dplyr::filter(date == release_date) %>% 
  select(id, title, `artist-credit`, date, country, `label-info`)
