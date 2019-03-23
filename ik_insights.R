
library(magrittr)
library(rvest)
ik_url <- 'https://indiankanoon.org/search/?formInput=citedby%3A+1641007'

ik_request <- httr::VERB(verb = 'GET',url = ik_url)
ik_response <- httr::content(ik_request)

# fetching insights from response


# Total cases -------------------------------------------------------------
total_data_points <-
  ik_response  %>% 
  html_nodes(xpath = "/html/body/div/div[3]/div[1]/b") %>% 
  html_text() %>% 
  stringr::str_replace_all(pattern = "[:digit:]+ - [:digit:]+ of ", "") %>% 
  as.numeric()

# By geography



# DataFrame of cases ------------------------------------------------------

case_title <- ik_response %>% 
  html_nodes(css = ".result_url") %>% 
  html_text()
