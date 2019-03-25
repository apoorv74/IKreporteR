
library(rvest)
library(stringr)

total_rows_cpath <- '.panel-footer'
# /html/body/main/div/div/div[2]/div/div[2]
# req_url <- 'https://indiacode.nic.in/handle/123456789/1362/browse?type=ministry'
req_url <- 'https://indiacode.nic.in/handle/123456789/1362/browse?type=shorttitle'

get_total_number <- function(req_url){
  response_object <- httr::VERB(verb = 'GET', req_url)
  response_content <- httr::content(response_object)
  total_rows <- response_content %>% html_nodes(css = total_rows_cpath) %>% html_text()
  total_rows <- stringr::str_replace_all(total_rows, "[\r\n\t]" , "") %>% stringr::str_trim()
  total_rows <-
    stringr::str_replace_all(string = total_rows,
                             replacement = '',
                             pattern = 'Showing items [:digit:]+ to [:digit:]+ of') %>% readr::parse_number()
  return(total_rows)
}

get_total_acts_by_ministry <- function(offset_num){
  request_url <- glue::glue('https://indiacode.nic.in/handle/123456789/1362/browse?type=ministry&order=ASC&rpp=20&offset={offset_num}')
  response_object <- httr::VERB(verb = 'GET', request_url)
  response_content <- httr::content(response_object)
  all_acts <- response_content %>% html_nodes(xpath = '/html/body/main/div/div/div[4]/div/div[2]/ul/li') %>% html_text()
  all_acts <- stringr::str_replace_all(all_acts, "[\r\n\t]" , "") %>% stringr::str_trim()
  total_number <- readr::parse_number(all_acts)
  return(total_number)
}

offset_values <- seq(0,20*(ceiling(get_total_number(req_url)/20)-1),20)
total_acts <- lapply(offset_values, get_total_acts_by_ministry) %>% unlist() %>% sum()

get_all_acts <- function(offset_num){
  print(glue::glue("Offset Number: {offset_num}"))
  request_url <- glue::glue('https://indiacode.nic.in/handle/123456789/1362/browse?type=shorttitle&sort_by=3&order=ASC&rpp=20&etal=-1&null=&offset={offset_num}')
  response_object <- httr::VERB(verb = 'GET', request_url)
  response_content <- httr::content(response_object)
  all_acts <- response_content %>% html_nodes(xpath = '/html/body/main/div/div/div[2]/div/table') %>% html_table() %>% dplyr::bind_rows()
  return(all_acts)
}

offset_values <- seq(0,20*(ceiling(get_total_number(req_url)/20)-1),20)

all_acts_df <- lapply(offset_values, get_all_acts) %>% dplyr::bind_rows()
all_acts_df$act_id <- 1:nrow(all_acts_df)

## Get cite number from Indian Kanoon

get_citation_number <- function(act_id){
  Sys.sleep(1)
  act_name <- all_acts_df$`Short Title`[all_acts_df$act_id == act_id]
  act_name <- stringr::str_replace_all(act_name, pattern =  "\\(Amendment\\)", replacement = "")
  act_name <- stringr::str_replace_all(string = act_name, pattern = "[\\s]+",replacement = " ")
  ik_url <- glue::glue('https://indiankanoon.org/search/?formInput={act_name}+doctypes:laws')
  ik_response <- httr::VERB(verb = 'GET', url = ik_url)
  ik_content <- httr::content(ik_response)
  all_titles <- ik_content %>% html_nodes(css = ".result_url") %>% html_text()
  if(length(all_titles) > 0){
    all_titles <- stringr::str_replace_all(string = all_titles, pattern = "Section [:digit:]+ in ",replacement = "")
    all_titles <- stringr::str_replace_all(string = all_titles, pattern = "[\\s]+",replacement = " ")
    did_xpath <- '/html/body/div[1]/div[3]/div[1]'
    did_text <- ik_content %>% html_nodes(xpath = did_xpath) %>% html_text()
    act_string_match <- which(stringr::str_to_lower(act_name) == stringr::str_to_lower(all_titles))
    if(length(act_string_match) > 0){
      if(stringr::str_detect(string = did_text,pattern = "Did")){
        act_index <- act_string_match[[1]]  + 2  
      } else {
        act_index <- act_string_match[[1]]  + 1  
      }
      } else {
      act_index <- 0
    }
    if(act_index > 0){
      act_xpath <- glue::glue("/html/body/div[1]/div[3]/div[{act_index}]/div[1]/a[2]")
      act_link <- ik_content %>% html_nodes(xpath = act_xpath) %>% html_attrs()
      act_link <-  unlist(act_link) %>% readr::parse_number()  
    } else {
      act_link <- 0
    }
  } else {
    act_link <- 0
  }
  print(glue::glue('Processing act number {act_id}: {act_name}: {act_link}'))
  return(act_link)
  }

all_act_id <- 1:nrow(all_acts_df)
# all_act_id <- 1:20
all_acts_cite_number <- lapply(all_act_id, get_citation_number) %>% unlist()
 
all_acts_df$ik_cite_number <- all_acts_cite_number

# i <- 1
# while(i<=10){
#   get_citation_number(678)
#   i <- i+1
# }

# data.table::fwrite(all_acts_df, "~/cdl/vayam/india_code/central_acts.csv")

# Reports from IK
source("ik_scrape_functions.R")

View(ik_case_summary_geography(1965344))

