library(dplyr)
library(rvest)
library(glue)


# Get a list of all courts present on the IK portal
get_all_courts <- function(){
  advanced_url <- 'https://indiankanoon.org/advanced.html'
  all_courts_xpath <- '/html/body/div[2]/div/form/div[2]/table[2]'
  all_courts_list <- advanced_url %>% read_html() %>%  html_nodes(xpath=all_courts_xpath) %>% html_children() %>% html_children() %>% html_children() %>% html_attrs() %>% unlist()
  all_courts_list <- all_courts_list[seq(3,length(all_courts_list),3),]
  names(all_courts_list)[] <- ''
  return(all_courts_list)
}

# Scraping stats for individual courts by citationId
get_court_cases_from_ik <- function( court_name, citedby){
  if(court_name == 'all'){
    ik_act_url <- glue::glue('https://indiankanoon.org/search/?formInput=citedby%3A%20{citedby}')
  } else {
    ik_act_url <- glue::glue('https://indiankanoon.org/search/?formInput=citedby%3A%20{citedby}+doctypes:{court_name}')
  }
  ik_act_data <- ik_act_url %>% read_html() %>% html_nodes(xpath = '/html/body/div[1]/div[3]/div[1]/b') %>% html_text() 
  ik_act_data <- stringr::str_replace_all(string = ik_act_data, pattern = '[:digit:]+ - [:digit:]+ of ',replacement = '') %>% stringr::str_trim() %>% as.numeric()
  ik_act_title <- ik_act_url %>% 
    read_html() %>% 
    html_nodes(css = '.document_cite a') %>% 
    html_text() %>% 
    stringr::str_remove_all("\\\n") %>% 
    stringr::str_squish()
  
  if(is.na(ik_act_data)){
    ik_act_data <- list('title' = ik_act_title, 'citedby'=citedby,'court'=court_name,'total_cases'= 0)  
  } else {
    ik_act_data <- list('title' = ik_act_title, 'citedby'=citedby,'court'=court_name,'total_cases'=ik_act_data)  
  }
  return(ik_act_data)
}

# Scraping stats for all high courts and supreme court
ik_case_summary_geography <- function(citedby){
  # get_all_courts <- get_all_courts()
  
  print(glue::glue("Processing citation Id: {citedby}"))
  all_courts_list <- c("supremecourt","scorders","allahabad","andhra",
                       "bombay","chattisgarh","chennai","delhi",
                       "gauhati","gujarat","himachal_pradesh","jammu",
                       "jharkhand","karnataka","kerala","kolkata",
                       "lucknow","madhyapradesh","orissa","patna",
                       "punjab","rajasthan","sikkim","uttaranchal",
                       "kolkata_app","jodhpur","patna_orders","srinagar",
                       "meghalaya","tripura","delhidc","bangaloredc")
  
  all_courts_list <- c("all",all_courts_list)
  
  act_geography_summary <- list()
  for(i in 1:length(all_courts_list)){
    print(glue::glue('Processing for {all_courts_list[[i]]}'))
    case_summary <- get_court_cases_from_ik(all_courts_list[[i]], citedby)
    act_geography_summary[[i]] <- case_summary
  }
  act_geography_summary_df <- dplyr::bind_rows(act_geography_summary)
  return(act_geography_summary_df)
}

# Generate a summary of all citations of the acts and all its sections across geographies
generate_act_summary <- function(act_id){
  ik_act_url <- glue::glue('https://indiankanoon.org/doc/{act_id}/')
  
  print('Fetching links to sections and sub sections')
  all_sections_of_act_link <- ik_act_url %>% 
    read_html() %>% 
    html_nodes(css = ".article a") %>% 
    html_attr('href') %>% 
    unlist()
  
  section_id <- all_sections_of_act_link %>% 
    stringr::str_replace_all(pattern = "\\/doc\\/",replacement = "") %>%
    stringr::str_trim() %>% 
    stringr::str_replace_all(pattern = "\\/",replacement = "")
  
  # Adding the parent act
  section_id <- c(act_id, section_id)
  
  print('Starting to fetch citations ... ')
  all_sections_citations_df <- lapply(section_id, ik_case_summary_geography) %>%
    dplyr::bind_rows()
  
  return(all_sections_citations_df)
}
