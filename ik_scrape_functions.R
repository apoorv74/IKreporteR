library(dplyr)
library(rvest)
library(glue)

# Change date formats to be compatible with IK

change_date_format <- function(date_to_change){
  return(
    paste0(as.numeric(lubridate::day(date_to_change)),'-',
           as.numeric(lubridate::month(date_to_change)),'-',
           as.numeric(lubridate::year(date_to_change)))
  )
}

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
get_court_cases_from_ik <- function( court_name, citedby, from_date=NULL, to_date=NULL){
  
  Sys.sleep(sample(seq(0.5,2,0.1),1))
  
  if(court_name == 'all'){
    if(length(from_date)>0){
      ik_act_url <- glue::glue('http://indiankanoon.org/search/?formInput=citedby%3A+{citedby}+fromdate%3A+{from_date}+todate%3A+{to_date}')
    } else {
      ik_act_url <- glue::glue('http://indiankanoon.org/search/?formInput=citedby%3A%20{citedby}')  
    }
  } else {
    if(length(from_date)>0){
      ik_act_url <- glue::glue('http://indiankanoon.org/search/?formInput=citedby%3A+{citedby}%20fromdate%3A%20{from_date}%20todate%3A%20{to_date}+doctypes:{court_name}')  
    } else {
      ik_act_url <- glue::glue('http://indiankanoon.org/search/?formInput=citedby%3A%20{citedby}+doctypes:{court_name}')  
    }
  }
  
  print(ik_act_url)
  ik_act_title <- tryCatch({
    ik_act_url %>% 
      read_html() %>% 
      html_nodes(css = '.document_cite a') %>% 
      html_text() %>% 
      stringr::str_remove_all("\\\n") %>% 
      stringr::str_squish()
  }, error = function(e){
    'error'
  }) 
  
  ik_act_data <- tryCatch({
    ik_act_data <- ik_act_url %>% read_html() %>% html_nodes(xpath = '/html/body/div[1]/div[3]/div[1]/b') %>% html_text() 
    ik_act_data <- stringr::str_replace_all(string = ik_act_data, pattern = '[:digit:]+ - [:digit:]+ of ',replacement = '') %>% stringr::str_trim() %>% as.numeric()
  }, error = function(e){
    'error'
  })
  
  ## Prepare columns to be included in the final data frame
  act_name <- ipc_section_citations$act_name[ipc_section_citations$section_id == citedby]
  section_name <- ipc_section_citations$section_name[ipc_section_citations$section_id == citedby]
  court_name <- court_df$court_name[court_df$court_id == court_name]
  
  
  if(is.na(ik_act_data)){
    ik_act_url <- 'https:://indiankanoon.org/'
    total_judgements <- 0
  } else {
    total_judgements <- as.numeric(ik_act_data)
  }
  
  ik_link <- createLink(ik_act_url)
  ik_act_data <- list('Act' = act_name, 'Section'= section_name,'CourtName' = court_name,'TotalJudgements' = total_judgements,'IndianKanon link'=ik_link)
  
  return(ik_act_data)
}

# Scraping stats for all high courts and supreme court
ik_case_summary_geography <- function(citedby, court_list, from_date=NULL, to_date=NULL){
  # get_all_courts <- get_all_courts()
  
  all_courts_list <- unlist(court_list)
  all_courts_list <- court_df$court_id[court_df$court_name %in% all_courts_list]
  
  print(glue::glue("Processing citation Id: {citedby}"))
  # all_courts_list <- c("supremecourt","scorders","allahabad","andhra",
  #                      "bombay","chattisgarh","chennai","delhi",
  #                      "gauhati","gujarat","himachal_pradesh","jammu",
  #                      "jharkhand","karnataka","kerala","kolkata",
  #                      "lucknow","madhyapradesh","orissa","patna",
  #                      "punjab","rajasthan","sikkim","uttaranchal",
  #                      "kolkata_app","jodhpur","patna_orders","srinagar",
  #                      "meghalaya","tripura","delhidc","bangaloredc")
  
  all_courts_list <- c("all",all_courts_list)

  if(length(from_date)>0){
    from_date <- change_date_format(from_date)
    to_date <- change_date_format(to_date)  
  }
  act_geography_summary <- list()
  for(i in 1:length(all_courts_list)){
    print(glue::glue('Processing for {all_courts_list[[i]]}'))
    case_summary <- get_court_cases_from_ik(all_courts_list[[i]], citedby, from_date = from_date, to_date = to_date)
    act_geography_summary[[i]] <- case_summary
  }
  act_geography_summary_df <- dplyr::bind_rows(act_geography_summary)
  
  # Find case contribution percent
  total_cases <- act_geography_summary_df$`TotalJudgements`[act_geography_summary_df$`CourtName` == 'All courts']
  act_geography_summary_df <- act_geography_summary_df %>% mutate('CaseContribution' = paste0(round(`TotalJudgements`/total_cases*100,2)," %"),
                                                                  'CourtRank' = rank(-TotalJudgements)-1)
  
  return(act_geography_summary_df)
}

# Memoised copy of ik_case_summary_geography - for caching similar requests
ik_case_summary_geography_mem <- memoise::memoise(ik_case_summary_geography)

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
  
  # section_id <- 1873250
  print('Starting to fetch citations ... ')
  all_sections_citations_df <- lapply(section_id, ik_case_summary_geography) %>%
    dplyr::bind_rows()
  
  return(all_sections_citations_df)
}
