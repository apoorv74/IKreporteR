library(dplyr)
library(rvest)
library(glue)

# check if an ID is an Act/Section or a case from the Supreme Court

# citation_id <- '19583647'
check_act_section_id <- function(citation_id){
  page_url <- glue::glue('https://indiankanoon.org/doc/{citation_id}/')
  check_for_text <- page_url %>% read_html() %>% html_nodes(css='.docsource_main') %>% html_text()
  if(check_for_text == 'Central Government Act'){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Get Act/Section details from citation ID
# Get section headings
get_section_title <- function(citedby){
  Sys.sleep(sample(seq(0.5,1,0.1),1))
  section_link <- glue::glue('https://indiankanoon.org/doc/{citedby}')
  tryCatch({
    all_sections_title <- section_link %>%
      read_html() %>%
      html_nodes(css = ".acts .doc_title") %>%
      html_text() %>% unlist()
    
    act_title <- stringr::str_replace_all(string = stringr::str_to_lower(all_sections_title),
                                          pattern = "section [\\S]+ in the ",
                                          replacement = ""
    )
    
    if(is.na(all_sections_title)){
      all_sections_title <- NA_character_
      act_title <- NA_character_
    }
  }, error = function(e){
    all_sections_title <- NA_character_
    act_title <- NA_character_
  })
  return(data.frame('citedby'=citedby, 'section_title'=all_sections_title, 'act_title' = act_title))
  close.connection(section_link)
}

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
get_court_cases_from_ik <- function( court_name, citedby, from_date=NULL, to_date=NULL, online_flag=0){
  
  Sys.sleep(sample(seq(0.5,2,0.1),1))
  
  if(court_name == 'all'){
    if(length(from_date)>0){
      ik_act_url <- glue::glue('http://indiankanoon.org/search/?formInput=citedby%3A+{citedby}+fromdate%3A+{from_date}+todate%3A+{to_date}+doctypes:judgments')
    } else {
      ik_act_url <- glue::glue('http://indiankanoon.org/search/?formInput=citedby%3A%20{citedby}+doctypes%3A+judgments')  
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
  if(online_flag == 0){
    act_name <- ipc_section_citations$act_name[ipc_section_citations$section_id == citedby]
    section_name <- ipc_section_citations$section_name[ipc_section_citations$section_id == citedby]  
  } else {
    act_name <- ""
    section_name <- ""
  }
   
  court_name <- court_df$court_name[court_df$court_id == court_name]
  if(is.na(ik_act_data)){
    ik_act_url <- 'https:://indiankanoon.org/'
    total_judgements <- 0
  } else {
    total_judgements <- as.numeric(ik_act_data)
  }
  
  ik_link <- createLink(ik_act_url)
  
  if(is.null(from_date)){
    from_date <- 'Not selected'
    to_date <- 'Not selected'
  }
  
  ik_act_data <- list('Act' = act_name, 'Section'= section_name,'Court' = court_name,'Judgements' = total_judgements,'IK'=ik_link,'FromDate' = from_date,'TillDate' = to_date)
  
  return(ik_act_data)
}

# Scraping stats for all high courts and supreme court
ik_case_summary_geography <- function(citedby, court_list, from_date=NULL, to_date=NULL, online_flag=0){
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
    case_summary <- get_court_cases_from_ik(all_courts_list[[i]], citedby, from_date = from_date, to_date = to_date, online_flag)
    act_geography_summary[[i]] <- case_summary
  }
  act_geography_summary_df <- dplyr::bind_rows(act_geography_summary)
  
  ## Fetch Section and Act names from the IK website. 
  # This takes care of cases where the search is done via the Act ID
  
  if(online_flag == 1){
      act_details_df <- get_section_title(citedby)
      act_geography_summary_df$Act <- stringr::str_to_title(act_details_df$act_title)
      act_geography_summary_df$Section <- stringr::str_to_title(act_details_df$section_title)
  }
  
  # Find case contribution percent
  total_cases <- act_geography_summary_df$`Judgements`[act_geography_summary_df$`Court` == 'All courts']
  
  # To avoid avraging ranks when equal, we are using 'min' as a method of ties - This will ensure that
  # all courts with highest judgements are coloured
  act_geography_summary_df <- act_geography_summary_df %>% mutate('Percent' = paste0(round(`Judgements`/total_cases*100,2)," %"),
                                                                  'CourtRank' = rank(-Judgements,ties.method= "min")-1)
  
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
