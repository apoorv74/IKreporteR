ik_act_url <- glue::glue('https://indiankanoon.org/doc/{1733066}/')

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

section_id <- unique(section_id)

# Get section headings
get_section_title <- function(citedby){
  Sys.sleep(5)
  section_link <- glue::glue('https://indiankanoon.org/doc/{citedby}')
  tryCatch({
    all_sections_title <- section_link %>%
      read_html() %>%
      html_nodes(css = ".acts .doc_title") %>%
      html_text() %>% unlist()
    if(is.na(all_sections_title)){
      all_sections_title <- NA_character_
    }
  }, error = function(e){
    all_sections_title <- NA_character_
  })
  print(all_sections_title)
  return(data.frame('citedby'=citedby, 'section_title'=all_sections_title))
  close.connection(section_link)
}

# get_section_title(1724893)
  
all_titles <- lapply(section_id, get_section_title)
all_titles_df <- dplyr::bind_rows(all_titles)
