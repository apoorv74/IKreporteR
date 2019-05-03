library(readr)
source("../ik_scrape_functions.R")

court_df <- data.frame('court_name' = c("All courts","Supreme court","Allahabad","Andhra Pradesh",
                                        "Bombay","Chattisgarh","Chennai","Delhi",
                                        "Gauhati","Gujarat","Himachal Pradesh","Jammu",
                                        "Jharkhand","Karnataka","Kerala","Kolkata",
                                        "Lucknow","Madhya Pradesh","Orissa","Patna",
                                        "Punjab","Rajasthan","Sikkim","Uttaranchal",
                                        "Jodhpur","Srinagar","Meghalaya","Tripura"), 
                       'court_id' = c('all',"supremecourt","allahabad","andhra",
                                        "bombay","chattisgarh","chennai","delhi",
                                        "gauhati","gujarat","himachal_pradesh","jammu",
                                        "jharkhand","karnataka","kerala","kolkata",
                                        "lucknow","madhyapradesh","orissa","patna",
                                        "punjab","rajasthan","sikkim","uttaranchal",
                                        "jodhpur","srinagar","meghalaya","tripura"))

ipc_section_citations <- read_csv("../ipc_section_citations.csv")
all_acts <- ipc_section_citations$section_name[!grepl(pattern = 'section',ignore.case = TRUE,x = ipc_section_citations$section_name)]
all_sections <- ipc_section_citations$section_name[grepl(pattern = 'section',ignore.case = TRUE,x = ipc_section_citations$section_name)]
all_sections <- c(all_acts, all_sections)

# Reference - https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">View Judgements</a>',val)
}
