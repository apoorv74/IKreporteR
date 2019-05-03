library(readr)
source("../ik_scrape_functions.R")

ipc_section_citations <- read_csv("../ipc_section_citations.csv")
all_acts <- ipc_section_citations$section_name[!grepl(pattern = 'section',ignore.case = TRUE,x = ipc_section_citations$section_name)]
all_sections <- ipc_section_citations$section_name[grepl(pattern = 'section',ignore.case = TRUE,x = ipc_section_citations$section_name)]
all_sections <- c(all_acts, all_sections)