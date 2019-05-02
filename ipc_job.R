library(dplyr)
library(rvest)
library(glue)
library(stringr)

source("ik_scrape_functions.R")

act_id <- 1873250
all_court_summary <- ik_case_summary_geography(act_id)

print("Exporting file ... ")
write.csv(all_court_summary, "ipc_all_court_summary.csv", row.names = FALSE)

