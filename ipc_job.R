library(dplyr)
library(rvest)
library(glue)
library(stringr)

source("ik_scrape_functions.R")

act_id <- 1569253
all_court_summary <- generate_act_summary(act_id)

print("Exporting file ... ")
write.csv(all_court_summary, "ipc_all_court_summary.csv", row.names = FALSE)
