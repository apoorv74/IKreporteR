library(readr)
source("R/ik_scrape_functions.R")
source("R/about_application.R")

court_df <- data.frame('court_name' = c("All courts","Supreme court","Allahabad","Andhra Pradesh",
                                        "Bombay","Chattisgarh","Chennai","Delhi",
                                        "Gauhati","Gujarat","Himachal Pradesh","Jammu & Kashmir",
                                        "Jharkhand","Karnataka","Kerala","Kolkata",
                                        "Lucknow (B)","Madhya Pradesh","Orissa","Patna",
                                        "Punjab","Rajasthan","Sikkim","Uttarakhand",
                                        "Jodhpur (B)","Srinagar (B)","Meghalaya","Tripura"), 
                       'court_id' = c('all',"supremecourt","allahabad","andhra",
                                        "bombay","chattisgarh","chennai","delhi",
                                        "gauhati","gujarat","himachal_pradesh","jammu",
                                        "jharkhand","karnataka","kerala","kolkata",
                                        "lucknow","madhyapradesh","orissa","patna",
                                        "punjab","rajasthan","sikkim","uttaranchal",
                                        "jodhpur","srinagar","meghalaya","tripura"),
                       'jurisdiction' = c("all courts","Supreme Court","Uttar Pradesh",
                                            "Andhra Pradesh and Telangana","Goa, Dadra and Nagar Haveli, Daman and Diu, Maharashtra",
                                            "Chhattisgarh","Pondicherry, Tamil Nadu","National Capital Territory of Delhi",
                                            "Arunachal Pradesh, Assam, Mizoram, Nagaland","Gujarat",
                                            "Himachal Pradesh","Jammu and Kashmir","Jharkhand","Karnataka",
                                            "Kerala, Lakshadweep","Andaman and Nicobar Islands, West Bengal",
                                            "Uttar Pradesh","Madhya Pradesh","Odisha","Bihar","Chandigarh, Haryana, Punjab",
                                            "Rajasthan","Sikkim","Uttarakhand","Jodhpur","Srinagar","Meghalaya","Tripura"),
                       'formatcolor' = c('#52aa8a', '#08b2e3',rep('#fed766', 26)))
court_df[] <- sapply(court_df,as.character)
ipc_section_citations <-
  read_csv(
    "data/ipc_section_citations.csv",
    col_types = cols(
      act_name = col_character(),
      section_id = col_double(),
      section_name = col_character()
    )
  )
all_acts <- unique(ipc_section_citations$act_name)

# Reference - https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-primary">View Judgements</a>',val)
}


