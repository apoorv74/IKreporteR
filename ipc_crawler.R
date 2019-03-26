library(Rcrawler)

act_id <- 1569253
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


ik_act_urls <- paste0('https://indiankanoon.org/search/?formInput=citedby%3A%20',section_id)

# all_courts_list <- c("","supremecourt","scorders","allahabad","andhra",
#                      "bombay","chattisgarh","chennai","delhi",
#                      "gauhati","gujarat","himachal_pradesh","jammu",
#                      "jharkhand","karnataka","kerala","kolkata",
#                      "lucknow","madhyapradesh","orissa","patna",
#                      "punjab","rajasthan","sikkim","uttaranchal",
#                      "kolkata_app","jodhpur","patna_orders","srinagar",
#                      "meghalaya","tripura","delhidc","bangaloredc")

# all_courts_list <- c("","supremecourt","allahabad","andhra",
#                      "bombay","chattisgarh","chennai","delhi",
#                      "gauhati","gujarat","himachal_pradesh","jammu",
#                      "jharkhand","karnataka","kerala","kolkata",
#                      "lucknow","madhyapradesh","orissa","patna",
#                      "punjab","rajasthan","sikkim","uttaranchal",
#                      "jodhpur","srinagar",
#                      "meghalaya","tripura")
# 
# ik_act_urls <- expand.grid(ik_act_urls, all_courts_list)
# ik_act_urls <- paste0(ik_act_urls$Var1,ik_act_urls$Var2)


# listUrls <- paste0("https://indiankanoon.org/doc/",section_id,"/")
listUrls <- ik_act_urls
citations <- Rcrawler::ContentScraper(Url = listUrls, XpathPatterns = c("/html/body/div[1]/div[3]/div[1]/b","/html/body/div[1]/div[3]/div[2]/a"),ManyPerPattern = TRUE,asDataFrame = TRUE)
cite_df <- data.frame('total_cites' = unlist(citations$X1), 'section_name' = unlist(citations$X2))
cite_df$section_id <- section_id

remaining_citations <- cite_df[cite_df$section_name == 'HTTP error code:503',]
listUrls <- paste0('https://indiankanoon.org/search/?formInput=citedby%3A%20',remaining_citations$section_id)
citations_rem <- Rcrawler::ContentScraper(Url = listUrls, XpathPatterns = c("/html/body/div[1]/div[3]/div[1]/b","/html/body/div[1]/div[3]/div[2]/a"),ManyPerPattern = TRUE,asDataFrame = TRUE)

cite_df_rem <- data.frame('total_cites' = unlist(citations_rem$X1), 'section_name' = unlist(citations_rem$X2))
cite_df_rem$section_id <- remaining_citations$section_id


cite_all <- dplyr::bind_rows(cite_df[cite_df$section_name != 'HTTP error code:503',],cite_df_rem)

remaining_citations_2 <- cite_all[cite_all$section_name == 'HTTP error code:503',]
listUrls <- paste0('https://indiankanoon.org/search/?formInput=citedby%3A%20',remaining_citations_2$section_id)
citations_rem_2 <- Rcrawler::ContentScraper(Url = listUrls, XpathPatterns = c("/html/body/div[1]/div[3]/div[1]/b","/html/body/div[1]/div[3]/div[2]/a"),ManyPerPattern = TRUE,asDataFrame = TRUE)

cite_df_rem_2 <- data.frame('total_cites' = unlist(citations_rem_2$X1), 'section_name' = unlist(citations_rem_2$X2))
cite_df_rem_2$section_id <- remaining_citations_2$section_id

cite_all_2 <- dplyr::bind_rows(cite_all[cite_all$section_name != 'HTTP error code:503',],cite_df_rem_2)

# Cleaning citations
cite_clean <- cite_all_2
cite_clean$total_cites <- stringr::str_replace_all(string = cite_clean$total_cites,pattern = "[:digit:]+ - [:digit:]+ of ",replacement = "")
cite_clean$total_cites <- ifelse(cite_clean$total_cites == "No matching results",0,cite_clean$total_cites)
cite_clean$total_cites <- as.numeric(cite_clean$total_cites)

write.csv(cite_clean,"ipc_section_citations.csv", row.names = FALSE)
