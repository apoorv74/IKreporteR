library(shiny)
library(DT)
library(shinyjs)
library(rvest)

shinyServer(function(input, output, session) {
  
  # This flag controls the number of courts selected from the UI. If the user
  # selectes more than 5 courts, then this flag toggles to FALSE and the
  # user is forced to re-select the courts
  reactiveFlags <- reactiveValues(court_number_flag = TRUE)
  
  # Select sections
  
  output$section_explorer <- renderUI({
    all_sections <- ipc_section_citations$section_name[ipc_section_citations$act_name == input$select_act]
    selectizeInput('select_section', 'Select section to explore',
                   choices = all_sections)
  })
  
  # Enable/Disable date picker
  
  output$selectDate <- renderUI({
    if(input$dateid == TRUE){
      dateRangeInput(
        inputId = "dates",
        label = "Choose start and end dates",
        start = Sys.Date() - 60,
        end = Sys.Date(),
        max = Sys.Date(),
        width = "300px"
      )
    }
  })
  
  # Enable/Disable court selector
  
  output$selectCourts <- renderUI({
    if(input$courtid == TRUE){
      checkboxGroupButtons(
        inputId = "selectcourts", label = "Select courts to explore (Max 5):", 
        choices = court_df$court_name[!grepl(court_df$court_name,pattern = 'all',ignore.case = TRUE)], 
        justified = FALSE, status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      )
    }
  })
  
  observeEvent(input$refresh, {
    shinyjs::show("exportCSV")
    shinyjs::show("exportJSON")
    reactiveFlags$court_number_flag <- TRUE
  })
  
  observeEvent(input$refresh, {
    if(input$courtid == TRUE && length(input$selectcourts) > 5){
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = "Please select at-most 5 courts",
        type = "error"
      )
      reactiveFlags$court_number_flag <- FALSE
    }
  })
  
  cases_by_courts <- eventReactive(input$refresh, {
    if(reactiveFlags$court_number_flag == TRUE) {
      cited_by_section <- input$select_section
    cited_by_id <- ipc_section_citations$section_id[ipc_section_citations$section_name == cited_by_section]
    if(input$courtid == TRUE) {
      court_list <- input$selectcourts
    } else {
      # By default (without selection of courts from the UI), stats from only Supreme court will be fetched
      court_list <- 'Supreme court'
    }
    if(input$dateid == TRUE){
      from_date <- input$dates[[1]]
      to_date <- input$dates[[2]]
    } else {
      from_date <- NULL
      to_date <- NULL
    }
    ik_case_summary_geography_mem(cited_by_id, court_list, from_date, to_date)
    } else {
      NULL
    }
  })
  

output$caseAggregator <- DT::renderDataTable({
  if(reactiveFlags$court_number_flag == TRUE){
    cases_by_courts() %>% DT::datatable(escape = FALSE, class = 'row-border hover nowrap') %>%
      DT::formatStyle(
        columns = 'CourtName',
        backgroundColor = DT::styleEqual(c(unique(
          cases_by_courts()$CourtName
        )),
        c(court_df$formatcolor[court_df$court_name %in% cases_by_courts()$CourtName])),
        fontWeight = 'bold'
      ) %>%
      DT::formatStyle(
        columns = 'CaseContribution',
        valueColumns = 'CourtRank',
        backgroundColor = styleEqual(c(1), c('#bc4b51'))
      )} else {
        cases_by_courts()
      }
  })
  

cases_by_courts_export <- eventReactive(input$refresh,{
  if(reactiveFlags$court_number_flag == TRUE) {
    cases_by_courts <- cases_by_courts() %>% data.frame()
    ik_link <- cases_by_courts$IndianKanonLink
    ik_link <- stringr::str_replace_all(ik_link,'""','"')
    ik_link <- paste0(ik_link, collapse = "")
    ik_link <- ik_link %>% read_html() %>% html_nodes('a') %>% html_attr('href') %>% unlist()
    cases_by_courts$IndianKanonLink <- ik_link
    cases_by_courts
  } else {
    NULL
  }
})

output$exportCSV <- downloadHandler(
  filename = function() {
    "ik_court_aggregates.csv"
  },
  content = function(file) {
    write.csv(cases_by_courts_export(), file, row.names = FALSE)
  }
)

output$exportJSON <- downloadHandler(
  filename = function() {
    "ik_court_aggregates.json"
  },
  content = function(file) {
    jsonlite::write_json(cases_by_courts_export(), file)
  }
)
  
  # disable the downdload buttons on page load
  shinyjs::hide("exportCSV")
  shinyjs::hide("exportJSON")
})
