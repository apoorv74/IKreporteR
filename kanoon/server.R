library(shiny)
library(DT)
library(shinyjs)
library(rvest)

shinyServer(function(input, output, session) {
  
  # This flag controls the number of courts selected from the UI. If the user
  # selectes more than 5 courts, then this flag toggles to FALSE and the
  # user is forced to re-select the courts
  reactiveFlags <- reactiveValues(court_number_flag = TRUE,
                                  valid_act_id = TRUE)
  
  # Toggle between selection by Act Name or Act ID
  
  output$actSelection <- renderUI({
    if (input$selectmode == 'Explore by acts') {
      selectizeInput('select_act', 'Select act to explore',
                     choices = all_acts)
    } else if (input$selectmode == 'Explore by IndianKanoon ID') {
      textInput("actIkId", label = "Enter Act/Section ID from IndianKanoon", value = "")
    }
  })
  
  # Notify if act ID is not correct
  
  observeEvent(input$refresh, {
    if (input$selectmode == 'Explore by IndianKanoon ID') {
      actIkId <- as.character(stringr::str_trim(input$actIkId))
      # print(glue::glue('this is act id -- {actIkId}'))
      # print(glue::glue('and this is the function output -- {check_act_section_id(actIkId)}'))
      if (!check_act_section_id(actIkId)) {
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "Please enter valid Act/Section ID from IndianKanoon",
          type = "error"
        )
        reactiveFlags$valid_act_id <- FALSE
      } else {
        reactiveFlags$valid_act_id <- TRUE
      }
    }
    # print(glue::glue('this is act flag -- {reactiveFlags$valid_act_id}'))
  })
  
  
  # Select sections
  
  output$section_explorer <- renderUI({
    if(input$selectmode == 'Explore by acts'){
    all_sections <- ipc_section_citations$section_name[ipc_section_citations$act_name == input$select_act]
    selectizeInput('select_section', 'Select section to explore',
                   choices = all_sections)
    }
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
        choices = court_df$court_name[!court_df$court_name %in% c('All courts')], 
        justified = FALSE, status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      )
    }
  })
  
  observeEvent(input$refresh, {
    shinyjs::show("exportCSV")
    shinyjs::show("exportJSON")
    # reactiveFlags$court_number_flag <- TRUE
    # reactiveFlags$valid_act_id <- TRUE
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
    } else {
      reactiveFlags$court_number_flag <- TRUE
    }
  })
  
  cases_by_courts <- eventReactive(input$refresh, {

    # print(glue::glue("reactiveFlags$court_number_flag -> {reactiveFlags$court_number_flag}"))
    # print(glue::glue("reactiveFlags$valid_act_id -> {reactiveFlags$valid_act_id}"))
    
    if (reactiveFlags$court_number_flag == TRUE & reactiveFlags$valid_act_id == TRUE) {
      if(input$selectmode == 'Explore by acts'){
      cited_by_section <- input$select_section
      cited_by_id <-
        ipc_section_citations$section_id[ipc_section_citations$section_name == cited_by_section]
      } else if(input$selectmode == 'Explore by IndianKanoon ID'){
        cited_by_id <- as.character(stringr::str_trim(input$actIkId))
    }
      if (input$courtid == TRUE) {
        court_list <- input$selectcourts
      } else {
        # By default (without selection of courts from the UI), stats from only Supreme court will be fetched
        court_list <- 'Supreme court'
      }
      if (input$dateid == TRUE) {
        from_date <- input$dates[[1]]
        to_date <- input$dates[[2]]
      } else {
        from_date <- NULL
        to_date <- NULL
      }
      online_flag <- ifelse(input$selectmode == 'Explore by acts',0,1)
      ik_case_summary_geography_mem(cited_by_id, court_list, from_date, to_date, online_flag)
    } else {
      NULL
    }
  })
  
  
  output$act_details <- renderUI({
    if (reactiveFlags$court_number_flag == TRUE &
        reactiveFlags$valid_act_id == TRUE) {
      act_name <- unique(cases_by_courts()[, 'Act'])
      section_name <- unique(cases_by_courts()[, 'Section'])
      wellPanel(tags$b(renderText(paste0(
        glue::glue("Act: {act_name}")
      ))),
      tags$br(),
      tags$b(renderText(paste0(
        glue::glue("Section: {section_name}")
      ))))
    }
  })

output$caseAggregator <- DT::renderDataTable({
  if(reactiveFlags$court_number_flag == TRUE & reactiveFlags$valid_act_id == TRUE){
    cases_by_courts_ui <- cases_by_courts()[, !names(cases_by_courts()) %in% c("Act","Section","FromDate","TillDate","CourtRank")]
    colour_var <- sort(cases_by_courts_ui$Judgements)[length(cases_by_courts_ui$Judgements)-1]
    cases_by_courts_ui %>% DT::datatable(escape = FALSE, class = 'row-border hover nowrap') %>%
      DT::formatStyle(
        columns = 'Court',
        backgroundColor = DT::styleEqual(c(unique(
          cases_by_courts()$Court
        )),
        c(court_df$formatcolor[court_df$court_name %in% cases_by_courts()$Court])),
        fontWeight = 'bold'
      ) %>%
      DT::formatStyle(
        columns = 'Percent',
        valueColumns = 'Judgements',
        backgroundColor = styleEqual(c(colour_var), c('#bc4b51'))
      )
    
    } else {
        cases_by_courts()
      }
  })
  

cases_by_courts_export <- eventReactive(input$refresh,{
  if(reactiveFlags$court_number_flag == TRUE & reactiveFlags$valid_act_id == TRUE) {
    cases_by_courts <- cases_by_courts() %>% data.frame()
    # browser()
    ik_link <- cases_by_courts$IK
    ik_link <- stringr::str_replace_all(ik_link,'""','"')
    ik_link <- paste0(ik_link, collapse = "")
    ik_link <- ik_link %>% read_html() %>% html_nodes('a') %>% html_attr('href') %>% unlist()
    cases_by_courts$IK <- ik_link
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
  
  observeEvent(input$about, {
    showModal(about_application())
  })
})
