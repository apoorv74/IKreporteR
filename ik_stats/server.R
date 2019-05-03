library(shiny)

shinyServer(function(input, output) {
  
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
        inputId = "selectcourts", label = "Select courts to explore:", 
        choices = c("supremecourt","allahabad","andhra",
                    "bombay","chattisgarh","chennai","delhi",
                    "gauhati","gujarat","himachal_pradesh","jammu",
                    "jharkhand","karnataka","kerala","kolkata",
                    "lucknow","madhyapradesh","orissa","patna",
                    "punjab","rajasthan","sikkim","uttaranchal",
                    "jodhpur","srinagar","meghalaya","tripura"), 
        justified = FALSE, status = "primary",
        checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
      )
    }
  })
  
  
  # observeEvent(input$refresh,{
  #   cat("")
  # })
  
  cases_by_courts <- eventReactive(input$refresh, {
    cited_by_section <- input$select_section
    cited_by_id <- ipc_section_citations$section_id[ipc_section_citations$section_name == cited_by_section]
    court_list <- input$selectcourts
    if(input$dateid == TRUE){
      from_date <- input$dates[[1]]
      to_date <- input$dates[[2]]
    } else {
      from_date <- NULL
      to_date <- NULL
    }
    ik_case_summary_geography(cited_by_id, court_list, from_date, to_date)
  })
  
  output$caseAggregator <- renderDataTable({
    cases_by_courts()
 })
})
