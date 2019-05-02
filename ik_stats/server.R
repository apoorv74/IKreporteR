library(shiny)
library(readr)

shinyServer(function(input, output) {
  
  observeEvent(input$refresh,{
    cat("")
  })
  
  cases_by_courts <- eventReactive(input$refresh, {
    cited_by_section <- input$select_section
    cited_by_id <- ipc_section_citations$section_id[ipc_section_citations$section_name == cited_by_section]
    court_list <- input$selectcourts
    ik_case_summary_geography(cited_by_id, court_list)
  })
  
  output$caseAggregator <- renderDataTable({
    cases_by_courts()
 })
})
