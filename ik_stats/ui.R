library(shinyWidgets)
library(shiny)
library(shinycssloaders)

fluidPage(
    tags$head(
        tags$link(href = "styles.css", rel = "stylesheet", type = "text/css")
    ),
    fluidRow(column(
        width = 10,
        tags$div(
            class = "title",
            tags$br(),
            tags$h1("Case law aggregation as per Central acts", class = "titre"),
            tags$h2(
                "Judgements from all high courts and the Supreme Court (via IndianKanoon)",
                class = "soustitre"
            ),
            tags$br(),
            
            tags$span(icon("balance-scale"), class = "main-icon")
        )
    )),
    fluidRow(
        column(
            width = 2,
            selectizeInput('select_act', 'Select act to explore',
                           choices = all_acts)
        ),
        column(
            width = 2,
            selectizeInput('select_section', 'Select section to explore',
                           choices = all_sections)
        )
    ),
    fluidRow(
        column(width=2,
               materialSwitch(inputId = "dateid", label = "Filter cases by date", status = "danger")),
        column(
        width = 2,
        uiOutput('selectDate')
    )),
    fluidRow(column(width=2, 
                    materialSwitch(inputId = "courtid", label = "Toggle to select courts (Default - All courts)", status = "danger")),
             column(
        width = 4,
        uiOutput('selectCourts')
    )),
    fluidRow(column(
        width = 4,
        actionBttn(
            inputId = "refresh",
            label = "Go",
            style = "unite", 
            color = "danger"
        )
    )),
    tags$br(),
    fluidRow(column(
        width=10,
        withSpinner(dataTableOutput(outputId = 'caseAggregator'),type = 8) 
    ))
)