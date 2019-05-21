library(shinyWidgets)
library(shiny)
library(shinycssloaders)
library(D3TableFilter)




fluidPage(
    shinyjs::useShinyjs(), 
    tags$head(
        tags$link(href = "styles.css", rel = "stylesheet", type = "text/css")
    ),
    tags$div(
        class = "container",
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
            tags$br()
            
            # ,tags$span(icon("balance-scale"), class = "main-icon")
        )
    )),
    fluidRow(column(
        width = 4,
        radioGroupButtons(
            inputId = "selectmode",
            label = "Explore judgements",
            choices = c("Explore by acts", "Explore by IndianKanoon ID"),
            status = "primary"
        )
    )),
    fluidRow(
        column(
            width = 4,
            uiOutput('actSelection')
        ),
        column(
            width = 4,
            uiOutput('section_explorer')
        )
    ),
    fluidRow(
        column(width=4,
               materialSwitch(inputId = "dateid", label = "Filter cases by date", status = "primary")),
        # column(width=2),
        column(
        width = 4,
        uiOutput('selectDate')
    )),
    fluidRow(column(width=4, 
                    materialSwitch(inputId = "courtid", label = "Toggle to select courts (Default - Supreme Court)", status = "primary")),
             # column(width=2),
             column(
        width = 6,
        uiOutput('selectCourts')
    )),
    fluidRow(column(
        width = 4,
        actionBttn(
            inputId = "refresh",
            label = "Explore",
            style = "unite", 
            color = "primary"
        )
    )),
    tags$br(),
    fluidRow(column(
      width = 6,
      uiOutput("act_details")
    )),
    fluidRow(column(
        width=10,
        withSpinner(DT::dataTableOutput(outputId = 'caseAggregator'),type = 8)
    )),
    
    tags$hr(),
    
    # Export options
    fluidRow(
        column(width = 2),
        downloadButton('exportCSV',"Export as CSV"),
        downloadButton('exportJSON',"Export as JSON")
    ),
    tags$br(),
    tags$br(),
    actionLink(
      inputId = "about", label = "About the application", icon = NULL,
      style = "color: #112446; padding: 5px; line-height:25px;", class = "pull-right"
    )
))



