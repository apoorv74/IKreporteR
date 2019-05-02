library(shinyWidgets)
library(shiny)

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
        dateRangeInput(
            inputId = "dates",
            label = "Choose start and end dates",
            start = Sys.Date() - 7,
            end = Sys.Date(),
            min = Sys.Date() - 45,
            max = Sys.Date(),
            width = "300px"
        )
    )),
    fluidRow(column(width=2, 
                    materialSwitch(inputId = "courtid", label = "Explore data from all courts ?", status = "danger")),
             column(
        width = 4,
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
    )),
    fluidRow(column(
        width = 2,
        actionButton(
            inputId = "refresh",
            label = "Go",
            style = "height: 30px; width:100px;padding: 0px 12px;"
        )
    )),
    tags$br(),
    fluidRow(column(
        width=10,
        dataTableOutput(outputId = 'caseAggregator')
    ))
)