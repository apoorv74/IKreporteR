about_application <- function() {
  modalDialog(
    title = "About the application",
    fluidPage(
      fluidRow(column(width = 2,
                      tags$b("Objective")),
               column(
                 width = 10,
                 tags$p(
                   "To view the total count of cases as per Central Acts and Sections by aggregating data from",
                   tags$a(href = 'https://indiankanoon.org', 'IndianKanoon')
                 )
               )),
      tags$br(),
      fluidRow(
        column(width = 2,
               tags$b("Motivation")),
        column(
          width = 10,
          "It all started when we wanted to view the trends for cases listed under a particular act/section. IndianKanoon is a great resource to conduct quick research and it gives you advanced search capabilities to view judgements across geographies, but we wanted a quicker way to get aggregated numbers across all courts. This platform helps the user see these patterns across years and lets them export results in a convenient machine-readable format (csv/json)"
        )
      ),
      # fluidRow(column(width = 1,
      #                 tags$b("Process")),
      #          column(width=11,
      #                 tags$ul(
      #                   tags$li("Select an act and section from the UI or enter the ID directly from IndianKanoon"),
      #                   tags$li("Select dates to compare trends across certain dates/years, by default aggregation is done for all cases on the platform"),
      #                   tags$li("Select courts to compare, a max of 5 courts can be compared at once, by default only Supreme Court cases are aggregated"),
      #                   tags$li("Explore!!")
      #                 ))),c("NA")c("NA")
      tags$br(),
      fluidRow(column(width = 2,
                      tags$b("Source Code")),
               column(width = 10,
                      tags$p('View on ',
                        tags$a(href = "https://github.com/apoorv74/IKreporteR/issues", shiny::icon('github'))
                      ))),
      tags$br(),
      fluidRow(column(width = 2,
                      tags$b("Documentation")),
               column(width = 10,
                      tags$p('How to use the application - ',
                        tags$a(href = "https://github.com/apoorv74/IKreporteR/wiki/Introduction", shiny::icon('file'))
                      ))),
      tags$br(),
      fluidRow(column(width = 2,
                      tags$b("Packages")),
               column(
                 width = 10,
                 tags$p(
                   "This tools stands on top of some amazing work done by developers, and we really appreciate them for opening up the source code. Kudos people :)",
                   tags$ul(
                     tags$li(tags$a("shiny", href = "https://shiny.rstudio.com/")),
                     tags$li(
                       tags$a("shinyWidgets", href = "https://github.com/dreamRs/shinyWidgets")
                     ),
                     tags$li(
                       tags$a("shinyjs", href = "https://github.com/daattali/shinyjs")
                     ),
                     tags$li(tags$a("DT", href = "https://rstudio.github.io/DT/")),
                     tags$li(
                       tags$a("shinycssloaders", href = "https://github.com/andrewsali/shinycssloaders")
                     ),
                     tags$li(
                       tags$a("rvest", href = "https://blog.rstudio.com/2014/11/24/rvest-easy-web-scraping-with-r/")
                     )
                   )
                 )
               )),
      tags$br(),
      fluidRow(
        column(width = 2,
               tags$b("Author[s]")),
        column(
          width = 10,
          "Made with",
          HTML("&#x2764;&#xFE0F;"),
          "+",
          HTML("\u2615\uFE0F"),
          "by",
          tags$a(href = 'https://twitter.com/apo_orv/', 'Apoorv')
        )
      ),
      tags$br(),
      fluidRow(column(width = 4,
                      tags$b(
                        "Acknowledgments"
                      ))),
      tags$hr(),
      fluidRow(column(
        width = 2,
        tags$a(
          href = 'https://indiankanoon.org',
          tags$img(src = "indiankanoon.png", style = "width: 100px; float:right; margin-right: 10px;")
        )
      ),
      column(
        width = 10,
        tags$p(
          "This package [and a lot of important research] was not possible without the work of",
          tags$a(href = "https://indiankanoon.org", "IndianKanoon."),
          " We thank ",
          tags$a(href = "https://twitter.com/sushantsinha", "Sushant Sinha"),
          "and team for all their work on this irreplaceable platform."
          
        )
      )),
      tags$br(),
      tags$hr(),
      fluidRow(column(
        width = 2,
        tags$a(
          href = 'http://creativecommons.org/licenses/by-nc/4.0/',
          tags$img(src = "https://i.creativecommons.org/l/by-nc/4.0/88x31.png", style = "border-width:0;width: 100px; float:right; margin-right: 10px;")
        )
      ),
      column(
        width = 10,
        tags$p(
          "This work is licensed under a",
          tags$a(
            href = 'http://creativecommons.org/licenses/by-nc/4.0/',
            "Creative Commons Attribution-NonCommercial 4.0 International License"
          )
        )
      ))
      
    ),
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Close")
  )
}
