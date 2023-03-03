library("shiny")
library("shinyWidgets")

s <-
  shiny::tags$nav(
    id= "my_social",
    shiny::tags$li(class="dropdown",
                   tags$a(href="https://www.linkedin.com/in/kanahia/", 
                          shiny::icon("linkedin"), 
                          "LinkedIn", target="_blank")
    ),
    shiny::tags$li(class="dropdown",
                   tags$a(href="https://github.com/kanahia", 
                          shiny::icon("github"), 
                          "GitHub", target="_blank")
                   )
    )

#thematic::thematic_shiny()
ui <-
  fluidPage(
    #theme = bslib::bs_theme(bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198"),
    includeCSS("www/my_style2.css"),
    titlePanel(title =
                 div(
                   div(class = "div_my_logo",
                       img(src = "AppRun_logo.png", 
                           id = "my_logo")
                       )
                  )
               ),
    navbarPage(title = "",
               tabPanel(title = "Plot1",
                        shiny::fluidRow(
                          daterangepicker::daterangepicker(
                            inputId = "daterange",
                            label = "Pick a Date",
                            start = Sys.Date() - 30, 
                            end = Sys.Date(),
                            style = "width:100%; border-radius:4px;",
                            icon = icon("calendar"),
                            options = 
                              daterangepicker::daterangepickerOptions(
                                alwaysShowCalendars = TRUE)
                            ),
                          verbatimTextOutput("print"),
                          actionButton("act", "Update Daterangepicker")
                        ),
                        shiny::wellPanel(
                          shiny::fluidRow(
                            style = "height:500px;",
                            shiny::column(
                              width = 7,
                              align="center",
                              plotly::plotlyOutput("plot", height = "500px")
                            ),
                            shiny::column(
                              width = 5,
                              align="center",
                              leaflet::leafletOutput("map", height = "500px")
                              )
                            )
                          ),
                          wellPanel(
                            fluidRow(
                              shiny::column(
                                width = 4,
                                align = "center",
                                plotly::plotlyOutput("zones")
                                )
                              )
                            )
                        ),
               tabset_summary,#tabPanel("Plot2"),
               tabPanel("Plot3")
               
               )
    )



server <- 
  function(input, output) {
    
    my_run <- fit_files[[1]]
    
    output$plot <- plotly::renderPlotly({
      plotly::subplot(style(plotly_HeartRate(my_run, black = FALSE, opacity = 0.7), showlegend = FALSE),
                      style(plotly_speed(my_run, black = FALSE, opacity = 0.7), showlegend = FALSE),
                      plotly_cadence(my_run, black = FALSE),
                      nrows = 3,
                      shareX = TRUE) %>%
        plotly::layout(hovermode = 'x')
    })
    
    output$map <- leaflet::renderLeaflet({
      plot_map(my_run)
    })
    
    output$zones <- plotly::renderPlotly({
      plotly_zones(my_run, black = FALSE)
    })
  }

shinyApp(ui, server)

