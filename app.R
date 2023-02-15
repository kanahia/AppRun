library("leaflet")
library("ggplot2")
library("plotly")
library("dplyr")
library("XML")


ui <-
  shiny.fluent::fluentPage(
    # tags$style(".card { padding: 28px; margin-bottom: 28px; }"),
    # analysis_page
    shinydashboard::box(headerPanel("Hello Shiny!")),
    sidebarLayout(
      sidebarPanel(sliderInput("obs", "Number of observations:", min = 0, max = 1000, value = 500)),
      mainPanel(plotOutput("distPlot", height = "2000px"))
    )
)


server <- 
  shinyServer(function(input, output, session) {
    
  })

shinyApp(ui, server)