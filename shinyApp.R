library(shiny)

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    textInput("city", "city name", "London"),
    textInput("country", "country name", "UK"),
    numericInput("distance", "nearby distance in km", 100),
    numericInput("maxcities", "max number of nearby cities", 5),
    dateRangeInput("date_range", "date range",
                   start  = "2017-01-01",
                   end    = Sys.Date(),
                   min    = "2001-01-01",
                   max    = Sys.Date(),
                   format = "mm/dd/yy"),
    
    textInput("myKey", "wunderground key", "407e6151ab7de146"),
    textInput('username', "geoname username", "nan_stat290"),
    actionButton("query", "query data"),
    actionButton("save", "save data")
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "Data",
      br(),
      tableOutput("nearbyCities"),
      verbatimTextOutput("currentCity")
    ),
    tabPanel("Plot", br(),
             plotOutput("plot")),
    tabPanel("Table", br(),
             tableOutput("table"))
  ))
))

server <- function(input, output, session) {
  source('userFuncs.R')
  
  addr <- reactive(paste(input$city, input$country, sep = ','))
  
  coord <- eventReactive(input$query, {
    longlat(addr())
  })
  
  nearbyCities <- eventReactive(input$query, {
    findNearbyCities(input$username, coord(), input$distance, input$maxcities)
  })
  
  output$nearbyCities <- renderTable({
    results <- nearbyCities()
    results <- cbind("No." = seq(from = 1, to = length(results)), "Nearby Cities" = results)
    head(results,10)
  })
  
  output$currentCity <- renderPrint({
    coord()
    
  })
  
  
  df <- reactive(queryData(input$myKey, nearbyCities(), as.character(input$date_range[1])))
  
  output$table <- renderTable({
    head(df(),48)
  })
  
  
  
  
  eventReactive(input$save, {
    save(df(),file="weatherData.Rda")
    
  })
  
  
  
  
  
}

shinyApp(ui, server)
