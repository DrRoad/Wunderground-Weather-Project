library(shiny)

ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    textInput("city", "city name", "San Francisco"),
    textInput("country", "country name", "US"),
    numericInput("distance", "nearby distance in km", 100),
    numericInput("maxcities", "max number of nearby cities", 3, max = 10),
    dateRangeInput("date_range", "date range",
                   start  = Sys.Date()-4,
                   end    = Sys.Date(),
                   min    = "2001-01-01",
                   max    = Sys.Date(),
                   format = "mm/dd/yy"),
    
    textInput("myKey", "wunderground key", "407e6151ab7de146"),
    textInput('username', "geoname username", "nan_stat290"),
    actionButton("query", "query data"),
    actionButton("save", "save data"),
    actionButton("savePlot", "save plot", style='padding:6px 16px 6px 17px'),
    actionButton("load", "load data", style='padding:6px 14px 6px 14px')
  ),
  mainPanel(tabsetPanel(
    tabPanel(
      "Nearby Stations",
      br(),
      tableOutput("nearbyStations"),
      tableOutput("nearbyCities"),
      verbatimTextOutput("currentCity")
    ),
    tabPanel("Weather Data", br(),
             tableOutput("table_history")),
    tabPanel("Weather Plot", br(),
             plotOutput("plot"))
  ))
))

server <- function(input, output, session) {
  source('userFuncs.R')
  
  addr <- reactive(paste(input$city, input$country, sep = ','))
  
  coord <- eventReactive(input$query, {
    longlat(addr())
  })
  
  nearbyCities <- reactive(findNearbyCities(input$username, coord(), input$distance, input$maxcities))
  
  
  nearbyStations <- reactive(findPWS(input$myKey, nearbyCities()))
  
  output$nearbyStations <- renderTable({
    results <- nearbyStations()$id
    results <- cbind("No." = seq(from = 1, to = length(results)), "PWS ID" = results)
    head(results,nrow(results))
  })
  
  output$nearbyCities <- renderTable({
    results <- nearbyCities()
    results <- cbind("No." = seq(from = 1, to = length(results)), "Nearby Cities" = results)
    head(results,nrow(results))
  })
  
  output$currentCity <- renderPrint({
    coord()
  })
  
  
  # Due to query limit per day, we limit the date range to be 5 days
  historyData <- reactive(queryHistory(input$myKey, nearbyStations(), 
                                       as.character(input$date_range[1]),  as.character(min(input$date_range[1]+4, input$date_range[2]))))
  
  output$table_history <- renderTable({
    data <- historyData()
    tmp <- gsub(' ', '',as.character(Sys.time()))
    tmp <- gsub('-', '', tmp)
    tmp <- gsub(':','', tmp)
    saveRDS(data,file=paste("./weatherData_", tmp, '.rds', sep = ''))
    head(data,nrow(data))
  })
  
  output$plot <- renderPlot({
    require(RgoogleMaps)
    allPws <- nearbyStations()
    bb <- qbbox(lat = allPws$lat, lon = allPws$lon)
    
    pwsMap <- GetMap.bbox(
      bb$lonR
      ,bb$latR
      ,destfile = "pwsMap.png"
      ,maptype="terrain"
      ,markers = allPws
      ,size=c(640,640)
    )
    
    #determine the max zoom, so that all points fit on the plot
    #zoom <- min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
    tmp <- PlotOnStaticMap(
      pwsMap
      ,lat = allPws$lat
      ,lon = allPws$lon
      ,zoom=min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
      ,cex=1.5
      ,pch=20
      ,col=c("red")
    )
    ##
    ## end mcaldwel code
  })
  
  eventReactive(input$save, {
    
    tmp <- gsub(' ', '',as.character(Sys.time()))
    tmp <- gsub('-', '', tmp)
    tmp <- gsub(':','', tmp)
    saveRDS(historyData(),file=paste("./weatherData_", tmp, '.rds', sep = ''))
    
  })
  
  
}

shinyApp(ui, server)
