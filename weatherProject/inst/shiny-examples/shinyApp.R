library(shiny)

##
## begin nan code
##

ui <- navbarPage(
  'Weather Project',
  tabPanel(
    'Nearby PWS',
    sidebarPanel(
      textInput("city", "city name", "San Francisco"),
      textInput("country", "country name", "US"),
      numericInput("distance", "nearby distance in km", 100),
      numericInput("maxcities", "max number of nearby cities", 3, max = 10),
      dateRangeInput(
        "date_range",
        "date range",
        start  = Sys.Date() - 4,
        end    = Sys.Date(),
        min    = "2001-01-01",
        max    = Sys.Date(),
        format = "mm/dd/yy"
      ),
      textInput("myKey", "wunderground key", "407e6151ab7de146"),
      textInput('username', "geoname username", "nan_stat290"),
      actionButton("query", "query"),
      actionButton("save", "save")
    ),
    mainPanel(
      tabPanel(
        "Nearby Stations",
        br(),
        tableOutput("nearbyStations"),
        tableOutput("nearbyCities"),
        verbatimTextOutput("currentCity")
      )
    )
  ),
  tabPanel(
    'Weather Data',
    sidebarPanel(
      fileInput("file1", "choose weather data file",
                accept = 'rds'),
      numericInput("distance2", "nearby distance in km", 10),
      dateRangeInput(
        "date_range2",
        "date range",
        start  = Sys.Date() - 1,
        end    = Sys.Date(),
        min    = "2001-01-01",
        max    = Sys.Date(),
        format = "mm/dd/yy"
      ),
      selectInput(
        "weatherParameter",
        "weather parameter:",
        choices = c(
          'temperature',
          'dew point',
          'humidity',
          'wind speed',
          'wind gust',
          'wind direction',
          'pressure',
          'precipitation rate',
          'precipitation total'
        )
      ),
      selectInput("unit",
                  "unit:",
                  choices = c('metric',
                              'english')),
      
      selectInput(
        "statistics",
        "statistics:",
        choices = c('mean',
                    'max',
                    'min',
                    'range')
      ),
      
      actionButton("plotWeather", "plot data", style = 'padding:6px 14px 6px 17px')
    ),
    mainPanel(
      tabPanel("Weather Plot",
               br(),
               plotOutput("plot")),
      tabPanel('History Data', br(),
               tableOutput("table_history"))
    )
  )
)

server <- function(input, output, session) {
  #source('userFuncs.R')
  
  addr <- reactive(paste(input$city, input$country, sep = ','))
  coord <- eventReactive(input$query, {longlat(addr())})
  nearbyCities <-
    eventReactive(input$query, {findNearbyCities(input$username, coord(), input$distance, input$maxcities)})
  nearbyStations <- eventReactive(input$query, {findPWS(input$myKey, nearbyCities())})
  
  historyData <- eventReactive(input$query, {
    queryHistory(
      input$myKey,
      nearbyStations(),
      as.character(input$date_range[1]),
      as.character(min(
        input$date_range[1] + 4, input$date_range[2]
      ))
    )
  })
  
  output$nearbyStations <- renderTable({
    results <- nearbyStations()
    results$No. <- seq(from = 1, to = nrow(results))
    results$PWS_ID <- results$id
    historyData()
    head(results[, c(11, 12, 6, 7, 1, 2, 3, 4, 8, 9)], nrow(results))
  })
  
  output$nearbyCities <- renderTable({
    results <- nearbyCities()
    results <-
      cbind("No." = seq(from = 1, to = length(results)),
            "Nearby Cities" = results)
    head(results, nrow(results))
  })
  
  output$currentCity <- renderPrint({
    coord()
  })
  
  observeEvent(input$save, {
    dataPWS <- nearbyStations()
    dataWeather <- historyData()
    tmp <- gsub(' ', '', as.character(Sys.time()))
    tmp <- gsub('-', '', tmp)
    tmp <- gsub(':', '', tmp)
    saveRDS(dataPWS, file = paste("./pwsData_", tmp, '.rds', sep = ''))
    saveRDS(dataWeather, file = paste("./weatherData_", tmp, '.rds', sep = ''))
    
  })
  
  # Due to query limit per day, we limit the date range to be 5 days
  output$table_history <- renderTable({
    tryCatch(
      {
        data <- historyData()
    results <- summarizeData(data, input$distance2,  as.character(input$date_range2[1]), as.character(input$date_range2[2]), input$weatherParameter, input$unit, input$statistics)
    head(data, nrow(data))
    }, 
    error = function(e)
      {NULL}
    )
    })
  
  
  output$table_history <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- readRDS(inFile$datapath)
    head(data, nrow(data))
  })
  ##
  ## end nan code
  ##
  
  
  
  ##
  ## begin mcaldwel code
  ##
  
  output$plot <- renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- readRDS(inFile$datapath)
    
    input$plotWeather
    
    pwstibble <- dplyr::distinct(data, pwsid, lat, lon, neighborhood, city, state, country, distance_km, distance_mi, .keep_all = FALSE)
    results <- isolate(summarizeData(data, input$distance2,  as.character(input$date_range2[1]), as.character(input$date_range2[2]), input$weatherParameter, input$unit, input$statistics))
    allPws <- dplyr::left_join(results, pwstibble)
    
    allPws$stat <- round(allPws$stat, digits = 2)
    
    bb <- RgoogleMaps::qbbox(lat = allPws$lat, lon = allPws$lon)
    
    
    #satellite, terrain, hybrid, and mobile
    pwsMap <- RgoogleMaps::GetMap.bbox(
      bb$lonR
      ,bb$latR
      ,destfile = "pwsMap.png"
      ,maptype="terrain"
      ,size=c(640,640)
    )
    
    
    tmpMap <- RgoogleMaps::PlotOnStaticMap(
      pwsMap
      ,lat = allPws$lat
      ,lon = allPws$lon
      ,zoom=min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
      ,cex=0.4
      ,pch=20
      ,col=c("black")
    )
    
    
    RgoogleMaps::TextOnStaticMap(
      pwsMap
      ,lat = allPws$lat
      ,lon = allPws$lon
      ,labels= allPws$stat
      ,add=TRUE
      ,cex=1.0
      ,col=c("red")
      ,offset=0.5
    ) 
    
    ##
    ## end mcaldwel code
    ##
  })
  
}
shinyApp(ui, server)
