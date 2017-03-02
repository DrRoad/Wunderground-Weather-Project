library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("city", "city name", "London" ),
      textInput("country", "country name", "UK"),
      numericInput("distance", "nearby distance in km", 100),
      numericInput("maxcities", "max number of nearby cities", 10),
      textInput("myKey", "wunderground key", "407e6151ab7de146"),
      textInput('username', "geoname username", "nan_stat290"),
      actionButton("query", "query data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("nearby cities", br(),
          tableOutput("table")
        ),
        tabPanel("Plot", br(),
          plotOutput("plot")
        ),
        tabPanel("Info", br(),
                 verbatimTextOutput("info"),
                 verbatimTextOutput("currentCity")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  source('userFuncs.R')

   
   addr <- eventReactive(input$query, {
     paste(input$city, input$country, sep = ',')
     })
   
   coord <- reactive({
     longlat(addr())
   })
  

   nearbyCities <- eventReactive(input$query, {
     findNearbyCities(input$username, coord(), input$distance, input$maxcities)
   })
   
   output$info <- renderPrint({
     head(nearbyCities())
     
   })
   
   output$currentCity <- renderPrint({
     addr()
   })
   
   


}

shinyApp(ui, server)
