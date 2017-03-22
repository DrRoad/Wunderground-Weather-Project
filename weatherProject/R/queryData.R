#' Query weather data for individual PWS
#'
#' @description Query weather data for individual PWS
#'
#' @param myKey key for accessing Wunderground API 
#' @param pwsid character pws ID
#' @param qTime a specific date in history for query 
#' 
#' @return a tibble containing the data for that specified pws from the specified date 
#' 
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#'
#' @examples
#' queryData("407e6151ab7de146", "KCASANFR699", Sys.Date()-1)

##
## start nan code 
##
# function to extract weather data for individual PWS
queryData <- function(myKey, pwsid, qTime) {
  wuApiPrefix <- "http://api.wunderground.com/api/"
  wuFormat <- ".json"
  
  wuURL <- paste(
    wuApiPrefix,
    myKey,
    '/history_',
    gsub("-", "", qTime),
    '/q/',
    paste0("pws:", pwsid),
    wuFormat,
    sep = ''
  )
  
  data <- jsonlite::fromJSON(wuURL)
  print(wuURL)
  
  weatherData <- data$history$observations
  
  year  <- weatherData$date$year
  mon <- weatherData$date$mon
  mday <- weatherData$date$mday
  hour <- weatherData$date$hour
  min <- weatherData$date$min
  
  date_time <- paste(year, mon, mday, hour, min, sep = '-')
  weatherData <-
    cbind(date_time, weatherData, stringsAsFactors = FALSE)
  
  utc_year  <- weatherData$utcdate$year
  utc_mon <- weatherData$utcdate$mon
  utc_mday <- weatherData$utcdate$mday
  utc_hour <- weatherData$utcdate$hour
  utc_min <- weatherData$utcdate$min
  
  utc_date_time <-
    paste(utc_year, utc_mon, utc_mday, utc_hour, utc_min, sep = '-')
  weatherData <-
    cbind(utc_date_time, weatherData, stringsAsFactors = FALSE)
  
  addr_name <- rep(pwsid, nrow(weatherData))
  weatherData <-
    cbind(pwsid, weatherData, stringsAsFactors = FALSE)
  
  weatherData$date <- NULL
  weatherData$utcdate <- NULL
  weatherData$metar <- NULL
  weatherData$solarradiation <- NULL
  weatherData$UV <- NULL
  weatherData$softwaretype <- NULL
  
  
  colNum = c(4:12, 15, 16, 21:24)
  weatherData[colNum] <- sapply(weatherData[colNum], as.numeric)
  
  tibble::as_tibble(weatherData)
  
}
##
## end nan code 
##