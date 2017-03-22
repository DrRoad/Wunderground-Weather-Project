#' Query weather history data from the specified PWS within the specified date range
#'
#' @description Query weather history data from the specified PWS within the specified date range
#'
#' @param myKey key for accessing Wunderground API
#' @param nearbyStations a tibble describing PWS
#' @param startDate a starting date
#' @param endDate an end date
#'
#' @return a tibble containing weather data for all the pws within the specified date range
#'
#' @export
#' @importFrom dplyr filter
#' @importFrom plyr rbind.fill
#'

##
## start nan code 
##
queryHistory <-
  function(myKey, nearbyStations, startDate, endDate) {
    # Due to call limit per day, we limit the max num of pws per city
    #nearbyStations <- dplyr::filter(nearbyStations, pwsNo <= 5)
    combinedData <- as.data.frame(NULL)
    duration  = as.numeric(as.Date(endDate) - as.Date(startDate) + 1)


    for (i in 1:nrow(nearbyStations)) {
      for (j in 1:duration) {
        weatherData <-
          queryData(myKey, nearbyStations$id[i], as.character(as.Date(startDate) + j - 1))
        Sys.sleep(10)

        # combine pws data frame with weather data
        weatherData$neighborhood <- nearbyStations$neighborhood[i]
        weatherData$city <- nearbyStations$city[i]
        weatherData$state <- nearbyStations$state[i]
        weatherData$country <- nearbyStations$country[i]
        weatherData$lat <- nearbyStations$lat[i]
        weatherData$lon <- nearbyStations$lon[i]
        weatherData$distance_km <- nearbyStations$distance_km[i]
        weatherData$distance_mi <- nearbyStations$distance_mi[i]

      }

      combinedData <-
        plyr::rbind.fill(combinedData, weatherData)

    }

    combinedData
  }

##
## end nan code 
##
