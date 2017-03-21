#' Find PWS within the cities
#'
#' @description Find PWS within the cities
#'
#' @param myKey key to access Wunderground API
#' @param nearbyCities a vector of city names formated as {state}/{city} for US cities and {country}/{city} for cities in other countries
#'
#' @return a tibble descriping all PWS, which will include an id, latitude, longitude, etc.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr union_all distinct
#'
#'
#' @examples
#' findPWS('407e6151ab7de146', c("CA/San_Francisco","CA/Daly_City", "CA/Sausalito"))
findPWS <- function(myKey, nearbyCities) {
  if (missing(myKey)) {
    stop("please provide your key...")
  }
  wuApiPrefix <- "http://api.wunderground.com/api/"
  wuFormat <- ".json"
  allPws <- NULL

##
## begin mcaldwel code
##
  for (i in 1:length(nearbyCities)) {
    callAddress <-
      paste0(wuApiPrefix,
             myKey,
             '/geolookup/q/',
             nearbyCities[i],
             wuFormat)
    callData <- jsonlite::fromJSON(callAddress)
    Sys.sleep(10)
    print(callAddress)

    #check for pws element before attempting to capture
    if(
      exists("location", where = callData)
      && exists("nearby_weather_stations", where = callData$location)
      && exists("pws", where = callData$location$nearby_weather_stations)
      && exists("station", where = callData$location$nearby_weather_stations$pws)
    ) {
      callDataPws <-
        tibble::as_tibble(callData$location$nearby_weather_stations$pws$station)

      callDataPws$pwsNo = 1:nrow(callDataPws)
      if (is.null(allPws)) {
        allPws <- callDataPws
      }
      else{
        allPws <- dplyr::union_all(allPws, callDataPws)
      }  #end if the pws element exists in the returned data
    }
  }

  colNum = c(8:9)
  allPws[colNum] <- sapply(allPws[colNum], as.numeric)
  #reduce to distinct pws due to multiple returned in each city
  allPws <- dplyr::distinct(allPws, id, .keep_all = TRUE)

##
## end mcaldwel code
##
}
