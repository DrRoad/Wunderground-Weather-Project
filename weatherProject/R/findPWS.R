#' Find PWS within the cities
#'
#' @description Find PWS within the cities
#'
#' @param myKey key to access Wunderground API
#' @param nearbyCities a vector of city names formated as {state}/{city} for US cities and {country}/{city} for cities in other countries
#' @param maxPerCity a numeric scalar that sets a limit on the maximum number of PWSs returned for a city that had data returned in the wunderground call
#' @param sleeptime a numeric scalar the specifies how many seconds to wait between calls of retrieving PWS information
#'
#' @return a tibble descriping all PWS, which will include an id, latitude, longitude, etc.
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr union_all distinct n
#'
#'
#' @examples
#' findPWS('407e6151ab7de146', c("CA/San_Francisco","CA/Daly_City"),sleeptime=1)
##
## begin mcaldwel code
##
findPWS <- function(myKey, nearbyCities, maxPerCity=5, sleeptime=10) {
  if( missing(myKey) ) {
    stop("PLEASE PROVIDE YOUR KEY...")
  }
  if(
    length(maxPerCity) != 1
    | ( !is.na(maxPerCity) & !is.numeric(maxPerCity) )
  ){
    stop("maxPerCity MUST BE A SCALAR OF LENGTH 1 EVEN IF NA, & NUMERIC SCALAR IF NOT NA")
  }
  if(
    length(sleeptime) != 1
    | is.na(sleeptime)
    | !is.numeric(sleeptime)
  ){
    stop("sleeptime MUST BE A NUMERIC SCALAR OF LENGTH 1 AND NOT NA")
  }
  wuApiPrefix <- "http://api.wunderground.com/api/"
  wuFormat <- ".json"
  allPws <- NULL

  #to be created within data sets later
  citseq <- NULL



  #some calls will not result in good data
  for (i in 1:length(nearbyCities)) {
    callAddress <-
      paste0(wuApiPrefix,
             myKey,
             '/geolookup/q/',
             nearbyCities[i],
             wuFormat)
    callData <- jsonlite::fromJSON(callAddress)
    Sys.sleep(sleeptime)
    #print(callAddress)

    #check for pws element before attempting to capture
    if(
      exists("location", where = callData)
      && exists("nearby_weather_stations", where = callData$location)
      && exists("pws", where = callData$location$nearby_weather_stations)
      && exists("station", where = callData$location$nearby_weather_stations$pws)
    ) {


      callDataPws <-
        tibble::as_tibble(callData$location$nearby_weather_stations$pws$station)

      #keep the original sequence just in the event want to check back against
      callDataPws$citseq = i


      if( is.null(allPws) ){
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

  #now get a true remaining sequence on pws within each city
  allPws <- dplyr::mutate(
    dplyr::group_by(allPws, citseq)
    ,pwsNo = seq(n())
  )

  #limit if specified
  if( !is.na(maxPerCity) ){
    allPws <- base::subset(allPws,pwsNo <= maxPerCity)
  }

  allPws

##
## end mcaldwel code
##
}
