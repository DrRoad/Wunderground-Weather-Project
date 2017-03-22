#' Find nearby cities within a certain distance from a location specified by longtitude and latitude 
#'
#' @description Find nearby cities within a certain distance from a location specified by longtitude and latitude.
#'
#' @param username username to access geonames.org api 
#' @param coord a list of character to specify the location in longtitude and latitude
#' @param distance a character to specify distance in km 
#' @param maxRows a character to specify the max number of nearby cities that need to be returned 
#' 
#' @return a vector of cities 
#' 
#' @export
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' findNearbyCities('nan_stat290', longlat('San Francisco, CA'), '100', '5')

##
## start nan code 
##
findNearbyCities <-
  function(username,
           coord,
           distance,
           maxRows) {
    geoAPI <- "http://api.geonames.org/findNearbyPlaceNameJSON?"
    url <-
      paste(
        geoAPI,
        'lat=',
        coord['lat'],
        '&lng=',
        coord['long'],
        '&stype=long',
        '&radius=',
        distance,
        '&maxRows=',
        maxRows,
        '&cities=cities1000',
        '&username=',
        username,
        sep = ''
      )
    
    data = jsonlite::fromJSON(url)
    
    
    # display error message to user
    if (length(data$status) > 0) {
      stop(
        paste(
          "error code ",
          data$status$value,
          " from server: ",
          data$status$message,
          sep = ""
        )
      )
    }
    
    results = data[['geonames']]
    
    nearbyCities <- as.character()
    for (i in 1:length(unlist(results[1]))) {
      cityName <- gsub(' ', '_', results[i,]$toponymName)
      nearbyCities[i] <-
        gsub(' ',
             '_',
             ifelse(
               results[i,]$countryCode == "US",
               paste(results[i,]$adminCode1, cityName, sep = '/'),
               paste(results[i,]$countryName, cityName, sep = '/')
             ))
    }
    nearbyCities
  }
##
## end nan code 
##