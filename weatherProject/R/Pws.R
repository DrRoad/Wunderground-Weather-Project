#' An R6 Class to represent one or more Personal Weather Stations.
#' @description An R6 Class to represent one or more Personal Weather Stations
#'
#' @docType class
#' @field id A character vector containing 1 or more PWS ids
#' @field lat           A numeric vector containing lat coordiantes for PWS ids
#' @field lon           A numeric vector containing lat coordiantes for PWS ids
#' @field neighborhood  A character vector containing the neighborhoods of the PWSs (1 per PWS)
#' @field city          A character vector containing the cities of the PWSs (1 per PWS)
#' @field state         A character vector containing state codes of the PWSs (1 per PWS)
#' @field country       A character vector containing country codes of the PWSs (1 per PWS)
#' @field distance_km   An integer vector of the distance in km for each PWS from city
#' @field distance_mi   An integer vector of the distance in miles for each PWS from city
#' @section Methods:
#' \describe{
#'   \item{\code{setId(val)}}{This method uses \code{val} to set all of the ids in the class instance}
#'   \item{\code{setLat(val)}}{This method uses \code{val} to set all of the latitudes in the class instance}
#'   \item{\code{setLon(val)}}{This method uses \code{val} to set all of the longitudes in the class instance}
#'   \item{\code{setCity(val)}}{This method uses \code{val} to set all of the cities in the class instance}
#'   \item{\code{setNeighborhood(val)}}{This method uses \code{val} to set all of the neighborhoods in the class instance}
#'   \item{\code{setState(val)}}{This method uses \code{val} to set all of the states in the class instance}
#'   \item{\code{setCountry(val)}}{This method uses \code{val} to set all of the countries in the class instance}
#'   \item{\code{setDistance_km(val)}}{This method uses \code{val} to set all of the distances in kms in the class instance}
#'   \item{\code{setDistance_mi(val)}}{This method uses \code{val} to set all of the disances in miles in the class instance}
#'   \item{\code{getId(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the ids in the class instance}
#'   \item{\code{getLat(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the latitudes in the class instance}
#'   \item{\code{getLon(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the longitudes in the class instance}
#'   \item{\code{getCity(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the cities in the class instance}
#'   \item{\code{getNeighborhood(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the neighborhoods in the class instance}
#'   \item{\code{getState(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the states in the class instance}
#'   \item{\code{getCountry(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the countries in the class instance}
#'   \item{\code{getDistance_km(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the distances in kms in the class instance}
#'   \item{\code{getDistance_mi(pwsId)}}{This method uses \code{pwsId} to determine if to get some or all (if NA) of the distances in miles in the class instance}
#'   \item{\code{getTibble(pwsId,pwsCity,pwsState,pwsDistanceLte)}}{This method uses \code{pwsId} as  priority 1 by itself/only, \code{state} priority 2 (assumed in US) and/or \code{country} priority 3, then \code{city}, and \code{pwsDistanceLte} in km to return a tibble containing pws data}
#'   \item{\code{print()}}{This method prints the class in the manner or a tibble}
#'   \item{\code{plot(plottype)}}{This method \code{plottype} to determine whether p/pins or t/text (ids) to produce a plot of weather stations on a map using RgoogleMaps package}
#' }
#' @importFrom R6 R6Class
#' @importFrom RgoogleMaps qbbox GetMap.bbox PlotOnStaticMap TextOnStaticMap MaxZoom
#' @importFrom tibble tibble
#' @examples
#' data(pwsData)
#' testr6 <- Pws$new(
#'   id               = pwsData$id
#'   ,lat             = pwsData$lat
#'   ,lon             = pwsData$lon
#'   ,neighborhood    = pwsData$neighborhood
#'   ,city            = pwsData$city
#'   ,state           = pwsData$state
#'   ,country         = pwsData$country
#'   ,distance_km     = pwsData$distance_km
#'   ,distance_mi     = pwsData$distance_mi
#' )
#' testr6$print()
#' testr6$plot()
#' @export

##
## begin mcaldwel code
##
##note - examples from https://github.com/wch/R6/issues/3 were used to document the class
Pws <- R6::R6Class(
  "Pws"
  ,public = list(
    initialize = function(
      id
      ,lat = NA
      ,lon = NA
      ,neighborhood = NA
      ,city = NA
      ,state = NA
      ,country = NA
      ,distance_km = 0
      ,distance_mi = 0
    ) {
      private$id <- id
      private$lat <- lat
      private$lon <- lon
      private$neighborhood <- neighborhood
      private$city <- city
      private$state <- state
      private$country <- country
      private$distance_km <- distance_km
      private$distance_mi <- distance_mi
    }
    #at this point functions that would set the entire column, need to add
    #parameters to limit to specific IDs
    ,setId             = function(val) { private$id            <- val }
    ,setLat            = function(val) { private$lat           <- val }
    ,setLon            = function(val) { private$lon           <- val }
    ,setNeighborhood   = function(val) { private$neighborhood  <- val }
    ,setCity           = function(val) { private$city          <- val }
    ,setState          = function(val) { private$state         <- val }
    ,setCountry        = function(val) { private$country       <- val }
    ,setDistance_km    = function(val) { private$distance_km   <- val }
    ,setDistance_mi    = function(val) { private$distance_mi   <- val }

    #either gets the entire vector specified, or only retrieves those with the id passed
    ,getId = function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$id }
      else{ private$id[private$id %in% pwsId] }
    }
    ,getLat = function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$lat }
      else{ private$lat[private$id %in% pwsId] }
    }

    ,getLon = function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$lon }
      else{ private$lon[private$id %in% pwsId] }
    }

    ,getNeighborhood= function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$neighborhood }
      else{ private$neighborhood[private$id %in% pwsId] }
    }

    ,getCity= function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$city }
      else{ private$city[private$id %in% pwsId] }
    }

    ,getState= function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$state }
      else{ private$state[private$id %in% pwsId] }
    }

    ,getCountry = function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$country }
      else{ private$country[private$id %in% pwsId] }
    }

    ,getDistance_km= function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$distance_km }
      else{ private$distance_km[private$id %in% pwsId] }
    }

    ,getDistance_mi= function(pwsId = NA) {
      if( all(is.na(pwsId)) ){ private$distance_mi }
      else{ private$distance_mi[private$id %in% pwsId] }
    }

    ,getTibble = function(
      pwsId = NA
      ,pwsCity = NA
      ,pwsState = NA
      ,pwsCountry = NA
      ,pwsDistanceLte = NA  #assuming km - needs to be a single value
    ) {
      #only do for no pwsId, as that trumps everything else
      if( is.na(pwsId) ){
        #first capture it as the entire vector, then reduce if needed
        pwsId = private$id
        #state trumps country and is assumed to be US
        if( !is.na( pwsState) ){
          pwsId <- pwsId[ pwsId %in% private$id[private$state %in% pwsState] ]
        }
        else if( !is.na( pwsCountry) ){
          pwsId <- pwsId[ pwsId %in% private$id[private$country %in% pwsCountry] ]
        }
        #now if a city, further narrow
        if( !is.na( pwsCity) ){
          pwsId <- pwsId[ pwsId %in% private$id[private$city %in% pwsCity] ]
        }

        #now if distance was provided, narrow again, assume km
        if( !is.na(pwsDistanceLte) ){
          pwsId <- pwsId[ pwsId %in% private$id[private$distance_km <= pwsDistanceLte] ]
        }
      } # end if is.na(pwsId)

      tibble::tibble(
        id               = self$getId(pwsId)
        ,lat             = self$getLat(pwsId)
        ,lon             = self$getLon(pwsId)
        ,neighborhood    = self$getNeighborhood(pwsId)
        ,city            = self$getCity(pwsId)
        ,state           = self$getState(pwsId)
        ,country         = self$getCountry(pwsId)
        ,distance_km     = self$getDistance_km(pwsId)
        ,distance_mi     = self$getDistance_mi(pwsId)
      )
    }
    ,print = function() {
      print( self$getTibble() )
    }
    ,plot = function(plottype="t") {
      if( !( plottype %in% c("t","p") ) ){
        stop("INVALID PLOT TYPE PASSED TO Pws class plot function")
      }

      #can only plot with a lat and long
      if( !is.na(private$lat) && !is.na(private$lon) ){

        bb <- RgoogleMaps::qbbox(lat = private$lat, lon = private$lon)


        #satellite, terrain, hybrid, and mobile are maptypes
        #zooming did not seem to be doing anything good, so took it out
        if(plottype=="t"){
          pwsMap <- RgoogleMaps::GetMap.bbox(
            bb$lonR
            ,bb$latR
            ,destfile = "pwsMap.png"
            ,maptype="terrain"
            ,size=c(640,640)
          )

          tmpMap <- RgoogleMaps::PlotOnStaticMap(
            pwsMap
            ,lat = private$lat
            ,lon = private$lon
            #,zoom=min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
            ,cex=0
            ,pch=20
            ,col=c("black")
          )


          RgoogleMaps::TextOnStaticMap(
            pwsMap
            ,lat = private$lat
            ,lon = private$lon
            ,labels=private$id
            ,add=TRUE
            ,cex=0.8
            ,col=c("red")
          )
        }  #end if plottype == t
        else if(plottype == "p"){
          pwsMap <- RgoogleMaps::GetMap.bbox(
            bb$lonR
            ,bb$latR
            ,destfile = "pwsMap.png"
            ,maptype="terrain"
            ,markers = self$getTibble()
            ,size=c(640,640)
          )

          RgoogleMaps::PlotOnStaticMap(
            pwsMap
            ,lat = private$lat
            ,lon = private$lon
            #,zoom=min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
            ,cex=0
            ,pch=20
            ,col=c("red")
          )
        }  #end if plottype == p
      }  #end if have lat and lon for plotting
    }  #end plot function
  )
  ,private = list(
    id              = character(0)
    ,lat            = numeric(0)
    ,lon            = numeric(0)
    ,neighborhood   = character(0)
    ,city           = character(0)
    ,state          = character(0)
    ,country        = character(0)
    ,distance_km    = integer(0)
    ,distance_mi    = integer(0)
  )
)

##
## end mcaldwel code
##