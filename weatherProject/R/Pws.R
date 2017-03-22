#' An R6 Class to represent one or more Personal Weather Stations.
#' @description An R6 Class to represent one or more Personal Weather Stations
#'
#' @docType class
#' @field id A character vector containing 1 or more PWS ids.  REQUIRED.
#' @field lat           A numeric vector containing lat coordiantes for PWS ids.  REQUIRED.
#' @field lon           A numeric vector containing lat coordiantes for PWS ids.  REQUIRED.
#' @field neighborhood  A character vector containing the neighborhoods of the PWSs (1 per PWS)
#' @field city          A character vector containing the cities of the PWSs (1 per PWS)
#' @field state         A character vector containing state codes of the PWSs (1 per PWS)
#' @field country       A character vector containing country codes of the PWSs (1 per PWS)
#' @field distance_km   An integer vector of the distance in km for each PWS from city.  REQUIRED, but has a 0 default.
#' @field distance_mi   An integer vector of the distance in miles for each PWS from city
#' @section Methods:
#' \describe{
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
##3 required elements - id, lat, lon, and distance_km (for later subsetting, but will default to 0)
Pws <- R6::R6Class(
  "Pws"
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
    ,valid = function(
      id
      ,lat
      ,lon
      ,neighborhood
      ,city
      ,state
      ,country
      ,distance_km
      ,distance_mi
    ) {
      #check required fields first
      #>0
     if(
          length( id           ) == 0
        | length( lat          ) == 0
        | length( lon          ) == 0
        | length( distance_km  ) == 0
      ) {
        return(FALSE)
      }
      #cannot be na anywhere
      if(
          any( is.na( id           ) )
        | any( is.na( lat          ) )
        | any( is.na( lon          ) )
        | any( is.na( distance_km  ) )
      ) {
        return(FALSE)
      }
      #for type
      if(
          !is.character( id           )
        | !is.numeric( lat          )
        | !is.numeric( lon          )
        | !is.numeric( distance_km  )
      ) {
        return(FALSE)
      }
      #now check the others if not all nas
      if(
          ( !all( is.na(neighborhood) ) & !is.character( neighborhood ) )
        | ( !all( is.na(city        ) ) & !is.character( city         ) )
        | ( !all( is.na(state       ) ) & !is.character( state        ) )
        | ( !all( is.na(country     ) ) & !is.character( country      ) )
        | ( !all( is.na(distance_mi ) ) & !is.numeric( distance_mi    ) )
      ){
        return(FALSE)
      }
      #everything needs to match the length of id
      if(
          length( lat          ) != length( id )
        | length( lon          ) != length( id )
        | length( distance_km  ) != length( id )
      ) {
        return(FALSE)
      }

      if(
          ( !all( is.na(neighborhood) ) & length( neighborhood ) != length( id ) )
        | ( !all( is.na(city        ) ) & length( city         ) != length( id ) )
        | ( !all( is.na(state       ) ) & length( state        ) != length( id ) )
        | ( !all( is.na(country     ) ) & length( country      ) != length( id ) )
        | ( !all( is.na(distance_mi ) ) & length( distance_mi  ) != length( id ) )
      ){
        return(FALSE)
      }
      #if made it to here, cleared checks
      TRUE
    }
  )  #end private list
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
      if( !private$valid(
        id
        ,lat
        ,lon
        ,neighborhood
        ,city
        ,state
        ,country
        ,distance_km
        ,distance_mi
      )) {
         stop("BAD INPUT VALUES")
      }
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
    #removed setters, will need to just create at instantiation by combining tibbles

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
      if( all(is.na(pwsId) ) ){
        #first capture it as the entire vector, then reduce if needed
        pwsId = private$id
        #state trumps country and is assumed to be US
        if( !all( is.na( pwsState) ) & is.character( pwsState ) ){
          pwsId <- pwsId[ pwsId %in% private$id[private$state %in% pwsState] ]
        }
        else if( !all( is.na( pwsCountry) ) & is.character( pwsCountry )  ){
          pwsId <- pwsId[ pwsId %in% private$id[private$country %in% pwsCountry] ]
        }
        #now if a city, further narrow
        if( !all( is.na( pwsCity) ) & is.character( pwsCity )  ){
          pwsId <- pwsId[ pwsId %in% private$id[private$city %in% pwsCity] ]
        }

        #now if distance was provided, narrow again, assume km
        if( !all( is.na(pwsDistanceLte) ) ){
          if(
            length(pwsDistanceLte) != 1 | !is.numeric(pwsDistanceLte)
          ){
            stop("IF NOT NA, pwsDistanceLte MUST BE OF LENGTH 1 NUMERIC SCALAR")
          }
          pwsId <- pwsId[ pwsId %in% private$id[private$distance_km <= pwsDistanceLte] ]
        }
      } # end if all is.na(pwsId)
      else{
        if(
          !is.character( pwsId )
        ){
          stop("IF NOT NA, pwsId MUST BE A CHARACTER TYPE")
        }
      }

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
      if( !any( is.na(private$lat) ) & !any( is.na(private$lon) ) ){

        if( length(unique(private$id)) > 10){
          message("Note: Plot can become cluttered with over 10 PWS ids")
        }

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
  )  # end public list
)  #end class declaration

##
## end mcaldwel code
##