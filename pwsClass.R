
##
## begin mcaldwel code
##
Pws <- R6Class(
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
      ,pwsDistanceLte = NA  #assuming km - needs to be a singel value
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
        
        #now further narrow down if a city was provided
      } # end if is.na(pwsId)
      
      tibble(
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
      #options here for printing will follow the tibble options
      #options(tibble.print_max = n, tibble.print_min = m)
      #if there are more than n rows, print only the first m rows. Use options(tibble.print_max = Inf) to always show all rows.
      #options(tibble.width = Inf)
      #will always print all columns, regardless of the width of the screen.
    }
    ,plot = function() {
      # data <- private$data
      # ggplot2::qplot(data$date, data$price, data = data, geom = "line")
      
      #can only plot with a lat and long
      if( !is.na(private$lat) && !is.na(private$lon) ){
        
        bb <- RgoogleMaps::qbbox(lat = private$lat, lon = private$lon)
        
        
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
          ,lat = private$lat
          ,lon = private$lon
          ,zoom=min(MaxZoom(latrange=bb$latR,lonrange=bb$lonR))
          ,cex=0.4
          ,pch=20
          ,col=c("black")
        )
        
        
        RgoogleMaps::TextOnStaticMap(
          pwsMap
          ,lat = private$lat
          ,lon = private$lon
          ,labels=private$id
          ,add=TRUE
          ,cex=0.6
          ,col=c("red")
          ,offset=0.5
        ) 
        
      }  #end if have lat and lon for plotting
    }
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