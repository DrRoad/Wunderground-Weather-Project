#' Plot weather data for visualization 
#'
#' @description Plot weather data for visualization 
#'
#' @param wtbl a tibble of weather data  
#' @param plottype specify plot type 
#' @param wvar weather parameter of interest 
#' @param aggtype aggregate type
#' @param startDate start date
#' @param endDate end date 
#' 
#' @return NULL
#' 
#' @export
#' @importFrom dplyr subset group_by mutate summarize
#' @importFrom RgoogleMaps qbbox GetMap.bbox PlotOnStaticMap TextOnStaticMap
#' @importFrom ggplot2 ggplot geom_line xlab ylab theme
#'
#' @examples
#' data(weatherData)
#' queryData(weatherData, plottype = 'm')
plotWeather <- function(
  wtbl
  ,plottype="m"
  ,wvar="tempi"
  ,aggtype="mean"
  ,startDate = NA
  ,endDate = NA
){
  #check to make sure parameters make sense and are accounted for
  if( !( wvar %in% c("tempm","tempi","dewptm","dewpti","pressurem","pressurei","hum") ) ){
    stop("INVALID WEATHER VARIABLE PASSED TO plotWeather")
  }
  if( !( aggtype %in% c("mean","max","min","range") ) ){
    stop("INVALID AGGREGATION TYPE PASSED TO plotWeather")
  }
  if( !( plottype %in% c("m","g") ) ){
    stop("INVALID PLOT TYPE PASSED TO plotWeather")
  }

  #get a working copy
  mywtbl <- wtbl
  #reduce down dates if needed
  if( !is.na(startDate) ){
    tryCatch(
      as.Date(startDate)
      ,error=function(e){ stop("INVALID START DATE FORMAT, NEED YYYY-MM-DD") }
    )
    mywtbl <- dplyr::subset( mywtbl, utc_date_time >= as.Date( startDate ) )
  }
  if( !is.na(endDate) ){
    tryCatch(
      as.Date(endDate)
      ,error=function(e){ stop("INVALID END DATE FORMAT, NEED YYYY-MM-DD") }
    )
    mywtbl <- dplyr::subset( mywtbl, date <= as.Date( endDate ) )
  }

  #combine the variable with the aggregation type to later use for var names
  wvaragg <- paste0(wvar,aggtype)

  #need a check for integrity of the data set columns
  #weather variable summary
  #if mapping, do not need date included
  if( plottype == "m" ){
    wvarSum <- dplyr::group_by(mywtbl,pwsid,lat,lon)
  }
  else if(plottype == "g"){
    wvarSum <- dplyr::group_by(mywtbl,pwsid,utc_date_time)
  }


  wvarSum <-  dplyr::mutate(
    dplyr::summarize(
      wvarSum
      ,tempmmean = mean(tempm, na.rm=TRUE)
      ,tempmmin = min(tempm, na.rm=TRUE)
      ,tempmmax = max(tempm, na.rm=TRUE)

      ,tempimean = mean(tempi, na.rm=TRUE)
      ,tempimin = min(tempi, na.rm=TRUE)
      ,tempimax = max(tempi, na.rm=TRUE)

      ,dewptmmean = mean(dewptm, na.rm=TRUE)
      ,dewptmmin = min(dewptm, na.rm=TRUE)
      ,dewptmmax = max(dewptm, na.rm=TRUE)

      ,dewptimean = mean(dewpti, na.rm=TRUE)
      ,dewptimin = min(dewpti, na.rm=TRUE)
      ,dewptimax = max(dewpti, na.rm=TRUE)

      ,pressuremmean = mean(pressurem, na.rm=TRUE)
      ,pressuremmin = min(pressurem, na.rm=TRUE)
      ,pressuremmax = max(pressurem, na.rm=TRUE)

      ,pressureimean = mean(pressurei, na.rm=TRUE)
      ,pressureimin = min(pressurei, na.rm=TRUE)
      ,pressureimax = max(pressurei, na.rm=TRUE)

      ,hummean = mean(hum, na.rm=TRUE)
      ,hummin = min(hum, na.rm=TRUE)
      ,hummax = max(hum, na.rm=TRUE)
    )
    ,tempmrange = tempmmax - tempmmin
    ,tempirange = tempimax - tempimin
    ,dewptmrange = dewptmmax - dewptmmin
    ,dewptirange = dewptimax - dewptimin
    ,pressuremrange = pressuremmax - pressuremmin
    ,pressureirange = pressureimax - pressureimin
    ,humrange = hummax - hummin
  )
  #now need to cut down lat/lon/labels based on data available, i.e. exclude missings
  #in order to get it to take a character parameter, additional complexity
  wvarFilt <- as.vector(
    !is.na( unlist( wvarSum[,wvaragg] ) )
    & !is.nan( unlist( wvarSum[,wvaragg] ) )
  )

  wvarSum <- wvarSum[wvarFilt,]


  #map specific
  if( plottype == "m" ){
    #define a box as shown in the package
    bb <- RgoogleMaps::qbbox(lat = wvarSum$lat, lon = wvarSum$lon)

    #satellite, terrain, hybrid, and mobile are types
    wvarMap <- RgoogleMaps::GetMap.bbox(
      bb$lonR
      ,bb$latR
      ,destfile = "pwsMap.png"
      ,maptype="terrain"
      ,size=c(640,640)
    )

    #now the labels will show the metric and aggregation chosen
    wvarMapLabel <- as.character( round(
      as.vector( unlist( wvarSum[,wvaragg] ) )
    ,0) )

    #set up the map
    tmpMap <- RgoogleMaps::PlotOnStaticMap(
      wvarMap
      ,lat = wvarSum$lat
      ,lon = wvarSum$lon
      ,zoom=min(maxZoom(latrange=bb$latR,lonrange=bb$lonR))
      ,cex=0
      ,pch=20
      ,col=c("black")
    )

    #add the text
    RgoogleMaps::TextOnStaticMap(
      wvarMap
      ,lat = wvarSum$lat
      ,lon = wvarSum$lon
      ,labels=wvarMapLabel
      ,add=TRUE
      ,cex=1
      ,col=c("red")
      ,offset=0.5
    )
  } #end plottype m
  else if( plottype == "g" ){

    #for this need to flip the name to allow generic naming in the call (y)
    names(wvarSum)[names(wvarSum) == wvaragg] <- "y"

    if( length(unique(wvarSum$pwsid)) > 5){
      message("Note: The g/graph plottype can become cluttered with over 5 PWS ids")
    }

    ggplot2::ggplot( data=wvarSum, aes( x = utc_date_time, y = y, color=pwsid ) ) +
      geom_line() +
      xlab('Date') +
      ylab( paste0(aggtype," of ",wvar) ) +
      theme(
        panel.background = element_rect(fill = 'white', color = 'black')
        ,legend.box.background = element_rect(fill = 'white', color='white')
        ,legend.key = element_rect(fill = 'white')
      )

  }  #end else plottype = g
}
##
## end mcaldwel code
##



