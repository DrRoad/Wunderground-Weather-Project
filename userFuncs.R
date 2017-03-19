
##
## begin nan code
##


# function to retrieve longtidue and lattitude from place names
longlat <- function(addr) {
  # remove empty space from addr to prepare for query
  addr <- gsub(' ', '', addr)
  url = paste0("http://maps.google.com/maps/api/geocode/xml?address=",
               addr)
  doc <- NULL
  coord <- NULL
  try(doc <- xml2::read_xml(x = url), silent = T)
  if (!is.null(doc[1])) {
    long_xpath <- "//result/geometry/location/lng"
    lat_xpath <- "//result/geometry/location/lat"
    long <-
      xml2::xml_text(xml2::xml_contents(xml2::xml_find_all(x = doc, xpath = long_xpath)))
    lat <-
      xml2::xml_text(xml2::xml_contents(xml2::xml_find_all(x = doc, xpath = lat_xpath)))
    coord <- c(long, lat)
    names(coord) <- c('long', 'lat')
  } else {
    print(paste("Error: Could not find", addr))
  }
  return(coord)
}

# function to find nearby polulated cities
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


##
## begin mcaldwel code
##

# function to find all nearby weather stations
findPWS <- function(myKey, nearbyCities) {
  wuApiPrefix <- "http://api.wunderground.com/api/"
  wuFormat <- ".json"
  allPws <- NULL
  
  for (i in 1:length(nearbyCities)) {
    callAddress <-
      paste0(wuApiPrefix,
             myKey,
             '/geolookup/q/',
             nearbyCities[i],
             wuFormat)
    callData <- jsonlite::fromJSON(callAddress)
    Sys.sleep(1)
    print(callAddress)
    
    if (exists("location", where = callData)
        && exists("nearby_weather_stations", where = callData$location)
        &&
        exists("pws", where = callData$location$nearby_weather_stations)
        &&
        exists("station", where = callData$location$nearby_weather_stations$pws)) {
      callDataPws <-
        tibble::as_tibble(callData$location$nearby_weather_stations$pws$station)
      
      callDataPws$pwsNo = 1:nrow(callDataPws)
      if(is.null(allPws)){
        allPws <- callDataPws
      }
      else{
        allPws <- dplyr::union_all(allPws,callDataPws)
      }
    }
  }
  
  allPws <- dplyr::distinct(allPws, id, .keep_all = TRUE)
  
}

##
## end mcaldwel code
##


##
## begin nan code
##

# function to query history data for all weather stations
queryHistory <-
  function(myKey, nearbyStations, startDate, endDate) {
    # Due to call limit per day, we limit the max num of pws per city
    nearbyStations <- subset(nearbyStations, pwsNo <= 5)
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
  
  as.data.frame(weatherData)
  
}


# This function is to subset weather data according to user input 
summarizeData <- function(weatherData, distance, startDate, endDate, param, unit, calType){
  data <- weatherData %>%
    dplyr::filter(as.Date(utc_date_time)>=startDate && as.Date(utc_date_time)<=endDate) %>%
    dplyr::filter(distance_km<=distance) 
  
  if (unit=='metric'){
  
    if (param=='temperature') colNum = match('tempm', names(data))
    if (param=='dew point') colNum = match('dewptm', names(data))
    if (param=='humidity') colNum = match('hum', names(data))
    if (param=='wind speed') colNum = match('wspdm', names(data))
    if (param=='wind gust') colNum = match('wgustm', names(data))
    if (param=='wind direction') colNum = match('wdird', names(data))
    if (param=='pressure') colNum = match('pressurem', names(data))
    if (param=='precipitation rate') colNum = match('precip_ratem', names(data))
    if (param=='precipitation total') colNum = match('precip_ratem', names(data))}
  
  else{
    
    if (param=='temperature') colNum = match('tempi', names(data))
    if (param=='dew point') colNum = match('dewpti', names(data))
    if (param=='humidity') colNum = match('hum', names(data))
    if (param=='wind speed') colNum = match('wspdi', names(data))
    if (param=='wind gust') colNum = match('wgusti', names(data))
    if (param=='wind direction') colNum = match('wdire', names(data))
    if (param=='pressure') colNum = match('pressurei', names(data))
    if (param=='precipitation rate') colNum = match('precip_ratei', names(data))
    if (param=='precipitation total') colNum = match('precip_ratei', names(data))}
  
  
  results <- data %>%
    dplyr::select(pwsid, lat, lon, colNum) %>%
    dplyr::group_by(pwsid)
  
  if (calType=='mean')  results <- dplyr::summarize(results, stat = mean(colNum))
  if (calType=='max')  results <- dplyr::summarize(results, stat = max(colNum))
  if (calType=='min')  results <- dplyr::summarize(results, stat = min(colNum))
  if (calType=='range')  results <- dplyr::summarize(results, stat = range(colNum))
  
  results
}

##
## end nan code
##
