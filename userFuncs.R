# function to retrieve longtidue and lattitude from place names
longlat <- function(addr) {
  url = paste0("http://maps.google.com/maps/api/geocode/xml?address=",
               addr)
  doc <- NA
  coord <- NA
  try(doc <- XML::xmlTreeParse(url), silent = T)
  if (!is.na(doc[1])) {
    root = XML::xmlRoot(doc)
    long = XML::xmlValue(root[["result"]][["geometry"]][["location"]][["lng"]])
    lat = XML::xmlValue(root[["result"]][["geometry"]][["location"]][["lat"]])
    coord <- c(long, lat)
    names(coord) <- c('long', 'lat')
  } else {
    print(paste("Error: Could not find", addr))
  }
  return(coord)
}

# function to find nearby cities
findNearbyCities <- function(username, coord, distance, maxRows, maxRowsIni=500) {
  options(geonamesUsername = username)
  results <-
    geonames::GNfindNearbyPlaceName(
      lat = coord["lat"],
      lng = coord["long"],
      radius = as.character(distance),
      maxRows = as.character(maxRowsIni),
      #maxRows = as.character(maxRows),
      style = "long"
    )
  ##
  ## begin mcaldwel code
  ##
  #initial results can return just neighborhoods within 1 city
  #which can result in same limited number of weather stations so I added maxRowsIni
  #then cut it down, it looks like population is an indicator of legit city
  results$population <- as.numeric(results$population)
  results <- head( subset(results,population>0 ), as.numeric(maxRows) )
  #we are now also going need a parammeter to limit the number of PWS
  ##
  ## end mcaldwel code
  ##
  
  nearbyCities <- as.character()
  for (i in 1:length(unlist(results[1]))) {
    cityName <- gsub(' ', '_', results[i, ]$toponymName)
    nearbyCities[i] <-
      gsub(' ',
           '_',
           ifelse(
             results[i, ]$countryCode == "US",
             paste(results[i, ]$adminCode1, cityName, sep = '/'),
             paste(results[i, ]$countryName, cityName, sep = '/')
           ))
  }
  nearbyCities
}


queryData <- function(myKey, nearbyCities, startTime) {
  wuApiPrefix <- "http://api.wunderground.com/api/"
  wuFormat <- ".json"
  weatherData <- list()
  combinedData <- NULL
  for (i in 1:length(nearbyCities)) {
    wuURL <- paste(
      wuApiPrefix,
      myKey,
      '/history_',
      gsub("-", "", startTime),
      '/q/',
      nearbyCities[i],
      wuFormat,
      sep = ''
    )
    data <- NULL
    data <- jsonlite::fromJSON(wuURL)
    
    weatherData <- NULL
    weatherData <- data$history$observations
    
    year  <- weatherData$date$year
    mon <- weatherData$date$mon
    mday <- weatherData$date$mday
    hour <- weatherData$date$hour
    min <- weatherData$date$min
    date_time <- NULL
    date_time <- paste(year, mon, mday, hour, min, sep = '-')
    weatherData <- cbind(date_time, weatherData,stringsAsFactors = FALSE)
    
    utc_year  <- weatherData$utcdate$year
    utc_mon <- weatherData$utcdate$mon
    utc_mday <- weatherData$utcdate$mday
    utc_hour <- weatherData$utcdate$hour
    utc_min <- weatherData$utcdate$min
    utc_date_time <- NULL
    utc_date_time <- paste(utc_year, utc_mon, utc_mday, utc_hour, utc_min, sep = '-')
    weatherData <- cbind(utc_date_time, weatherData, stringsAsFactors = FALSE)
    
    city_name <- rep(nearbyCities[i], nrow(weatherData))
    weatherData <- cbind(city_name, weatherData, stringsAsFactors = FALSE)
    
    weatherData$date <- NULL
    weatherData$utcdate <- NULL
    Sys.sleep(1)
    combinedData <- rbind(combinedData, weatherData, stringsAsFactors = FALSE)
  }
  combinedData$metar <- NULL
  combinedData
}


