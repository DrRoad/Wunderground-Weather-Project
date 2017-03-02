# function to retrieve longtidue and lattitude from place names 
longlat <- function(addr) {
  url = paste0("http://maps.google.com/maps/api/geocode/xml?address=", addr)
  doc <- NA
  try(doc <- xmlTreeParse(url), silent = T)
  if (!is.na(doc[1])) {
    root = xmlRoot(doc)
    long = xmlValue(root[["result"]][["geometry"]][["location"]][["lng"]])
    lat = xmlValue(root[["result"]][["geometry"]][["location"]][["lat"]])
    coord <- c(long, lat)
    names(coord) <- c('long', 'lat')
  } else {
    print(paste("Error: Could not find", addr))
  }
  return(coord)
}

# function to find nearby cities
findNearbyCities <- function(username, coord, distance, maxRows){
  require(geonames)
  options(geonamesUsername=username)
  results <- geonames::GNfindNearbyPlaceName(lat = coord["lat"], lng = coord["long"], 
                                           radius = distance, maxRows = maxRows)
  
  nearbyCities <- list()
  for (i in 1:length(results)){
    
    cityName <- gsub(' ', '_', results[i,]$toponymName)
    
    nearbyCities[[i]] <- ifelse(results[i,]$countryCode=="US", 
                      paste(results[i,]$adminCode1, cityName, sep = '/'),  
                      paste(cityName, results[i,]$countryId, sep = '/'))
  }
}
