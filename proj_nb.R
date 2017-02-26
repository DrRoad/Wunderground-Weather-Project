## Stat 290: Weather Map Project 
## Nan Bai 
## 

require(XML)
require(jsonlite)
require(tibble)

myKey <- "407e6151ab7de146"
wuApiPrefix <- "http://api.wunderground.com/api/"
wuFormat <- ".json"

opwAPIprefix <- "http://api.openweathermap.org/data/2.5/box/city?bbox="
opwFormat <- ".json"
personalID <- "&appid=2c9024acce426b6e2dfb9fb439ed6e45"


# startTime <- as.POSIXct("2017-02-01 2:13:46 PST")
# endTime <- as.POSIXct("2017-02-02 2:13:46 PST")

startTime <- as.Date("2017-02-01 2:13:46 PST")
endTime <- as.Date("2017-02-02 2:13:46 PST")


addr = "indianapolis"
dist_long = 5;
dist_lat = 5; 

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

coord <- longlat(addr)

# use the box to extract its nearby cities from openWeatherMap 
# box = paste(as.character(coord[1]-dist_long), as.character(coord[2]-dist_lat), 
#                            as.character(coord[1]+dist_long), as.character(coord[2]+dist_lat), 
#                            10, sep = ',')
# opwURL <- paste0(opwAPIprefix, box,personalID)
# opw_data <- fromJSON(opwURL)
# cities <- opw_data$list$name
# cities_coords <- opw_data$list$coord

# use geonames web service to query nearby cities
username <- "nan_stat290"
options(geonamesUsername=username)
distance = "30"
maxRows = "10"
results <- geonames::GNfindNearbyPlaceName(lat = coord["lat"], lng = coord["long"], 
                                           radius = distance, maxRows = maxRows)

weatherData <- list()
for (i in 1:length(results)){
  
# query their history data from Wunderground weather 
  cityName <- gsub(' ', '_', results[i,]$toponymName)

  address <- ifelse(results[i,]$countryCode=="US", 
         paste(results[i,]$adminCode1, cityName, sep = '/'),  
         paste(cityName, results[i,]$countryId, sep = '/'))
    
  wuURL <- paste(wuApiPrefix, myKey, '/history_',
               gsub("-","",startTime), '/q/', address, wuFormat, sep = '')
  data <- fromJSON(wuURL)
  addName <- gsub('/', '_', address)
  weatherData[[i]] <- data
  Sys.sleep(60)
  
}


