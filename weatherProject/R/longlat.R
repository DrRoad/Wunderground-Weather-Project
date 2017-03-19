#' Find longtitude and latitude coordinates of a given address 
#'
#' @description find longtitude and latitude coordinates of a given address 
#'
#' @param addr a character vector of an address
#' 
#' @return coord longitude and latitude in character format. It will stop if it cannot find the address 
#' @export
#' @importFrom xml2 read_xml xml_text xml_contents xml_find_all
#'
#'
#' @examples
#' longlat("San Francisco, US")

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