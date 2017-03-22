#' Aggregate weather data from pws within certain distance and specified date range 
#'
#' @description Aggregate weather data from pws within certain distance and specified date range
#'
#' @param weatherData a tibble containing some history weather data for some pws 
#' @param distance a numeric to specify distance in km
#' @param startDate a starting date  
#' @param endDate an end date 
#' @param param a weather parameter of interest 
#' @param unit either 'metric' or 'english'
#' @param calType the statistics of interest: mean, max, min, range 
#' 
#' @return a tibble containing the results 
#' 
#' @export
#' @importFrom dplyr filter select group_by summarise_all
#' @importFrom magrittr %>%
#'
#' @examples
#' data(weatherData)
#' summarizeData(weatherData, 10, '2017-03-18', '2017-03-19', 'temperature', 'metric', 'mean')

##
## start nan code 
##
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
    dplyr::select(id, lat, lon, colNum) %>%
    dplyr::group_by(id)
  
  names(results)[4] <- 'stat'
  if (calType=='mean')  results <- dplyr::summarise_all(results, mean,  na.rm=TRUE)
  if (calType=='max')  results <- dplyr::summarise_all(results, max,  na.rm=TRUE)
  if (calType=='min')  results <- dplyr::summarise_all(results, min,  na.rm=TRUE)
  if (calType=='range')  results <- dplyr::summarise_all(results, range,  na.rm=TRUE)
  
  results
}
##
## end nan code 
##