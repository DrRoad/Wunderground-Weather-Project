#' A package for Wunderground weather project
#'
#' A collection of simple functions for illustration. See examples
#' for functions \code{link{findNearbyCities}} and \code{link{queryData}}.
#' @seealso \code{\link{findNearbyCities}}
#' @seealso \code{link{findPWS}}
#' @seealso \code{link{longlat}}
#' @seealso \code{link{plotWeather}}
#' @seealso \code{link{queryData}}
#' @seealso \code{link{queryHistory}}
#' @seealso \code{link{summarizeData}}
#' @docType package

#' @name weatherProject
NULL

utils::globalVariables(names = c("id",
                                 "utc_date_time",
                                 "pwsid",
                                 "lat",
                                 "lon",
								 "tempm",
								 "tempi",
								 "dewptm",
								 "dewpti",
								 "pressurem",
								 "pressurei",
								 "hum",
								 "tempmmax",
								 "tempmmin",
								 "tempimax",
								 "tempimin",
								 "dewptmmax",
								 "dewptmmin",
								 "dewptimax",
								 "dewptimin",
								 "pressuremmax",
								 "pressuremmin",
								 "pressureimax",
								 "pressureimin",
								 "hummax",
								 "hummin",
								 "y",
								 "pwsNo",
                                 "distance_km"))
							

