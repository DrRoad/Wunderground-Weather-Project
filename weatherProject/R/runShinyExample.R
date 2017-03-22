#' Example to use package with a shiny app
#'
#' @description Example to use package with a shiny app 

#' @importFrom shiny runApp
#' @export
#' 
##
## begin nan code
##
runShinyExample <- function() {
  appDir <- system.file("shiny-examples", "shinyApp.R", package = "weatherProject")
  if (appDir == "") {
    stop("Could not find example directory.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

##
## end nan code
##