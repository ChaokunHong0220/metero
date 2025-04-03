#' Launch the metero Shiny application
#'
#' This function launches the interactive Shiny application for the metero package,
#' allowing users to explore AMR data, perform analyses, and create visualizations.
#'
#' @param data Optional data frame containing AMR data to load into the application
#' @param ... Additional parameters passed to shiny::runApp
#'
#' @return This function does not return a value. It launches the Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the app with no pre-loaded data
#' launch_metero()
#'
#' # Launch the app with pre-loaded data
#' data <- import_amr_data("my_amr_data.csv")
#' launch_metero(data = data)
#' }
launch_metero <- function(data = NULL, ...) {
  # The required packages should be automatically installed because 
  # they are in the DESCRIPTION file's Imports section.
  # But just in case, we'll provide helpful messages.
  
  # Check for required packages
  missing_packages <- character(0)
  required_packages <- c("shiny", "shinydashboard", "DT", "leaflet", "plotly")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  # If any packages are missing, try to install them
  if (length(missing_packages) > 0) {
    message("Installing required packages: ", paste(missing_packages, collapse = ", "))
    utils::install.packages(missing_packages)
    
    # Check again if installation succeeded
    still_missing <- character(0)
    for (pkg in missing_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        still_missing <- c(still_missing, pkg)
      }
    }
    
    # If there are still missing packages, show error
    if (length(still_missing) > 0) {
      stop("Could not install required packages: ", paste(still_missing, collapse = ", "), 
           "\nPlease install these packages manually with install.packages()")
    }
  }
  
  # Save data to a temporary environment that will be accessible to the Shiny app
  if (!is.null(data)) {
    metero_env <- new.env()
    metero_env$data <- data
    options(metero.env = metero_env)
  }
  
  # Get the directory containing the Shiny app
  app_dir <- system.file("shiny-apps/metero", package = "metero")
  
  # Check if app directory exists
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try reinstalling 'metero'.")
  }
  
  # Launch the Shiny app
  message("Starting metero Shiny application...")
  shiny::runApp(app_dir, ...)
  
  # Clean up
  if (!is.null(data)) {
    options(metero.env = NULL)
  }
}

#' Get current data in Shiny app
#'
#' Internal function to retrieve the current data loaded in the Shiny app
#'
#' @return The data frame containing AMR data, or NULL if no data is loaded
#' @keywords internal
get_metero_data <- function() {
  metero_env <- getOption("metero.env")
  if (is.null(metero_env)) {
    return(NULL)
  }
  
  return(metero_env$data)
} 