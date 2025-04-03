# Global settings and functions for metero Shiny app

# Load required packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(metero)  # Load main package

# Helper functions for the app

# Function to retrieve the data from the metero environment
get_app_data <- function() {
  metero_env <- getOption("metero.env")
  if (is.null(metero_env) || is.null(metero_env$data)) {
    return(NULL)
  }
  return(metero_env$data)
}

# Filter data based on user selections
filter_data <- function(data, pathogens = NULL, antibiotics = NULL, countries = NULL, years = NULL) {
  if (is.null(data)) return(NULL)
  
  filtered <- data
  
  # Filter by pathogen
  if (!is.null(pathogens) && length(pathogens) > 0) {
    pathogen_col <- if ("pathogen_name" %in% names(data)) "pathogen_name" else "pathogen"
    filtered <- filtered[filtered[[pathogen_col]] %in% pathogens, ]
  }
  
  # Filter by antibiotic
  if (!is.null(antibiotics) && length(antibiotics) > 0) {
    antibiotic_col <- if ("antibiotic_name" %in% names(data)) "antibiotic_name" else "antibiotic"
    filtered <- filtered[filtered[[antibiotic_col]] %in% antibiotics, ]
  }
  
  # Filter by country
  if (!is.null(countries) && length(countries) > 0) {
    if ("country" %in% names(data)) {
      filtered <- filtered[filtered$country %in% countries, ]
    }
  }
  
  # Filter by year
  if (!is.null(years) && length(years) > 0) {
    if ("year" %in% names(data)) {
      filtered <- filtered[filtered$year %in% years, ]
    }
  }
  
  return(filtered)
}

# Format metadata for display
format_metadata <- function(data) {
  if (is.null(data)) return(NULL)
  
  meta <- list(
    n_records = nrow(data),
    pathogens = if ("pathogen_name" %in% names(data)) length(unique(data$pathogen_name)) else 
                if ("pathogen" %in% names(data)) length(unique(data$pathogen)) else NA,
    antibiotics = if ("antibiotic_name" %in% names(data)) length(unique(data$antibiotic_name)) else
                  if ("antibiotic" %in% names(data)) length(unique(data$antibiotic)) else NA,
    countries = if ("country" %in% names(data)) length(unique(data$country)) else NA,
    years = if ("year" %in% names(data)) paste(range(data$year, na.rm = TRUE), collapse = " - ") else NA
  )
  
  return(meta)
}

# Get list of available pathogens
get_pathogens <- function(data) {
  if (is.null(data)) return(NULL)
  
  pathogen_col <- if ("pathogen_name" %in% names(data)) "pathogen_name" else "pathogen"
  if (!(pathogen_col %in% names(data))) return(NULL)
  
  return(sort(unique(data[[pathogen_col]])))
}

# Get list of available antibiotics
get_antibiotics <- function(data) {
  if (is.null(data)) return(NULL)
  
  antibiotic_col <- if ("antibiotic_name" %in% names(data)) "antibiotic_name" else "antibiotic"
  if (!(antibiotic_col %in% names(data))) return(NULL)
  
  return(sort(unique(data[[antibiotic_col]])))
}

# Get list of available countries
get_countries <- function(data) {
  if (is.null(data) || !("country" %in% names(data))) return(NULL)
  
  return(sort(unique(data$country)))
}

# Get list of available years
get_years <- function(data) {
  if (is.null(data) || !("year" %in% names(data))) return(NULL)
  
  return(sort(unique(data$year)))
}

# Get available visualization options based on data
get_viz_options <- function(data) {
  if (is.null(data)) return(NULL)
  
  options <- list()
  
  # Check for geographic visualization
  if ("country" %in% names(data) || "region" %in% names(data)) {
    options$geo_map <- "Geographic Map"
  }
  
  # Check for trend visualization
  if ("year" %in% names(data)) {
    options$trend <- "Time Trend Plot"
  }
  
  # Always available
  options$forest <- "Forest Plot"
  options$heatmap <- "Resistance Heatmap"
  
  return(options)
}

# South Asian countries
south_asian_countries <- c("India", "Pakistan", "Bangladesh", "Nepal", 
                          "Sri Lanka", "Bhutan", "Maldives", "Afghanistan") 