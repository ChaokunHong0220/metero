#' Create a South Asia region AMR map
#'
#' Creates a specialized map visualization focused on the South Asian region,
#' showing antimicrobial resistance patterns across countries in this area.
#'
#' @param data A data frame containing AMR data
#' @param pathogen Pathogen to filter by (optional)
#' @param antibiotic Antibiotic to filter by (optional)
#' @param color_palette Color palette for the map (default: "viridis")
#' @param title Custom title for the map (optional)
#' @param include_afghanistan Logical; whether to include Afghanistan in the map (default: TRUE)
#'
#' @return A ggplot2 object with the South Asia map
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a map of E. coli resistance to Ciprofloxacin in South Asia
#' south_asia_map <- create_south_asia_map(
#'   data = amr_data,
#'   pathogen = "E. coli",
#'   antibiotic = "Ciprofloxacin"
#' )
#' }
create_south_asia_map <- function(data, pathogen = NULL, antibiotic = NULL,
                                 color_palette = c("viridis", "red_blue", "red_green"),
                                 title = NULL, include_afghanistan = TRUE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations")
  }
  
  if (!requireNamespace("maps", quietly = TRUE)) {
    stop("Package 'maps' is required for geographic visualizations")
  }
  
  # Match arguments
  color_palette <- match.arg(color_palette)
  
  # Define South Asian countries
  south_asia_countries <- c("India", "Pakistan", "Bangladesh", "Nepal", 
                           "Sri Lanka", "Bhutan", "Maldives")
  
  # Add Afghanistan if requested
  if (include_afghanistan) {
    south_asia_countries <- c(south_asia_countries, "Afghanistan")
  }
  
  # Filter data by region or countries
  filtered_data <- data
  
  # First try filtering by region
  if ("region" %in% names(data) && any(grepl("South Asia", data$region, ignore.case = TRUE))) {
    filtered_data <- filtered_data[grepl("South Asia", filtered_data$region, ignore.case = TRUE), ]
  } 
  # Then filter by countries if needed
  else if ("country" %in% names(data)) {
    filtered_data <- filtered_data[filtered_data$country %in% south_asia_countries, ]
  }
  
  # Check if any data remains after region/country filtering
  if (nrow(filtered_data) == 0) {
    stop("No South Asian data found in the dataset")
  }
  
  # Apply pathogen filtering if specified
  if (!is.null(pathogen)) {
    pathogen_col <- if ("pathogen_name" %in% names(filtered_data)) "pathogen_name" else "pathogen"
    if (!(pathogen_col %in% names(filtered_data))) {
      stop("Pathogen column not found in data")
    }
    
    # Allow partial matching (e.g., "E. coli" or "Ecoli" both match)
    pattern <- pathogen
    if (grepl("\\.", pathogen)) {
      # Handle abbreviated format like "E. coli"
      pattern <- gsub("\\. ", "", pathogen)
    }
    
    pattern_matches <- grepl(pattern, filtered_data[[pathogen_col]], ignore.case = TRUE)
    if (sum(pattern_matches) == 0) {
      stop("No matching pathogen found in South Asian data")
    }
    
    filtered_data <- filtered_data[pattern_matches, ]
  }
  
  # Apply antibiotic filtering if specified
  if (!is.null(antibiotic)) {
    antibiotic_col <- if ("antibiotic_name" %in% names(filtered_data)) "antibiotic_name" else "antibiotic"
    if (!(antibiotic_col %in% names(filtered_data))) {
      stop("Antibiotic column not found in data")
    }
    
    # Allow partial matching for antibiotic names
    pattern <- antibiotic
    pattern_matches <- grepl(pattern, filtered_data[[antibiotic_col]], ignore.case = TRUE)
    if (sum(pattern_matches) == 0) {
      stop("No matching antibiotic found in South Asian data")
    }
    
    filtered_data <- filtered_data[pattern_matches, ]
  }
  
  # Check if any data remains after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remains after filtering for specified pathogen and antibiotic")
  }
  
  # Create a map limited to South Asia
  map_data <- maps::map("world", region = south_asia_countries, plot = FALSE, fill = TRUE)
  map_df <- ggplot2::fortify(map_data)
  
  # Standardize country names for matching
  map_df$region <- tolower(map_df$region)
  
  # Aggregate resistance rates by country
  resistance_data <- NULL
  if ("country" %in% names(filtered_data)) {
    if ("resistance_rate" %in% names(filtered_data)) {
      resistance_data <- stats::aggregate(
        resistance_rate ~ country, 
        data = filtered_data, 
        FUN = function(x) mean(x, na.rm = TRUE)
      )
      
      # Convert to percentage
      resistance_data$resistance_pct <- resistance_data$resistance_rate * 100
      
      # Add standardized country names for map matching
      resistance_data$region_std <- tolower(resistance_data$country)
    }
  }
  
  # Create title if not provided
  if (is.null(title)) {
    title_parts <- c("AMR in South Asia")
    if (!is.null(pathogen)) {
      pathogen_display <- if (grepl("\\.", pathogen)) pathogen else pathogen
      title_parts <- c(title_parts, paste("for", pathogen_display))
    }
    if (!is.null(antibiotic)) title_parts <- c(title_parts, paste("against", antibiotic))
    title <- paste(title_parts, collapse = " ")
  }
  
  # Set up color palette
  if (color_palette == "viridis") {
    if (!requireNamespace("viridis", quietly = TRUE)) {
      warning("Package 'viridis' not available, using default color palette")
      color_scale <- ggplot2::scale_fill_gradient2(
        low = "blue", mid = "yellow", high = "red",
        midpoint = 50,
        na.value = "gray90",
        name = "Resistance (%)"
      )
    } else {
      color_scale <- viridis::scale_fill_viridis(
        option = "inferno",
        na.value = "gray90",
        name = "Resistance (%)"
      )
    }
  } else if (color_palette == "red_blue") {
    color_scale <- ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 50,
      na.value = "gray90",
      name = "Resistance (%)"
    )
  } else if (color_palette == "red_green") {
    color_scale <- ggplot2::scale_fill_gradient2(
      low = "green", mid = "yellow", high = "red",
      midpoint = 50,
      na.value = "gray90",
      name = "Resistance (%)"
    )
  }
  
  # Create the map
  p <- ggplot2::ggplot()
  
  # Add map boundaries
  p <- p + ggplot2::geom_map(
    data = map_df,
    map = map_df,
    ggplot2::aes(map_id = region, x = long, y = lat),
    fill = "gray90",
    color = "gray50",
    size = 0.25
  )
  
  # Add resistance data if available
  if (!is.null(resistance_data)) {
    p <- p + ggplot2::geom_map(
      data = resistance_data, 
      map = map_df,
      ggplot2::aes(map_id = region_std, fill = resistance_pct), 
      color = "black", 
      size = 0.25
    )
    
    # Add color scale
    p <- p + color_scale
  }
  
  # Add country labels
  p <- p + ggplot2::geom_text(
    data = get_country_centers(south_asia_countries),
    ggplot2::aes(x = long, y = lat, label = country),
    size = 3,
    fontface = "bold"
  )
  
  # Set map appearance
  p <- p + ggplot2::expand_limits(x = map_df$long, y = map_df$lat) +
    ggplot2::labs(
      title = title,
      subtitle = "South Asia Region",
      fill = "Resistance (%)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  return(p)
}

# Internal helper function to get approximate country centers for labels
get_country_centers <- function(countries) {
  # Approximate centers for South Asian countries
  centers <- data.frame(
    country = c("India", "Pakistan", "Bangladesh", "Nepal", 
                "Sri Lanka", "Bhutan", "Maldives", "Afghanistan"),
    long = c(78.0, 70.0, 90.0, 84.0, 80.5, 90.5, 73.5, 66.0),
    lat = c(22.0, 30.0, 23.5, 28.0, 7.5, 27.5, 3.2, 34.0),
    stringsAsFactors = FALSE
  )
  
  # Filter to requested countries
  centers <- centers[centers$country %in% countries, ]
  
  return(centers)
} 