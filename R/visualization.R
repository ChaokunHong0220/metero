#' Create a resistance heatmap
#'
#' Generates a heatmap visualizing antimicrobial resistance rates
#' across different pathogens and antibiotics.
#'
#' @param data A data frame containing AMR data
#' @param filter_conditions Optional list of filtering conditions
#' @param sort_pathogens Method to sort pathogens: "alphabetical", "resistance", or NULL
#' @param sort_antibiotics Method to sort antibiotics: "alphabetical", "resistance", "class", or NULL
#' @param color_palette Color palette for the heatmap
#' @param na_color Color for missing values
#' @param label_format Format string for cell labels (e.g., "%.1f%%")
#' @param threshold Resistance rate threshold for color differentiation
#' @param min_studies Minimum number of studies required to show a cell
#'
#' @return A ggplot2 object with the heatmap
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a basic resistance heatmap
#' heatmap <- create_amr_heatmap(amr_data)
#' 
#' # Create a filtered heatmap for a specific country
#' heatmap <- create_amr_heatmap(
#'   amr_data,
#'   filter_conditions = list(country = "India")
#' )
#' }
create_amr_heatmap <- function(data, filter_conditions = NULL, 
                              sort_pathogens = c("resistance", "alphabetical", NULL),
                              sort_antibiotics = c("class", "resistance", "alphabetical", NULL),
                              color_palette = c("viridis", "red_blue", "red_green", "custom"),
                              na_color = "gray90", 
                              label_format = "%.1f%%",
                              threshold = 0.5,
                              min_studies = 1) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations")
  }
  
  # Match arguments
  if (!is.null(sort_pathogens) && length(sort_pathogens) > 1) {
    sort_pathogens <- match.arg(sort_pathogens)
  }
  if (!is.null(sort_antibiotics) && length(sort_antibiotics) > 1) {
    sort_antibiotics <- match.arg(sort_antibiotics)
  }
  if (!is.null(color_palette) && length(color_palette) > 1) {
    color_palette <- match.arg(color_palette)
  }
  
  # Check required columns
  required_cols <- c("pathogen_name", "antibiotic_name", "resistance_rate")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Apply filtering if provided
  if (!is.null(filter_conditions) && length(filter_conditions) > 0) {
    for (field in names(filter_conditions)) {
      if (field %in% names(data)) {
        data <- data[data[[field]] == filter_conditions[[field]], ]
      } else {
        warning("Filter field not found in data: ", field)
      }
    }
    
    # Check if any data remains after filtering
    if (nrow(data) == 0) {
      stop("No data remains after applying filter conditions")
    }
  }
  
  # Aggregate data for heatmap (average resistance rates)
  if ("study_count" %in% names(data) || "study_id" %in% names(data)) {
    # If study counts are available, use weighted average
    if ("study_count" %in% names(data)) {
      agg_data <- stats::aggregate(
        data[c("resistance_rate", "study_count")],
        by = list(pathogen = data$pathogen_name, antibiotic = data$antibiotic_name),
        FUN = function(x) if(is.numeric(x)) mean(x) else x[1]
      )
    } else {
      # Count studies by study_id
      study_counts <- stats::aggregate(
        study_id ~ pathogen_name + antibiotic_name,
        data = data,
        FUN = function(x) length(unique(x))
      )
      names(study_counts)[3] <- "study_count"
      
      # Merge with data
      data <- merge(data, study_counts, 
                   by = c("pathogen_name", "antibiotic_name"),
                   all.x = TRUE)
      
      # Aggregate
      agg_data <- stats::aggregate(
        data[c("resistance_rate", "study_count")],
        by = list(pathogen = data$pathogen_name, antibiotic = data$antibiotic_name),
        FUN = function(x) if(is.numeric(x)) mean(x) else x[1]
      )
    }
    
    # Filter by minimum number of studies
    if (min_studies > 1) {
      agg_data <- agg_data[agg_data$study_count >= min_studies, ]
    }
  } else {
    # Simple aggregation if study counts are not available
    agg_data <- stats::aggregate(
      resistance_rate ~ pathogen_name + antibiotic_name,
      data = data,
      FUN = mean
    )
    names(agg_data) <- c("pathogen", "antibiotic", "resistance_rate")
    agg_data$study_count <- 1  # Assume one study per combination
  }
  
  # Check if any data remains after aggregation
  if (nrow(agg_data) == 0) {
    stop("No data remains after aggregation and filtering")
  }
  
  # Sort pathogens if requested
  if (!is.null(sort_pathogens)) {
    if (sort_pathogens == "alphabetical") {
      pathogen_order <- sort(unique(agg_data$pathogen))
    } else if (sort_pathogens == "resistance") {
      # Order by mean resistance rate
      path_means <- stats::aggregate(
        resistance_rate ~ pathogen,
        data = agg_data,
        FUN = mean
      )
      path_means <- path_means[order(-path_means$resistance_rate), ]
      pathogen_order <- path_means$pathogen
    } else {
      pathogen_order <- unique(agg_data$pathogen)
    }
    
    # Convert to factor with ordered levels
    agg_data$pathogen <- factor(agg_data$pathogen, levels = pathogen_order)
  }
  
  # Sort antibiotics if requested
  if (!is.null(sort_antibiotics)) {
    if (sort_antibiotics == "alphabetical") {
      antibiotic_order <- sort(unique(agg_data$antibiotic))
    } else if (sort_antibiotics == "resistance") {
      # Order by mean resistance rate
      ab_means <- stats::aggregate(
        resistance_rate ~ antibiotic,
        data = agg_data,
        FUN = mean
      )
      ab_means <- ab_means[order(-ab_means$resistance_rate), ]
      antibiotic_order <- ab_means$antibiotic
    } else if (sort_antibiotics == "class") {
      # Try to extract class from antibiotic name or use antibiotic classification if available
      if ("antibiotic_class" %in% names(data)) {
        # Group by class then by mean resistance within class
        class_data <- unique(data[c("antibiotic_name", "antibiotic_class")])
        names(class_data) <- c("antibiotic", "class")
        
        # Merge class data with aggregated data
        agg_data <- merge(agg_data, class_data, by = "antibiotic", all.x = TRUE)
        
        # Order classes by mean resistance
        class_means <- stats::aggregate(
          resistance_rate ~ class,
          data = agg_data,
          FUN = mean
        )
        class_means <- class_means[order(-class_means$resistance_rate), ]
        class_order <- class_means$class
        
        # Order antibiotics by class, then by resistance within class
        agg_data$class <- factor(agg_data$class, levels = class_order)
        agg_data <- agg_data[order(agg_data$class, -agg_data$resistance_rate), ]
        antibiotic_order <- unique(agg_data$antibiotic)
      } else {
        # Fallback to alphabetical if class information is not available
        antibiotic_order <- sort(unique(agg_data$antibiotic))
        warning("Antibiotic class information not available, sorting alphabetically instead")
      }
    } else {
      antibiotic_order <- unique(agg_data$antibiotic)
    }
    
    # Convert to factor with ordered levels
    agg_data$antibiotic <- factor(agg_data$antibiotic, levels = antibiotic_order)
  }
  
  # Convert resistance rate to percentage for display
  agg_data$resistance_pct <- agg_data$resistance_rate * 100
  
  # Set up color palette
  if (is.null(color_palette) || color_palette == "viridis") {
    if (!requireNamespace("viridis", quietly = TRUE)) {
      warning("Package 'viridis' not available, using default color palette")
      color_scale <- ggplot2::scale_fill_gradient2(
        low = "blue", mid = "yellow", high = "red",
        midpoint = threshold * 100,
        na.value = na_color,
        name = "Resistance (%)"
      )
    } else {
      color_scale <- viridis::scale_fill_viridis(
        option = "inferno",
        na.value = na_color,
        name = "Resistance (%)"
      )
    }
  } else if (color_palette == "red_blue") {
    color_scale <- ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = threshold * 100,
      na.value = na_color,
      name = "Resistance (%)"
    )
  } else if (color_palette == "red_green") {
    color_scale <- ggplot2::scale_fill_gradient2(
      low = "green", mid = "yellow", high = "red",
      midpoint = threshold * 100,
      na.value = na_color,
      name = "Resistance (%)"
    )
  } else {
    # Default to red-blue if custom is specified but no custom colors provided
    color_scale <- ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = threshold * 100,
      na.value = na_color,
      name = "Resistance (%)"
    )
  }
  
  # Create the heatmap
  p <- ggplot2::ggplot(agg_data, ggplot2::aes(x = antibiotic, y = pathogen, fill = resistance_pct)) +
    ggplot2::geom_tile(color = "white", size = 0.25) +
    color_scale +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  # Add text labels if requested
  if (!is.null(label_format)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf(label_format, resistance_pct)),
      color = "black",
      size = 3
    )
  }
  
  # Add study count as overlay if available
  if ("study_count" %in% names(agg_data) && min_studies > 1) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0("n=", study_count)),
      color = "black",
      size = 2,
      vjust = -1
    )
  }
  
  return(p)
}

#' Create a forest plot for meta-analysis results
#'
#' Generates a forest plot to visualize pooled resistance rates
#' and their confidence intervals from meta-analysis.
#'
#' @param meta_results Results from calculate_pooled_rate or similar function
#' @param sort_by Variable to sort results by: "rate", "ci_width", "i_squared", or NULL
#' @param color_by Variable to color points by, or NULL for no coloring
#' @param include_heterogeneity Logical; whether to include heterogeneity statistics
#' @param point_size Base size for points (will vary by sample size if weights = TRUE)
#' @param weights Logical; whether to adjust point sizes by sample size
#'
#' @return A ggplot2 object with the forest plot
#' @export
#'
#' @examples
#' \dontrun{
#' # Perform meta-analysis
#' meta_results <- calculate_pooled_rate(
#'   data = amr_data,
#'   by = c("pathogen_name", "antibiotic_name")
#' )
#' 
#' # Create forest plot
#' forest <- create_forest_plot(meta_results)
#' }
create_forest_plot <- function(meta_results, sort_by = c("rate", "ci_width", "i_squared", NULL),
                             color_by = NULL, include_heterogeneity = TRUE,
                             point_size = 3, weights = TRUE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations")
  }
  
  # Match arguments
  if (!is.null(sort_by) && length(sort_by) > 1) {
    sort_by <- match.arg(sort_by)
  }
  
  # Check input type
  if (!inherits(meta_results, "amr_meta_analysis") && 
      !is.data.frame(meta_results)) {
    stop("Input must be an amr_meta_analysis object or a data frame with meta-analysis results")
  }
  
  # Extract data based on input type
  if (inherits(meta_results, "amr_meta_analysis")) {
    plot_data <- meta_results$summary
    by_vars <- meta_results$by
  } else {
    plot_data <- meta_results
    # Try to guess grouping variables
    by_vars <- setdiff(
      names(plot_data),
      c("studies", "total_samples", "pooled_rate", "ci_lower", "ci_upper", 
        "i_squared", "heterogeneity_p", "group")
    )
  }
  
  # Check required columns
  required_cols <- c("pooled_rate", "ci_lower", "ci_upper")
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create label column combining the grouping variables
  if (length(by_vars) > 0) {
    label_parts <- lapply(by_vars, function(var) plot_data[[var]])
    plot_data$label <- do.call(paste, c(label_parts, list(sep = " - ")))
  } else if ("group" %in% names(plot_data)) {
    plot_data$label <- plot_data$group
  } else {
    plot_data$label <- "Overall"
  }
  
  # Calculate CI width for potential sorting
  plot_data$ci_width <- plot_data$ci_upper - plot_data$ci_lower
  
  # Sort data if requested
  if (!is.null(sort_by)) {
    if (sort_by == "rate") {
      plot_data <- plot_data[order(plot_data$pooled_rate), ]
    } else if (sort_by == "ci_width") {
      plot_data <- plot_data[order(plot_data$ci_width), ]
    } else if (sort_by == "i_squared" && "i_squared" %in% names(plot_data)) {
      plot_data <- plot_data[order(plot_data$i_squared), ]
    }
  }
  
  # Convert to percentages for display
  plot_data$pooled_rate_pct <- plot_data$pooled_rate * 100
  plot_data$ci_lower_pct <- plot_data$ci_lower * 100
  plot_data$ci_upper_pct <- plot_data$ci_upper * 100
  
  # Adjust labels with additional information if requested
  if (include_heterogeneity && "i_squared" %in% names(plot_data)) {
    plot_data$label_ext <- sprintf(
      "%s (I² = %.1f%%)", 
      plot_data$label, 
      plot_data$i_squared
    )
  } else {
    plot_data$label_ext <- plot_data$label
  }
  
  # Set up factor levels for correct ordering in plot
  plot_data$label_ext <- factor(plot_data$label_ext, levels = rev(unique(plot_data$label_ext)))
  
  # Set up point size adjustment by sample size if requested
  if (weights && "total_samples" %in% names(plot_data)) {
    size_var <- ggplot2::aes(size = total_samples)
    size_scale <- ggplot2::scale_size_continuous(
      name = "Sample Size",
      range = c(2, 5)
    )
  } else {
    size_var <- point_size
    size_scale <- NULL
  }
  
  # Set up color mapping if requested
  if (!is.null(color_by) && color_by %in% names(plot_data)) {
    color_var <- ggplot2::aes_string(color = color_by)
    color_scale <- ggplot2::scale_color_viridis_c(
      name = gsub("_", " ", tools::toTitleCase(color_by))
    )
  } else {
    color_var <- ggplot2::aes(color = "forestgreen")
    color_scale <- ggplot2::scale_color_identity()
  }
  
  # Create the forest plot
  p <- ggplot2::ggplot(
    plot_data, 
    ggplot2::aes(
      x = pooled_rate_pct, 
      y = label_ext, 
      xmin = ci_lower_pct, 
      xmax = ci_upper_pct
    )
  ) +
    ggplot2::geom_vline(xintercept = 50, linetype = "dashed", color = "gray") +
    ggplot2::geom_errorbarh(height = 0.2) +
    ggplot2::geom_point(size_var, color_var) +
    color_scale +
    ggplot2::labs(
      title = "Forest Plot of Antimicrobial Resistance Rates",
      x = "Resistance Rate (%)",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10)
    )
  
  # Add size scale if using weights
  if (!is.null(size_scale)) {
    p <- p + size_scale
  }
  
  # Add text labels with the actual percentages
  p <- p + ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f%% (%.1f-%.1f)", 
                               pooled_rate_pct, ci_lower_pct, ci_upper_pct)),
    hjust = -0.1,
    size = 3
  )
  
  # Add study counts if available
  if ("studies" %in% names(plot_data)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("k=%d", studies)),
      hjust = -0.5,
      size = 3
    )
  }
  
  return(p)
}

#' Create a geographic map of AMR data
#'
#' Generates a choropleth map visualizing antimicrobial resistance rates
#' across different geographical regions.
#'
#' @param data A data frame containing AMR data with country information
#' @param pathogen Optional pathogen to filter for
#' @param antibiotic Optional antibiotic to filter for
#' @param map_region Region to focus on: "world", "south_asia", "europe", etc.
#' @param title Plot title
#' @param color_palette Color palette for the map
#'
#' @return A ggplot2 object with the choropleth map
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a world map of E. coli resistance to ciprofloxacin
#' map <- create_geo_map(
#'   data = amr_data,
#'   pathogen = "Escherichia coli",
#'   antibiotic = "Ciprofloxacin"
#' )
#' }
create_geo_map <- function(data, pathogen = NULL, antibiotic = NULL,
                         map_region = "world", title = NULL,
                         color_palette = c("viridis", "red_blue", "red_green")) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations")
  }
  
  if (!requireNamespace("maps", quietly = TRUE)) {
    stop("Package 'maps' is required for geographic visualizations")
  }
  
  # Match arguments
  color_palette <- match.arg(color_palette)
  
  # Check required columns
  if (!("country" %in% names(data)) && !("region" %in% names(data))) {
    stop("Data must include either 'country' or 'region' column")
  }
  
  if (!("resistance_rate" %in% names(data))) {
    stop("Data must include 'resistance_rate' column")
  }
  
  # Apply filtering if requested
  filtered_data <- data
  
  if (!is.null(pathogen)) {
    pathogen_col <- if ("pathogen_name" %in% names(data)) "pathogen_name" else "pathogen"
    if (!(pathogen_col %in% names(data))) {
      stop("Pathogen column not found in data")
    }
    filtered_data <- filtered_data[filtered_data[[pathogen_col]] == pathogen, ]
  }
  
  if (!is.null(antibiotic)) {
    antibiotic_col <- if ("antibiotic_name" %in% names(data)) "antibiotic_name" else "antibiotic"
    if (!(antibiotic_col %in% names(data))) {
      stop("Antibiotic column not found in data")
    }
    filtered_data <- filtered_data[filtered_data[[antibiotic_col]] == antibiotic, ]
  }
  
  # Check if any data remains after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remains after filtering")
  }
  
  # Determine the geographic column to use
  geo_col <- if ("country" %in% names(filtered_data)) "country" else "region"
  
  # Aggregate data by region/country
  agg_data <- stats::aggregate(
    resistance_rate ~ filtered_data[[geo_col]],
    data = filtered_data,
    FUN = mean
  )
  names(agg_data) <- c("region", "resistance_rate")
  
  # Convert to percentage for better display
  agg_data$resistance_pct <- agg_data$resistance_rate * 100
  
  # Get the map data
  if (map_region == "world") {
    map_data <- maps::map("world", plot = FALSE, fill = TRUE)
  } else if (map_region == "south_asia") {
    map_data <- maps::map("world", region = c("India", "Pakistan", "Bangladesh", 
                                             "Nepal", "Sri Lanka", "Bhutan", 
                                             "Maldives", "Afghanistan"),
                         plot = FALSE, fill = TRUE)
  } else {
    # Try to use the specified region
    tryCatch({
      map_data <- maps::map(map_region, plot = FALSE, fill = TRUE)
    }, error = function(e) {
      stop("Invalid map region: ", map_region)
    })
  }
  
  # Convert to data frame for ggplot
  map_df <- ggplot2::fortify(map_data)
  
  # Standardize country names for matching
  map_df$region <- tolower(map_df$region)
  agg_data$region_std <- tolower(agg_data$region)
  
  # Create title if not provided
  if (is.null(title)) {
    title_parts <- c("AMR Map")
    if (!is.null(pathogen)) title_parts <- c(title_parts, paste("for", pathogen))
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
  p <- ggplot2::ggplot() +
    ggplot2::geom_map(
      data = agg_data, 
      map = map_df,
      ggplot2::aes(map_id = region_std, fill = resistance_pct), 
      color = "black", size = 0.25
    ) +
    ggplot2::expand_limits(x = map_df$long, y = map_df$lat) +
    color_scale +
    ggplot2::labs(
      title = title,
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

#' Create a time trend plot for AMR data
#'
#' Generates a line plot showing the trend of antimicrobial resistance
#' rates over time, with options for grouping and smoothing.
#'
#' @param data A data frame containing AMR data with time information
#' @param time_var Name of the time variable
#' @param group_vars Optional character vector of grouping variables
#' @param smooth_method Smoothing method: "loess", "lm", "gam", or NULL for none
#' @param span Smoothing span for loess
#' @param min_years Minimum number of years required for trend analysis
#' @param point_size Size of the data points
#' @param line_size Size of the trend lines
#' @param conf_int Logical; whether to display confidence intervals
#'
#' @return A ggplot2 object with the trend plot
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a basic time trend plot
#' trend_plot <- create_trend_plot(
#'   data = amr_data, 
#'   time_var = "year"
#' )
#' 
#' # Create a grouped trend plot
#' trend_plot <- create_trend_plot(
#'   data = amr_data,
#'   time_var = "year",
#'   group_vars = c("pathogen_name")
#' )
#' }
create_trend_plot <- function(data, time_var, group_vars = NULL,
                            smooth_method = c("loess", "lm", "gam", NULL),
                            span = 0.75, min_years = 3, point_size = 2,
                            line_size = 1, conf_int = TRUE) {
  # Check dependencies
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for visualizations")
  }
  
  # Match arguments
  if (!is.null(smooth_method) && length(smooth_method) > 1) {
    smooth_method <- match.arg(smooth_method)
  }
  
  # Check required columns
  required_cols <- c(time_var, "resistance_rate")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check time variable
  if (!is.numeric(data[[time_var]])) {
    warning("Time variable is not numeric, attempting to convert")
    data[[time_var]] <- as.numeric(as.character(data[[time_var]]))
  }
  
  # Apply grouping if specified
  if (!is.null(group_vars)) {
    # Check if all grouping variables exist in the data
    missing_vars <- setdiff(group_vars, names(data))
    if (length(missing_vars) > 0) {
      stop("The following grouping variables are not in the data: ", 
           paste(missing_vars, collapse = ", "))
    }
    
    # Aggregate data by time and grouping variables
    group_formula <- as.formula(
      paste("resistance_rate ~", paste(c(time_var, group_vars), collapse = " + "))
    )
    
    if ("sample_count" %in% names(data)) {
      # Use weighted average if sample counts are available
      agg_data <- stats::aggregate(
        cbind(resistance_rate, sample_count) ~ eval(group_formula),
        data = data,
        FUN = function(x) if(is.numeric(x)) sum(x) else x[1]
      )
      
      # Convert sample-weighted sums to means
      agg_data$resistance_rate <- agg_data$resistance_rate / agg_data$sample_count
    } else {
      agg_data <- stats::aggregate(
        group_formula,
        data = data,
        FUN = mean
      )
    }
    
  } else {
    # Aggregate by time only
    time_formula <- as.formula(paste("resistance_rate ~", time_var))
    
    if ("sample_count" %in% names(data)) {
      # Use weighted average if sample counts are available
      agg_data <- stats::aggregate(
        cbind(resistance_rate, sample_count) ~ eval(time_formula),
        data = data,
        FUN = function(x) if(is.numeric(x)) sum(x) else x[1]
      )
      
      # Convert sample-weighted sums to means
      agg_data$resistance_rate <- agg_data$resistance_rate / agg_data$sample_count
    } else {
      agg_data <- stats::aggregate(
        time_formula,
        data = data,
        FUN = mean
      )
    }
  }
  
  # Convert resistance rate to percentage for display
  agg_data$resistance_pct <- agg_data$resistance_rate * 100
  
  # Check for minimum number of years
  if (!is.null(group_vars)) {
    # Check each group has enough time points
    group_cols <- agg_data[, group_vars, drop = FALSE]
    unique_groups <- unique(do.call(paste, c(group_cols, list(sep = "-"))))
    
    for (group in unique_groups) {
      group_values <- strsplit(group, "-")[[1]]
      filter_expr <- rep(TRUE, nrow(agg_data))
      
      for (i in seq_along(group_vars)) {
        filter_expr <- filter_expr & (agg_data[[group_vars[i]]] == group_values[i])
      }
      
      group_data <- agg_data[filter_expr, ]
      time_points <- length(unique(group_data[[time_var]]))
      
      if (time_points < min_years) {
        warning("Group '", group, "' has only ", time_points, 
                " time points, which is less than the required minimum of ", min_years)
      }
    }
  } else {
    # Check overall data has enough time points
    time_points <- length(unique(agg_data[[time_var]]))
    if (time_points < min_years) {
      warning("Data has only ", time_points, 
              " time points, which is less than the required minimum of ", min_years)
    }
  }
  
  # Build the plot
  if (!is.null(group_vars)) {
    # Create a group interaction term for color and facet
    if (length(group_vars) > 1) {
      agg_data$group <- do.call(paste, c(lapply(group_vars, function(var) agg_data[[var]]), list(sep = " - ")))
    } else {
      agg_data$group <- agg_data[[group_vars]]
    }
    
    # Create grouped plot
    p <- ggplot2::ggplot(agg_data, ggplot2::aes_string(x = time_var, y = "resistance_pct", color = "group", group = "group")) +
      ggplot2::geom_point(size = point_size) +
      ggplot2::geom_line(size = line_size / 2, alpha = 0.7)
    
    # Add smoothing if requested
    if (!is.null(smooth_method)) {
      if (smooth_method == "loess") {
        p <- p + ggplot2::geom_smooth(
          method = "loess", 
          se = conf_int, 
          span = span,
          size = line_size
        )
      } else if (smooth_method == "lm") {
        p <- p + ggplot2::geom_smooth(
          method = "lm", 
          se = conf_int,
          size = line_size
        )
      } else if (smooth_method == "gam") {
        if (!requireNamespace("mgcv", quietly = TRUE)) {
          warning("Package 'mgcv' not available, using loess smoothing instead")
          p <- p + ggplot2::geom_smooth(
            method = "loess", 
            se = conf_int, 
            span = span,
            size = line_size
          )
        } else {
          p <- p + ggplot2::geom_smooth(
            method = "gam", 
            formula = y ~ s(x, bs = "cs"),
            se = conf_int,
            size = line_size
          )
        }
      }
    }
    
    # Add color scale
    if (length(unique(agg_data$group)) <= 8) {
      p <- p + ggplot2::scale_color_brewer(palette = "Dark2")
    } else {
      if (requireNamespace("viridis", quietly = TRUE)) {
        p <- p + viridis::scale_color_viridis(discrete = TRUE)
      }
    }
    
  } else {
    # Create simple plot without grouping
    p <- ggplot2::ggplot(agg_data, ggplot2::aes_string(x = time_var, y = "resistance_pct")) +
      ggplot2::geom_point(size = point_size) +
      ggplot2::geom_line(size = line_size / 2)
    
    # Add smoothing if requested
    if (!is.null(smooth_method)) {
      if (smooth_method == "loess") {
        p <- p + ggplot2::geom_smooth(
          method = "loess", 
          se = conf_int, 
          span = span,
          size = line_size,
          color = "blue"
        )
      } else if (smooth_method == "lm") {
        p <- p + ggplot2::geom_smooth(
          method = "lm", 
          se = conf_int,
          size = line_size,
          color = "blue"
        )
      } else if (smooth_method == "gam") {
        if (!requireNamespace("mgcv", quietly = TRUE)) {
          warning("Package 'mgcv' not available, using loess smoothing instead")
          p <- p + ggplot2::geom_smooth(
            method = "loess", 
            se = conf_int, 
            span = span,
            size = line_size,
            color = "blue"
          )
        } else {
          p <- p + ggplot2::geom_smooth(
            method = "gam", 
            formula = y ~ s(x, bs = "cs"),
            se = conf_int,
            size = line_size,
            color = "blue"
          )
        }
      }
    }
  }
  
  # Add plot styling
  p <- p + ggplot2::labs(
    title = "Antimicrobial Resistance Trends Over Time",
    x = tools::toTitleCase(gsub("_", " ", time_var)),
    y = "Resistance (%)"
  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # If many groups, consider faceting instead of coloring
  if (!is.null(group_vars) && length(unique(agg_data$group)) > 8) {
    p <- p + ggplot2::facet_wrap(~group)
  }
  
  return(p)
} 