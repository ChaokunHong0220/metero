#' Analyze AMR data time trends
#'
#' This function analyzes changes in antimicrobial resistance (AMR) data over different time periods,
#' and can perform grouped analysis based on specified variables (such as pathogen, antibiotic, or country).
#'
#' @param data Standardized AMR data frame
#' @param time_var Time variable column name, default is "year"
#' @param group_vars Vector of variable names for grouping, such as c("pathogen_name", "country")
#' @param min_studies Minimum number of studies required for each time point, default is 1
#' @param smooth_method Trend line smoothing method, options: "loess" or "lm"
#'
#' @return A list containing time trend analysis results:
#'   \item{trend_data}{Analyzed data frame with time points and corresponding resistance rates}
#'   \item{plot}{Time trend visualization ggplot object}
#'   \item{model_summary}{If smooth_method="lm", includes linear model summary}
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze E. coli resistance trends across different antibiotics
#' data(human_amr)
#' ec_data <- subset(human_amr, pathogen_name == "Escherichia coli")
#' trends <- analyze_amr_trends(ec_data, group_vars = "antibiotic_name")
#' 
#' # Show trend chart
#' print(trends$plot)
#' }
analyze_amr_trends <- function(data, 
                              time_var = "year", 
                              group_vars = NULL,
                              min_studies = 1,
                              smooth_method = "loess") {
  
  # Check if required packages are available
  if (!requireNamespace("dplyr", quietly = TRUE) || 
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("dplyr and ggplot2 packages are required")
  }
  
  # Validate input data
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }
  
  if (!time_var %in% names(data)) {
    stop(paste0("Time variable not found in data: ", time_var))
  }
  
  if (!is.null(group_vars)) {
    missing_vars <- group_vars[!group_vars %in% names(data)]
    if (length(missing_vars) > 0) {
      stop("The following grouping variables not found in data: ", paste(missing_vars, collapse = ", "))
    }
  }
  
  # Check if key variables exist
  required_vars <- c("study_id", "sample_count", "resistant_count")
  for (var in required_vars) {
    if (!var %in% names(data)) {
      stop(paste0("Missing required variable: ", var))
    }
  }
  
  # Build grouping variables
  group_cols <- c(time_var)
  if (!is.null(group_vars)) {
    group_cols <- c(group_cols, group_vars)
  }
  
  # Calculate summary data for each time point and group
  trend_data <- data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(group_cols))) %>%
    dplyr::summarise(
      studies = dplyr::n_distinct(study_id, na.rm = TRUE),
      total_samples = sum(sample_count, na.rm = TRUE),
      total_resistant = sum(resistant_count, na.rm = TRUE),
      resistance_rate = total_resistant / total_samples,
      .groups = "drop"
    ) %>%
    dplyr::filter(studies >= min_studies) %>%
    dplyr::arrange_at(dplyr::vars(dplyr::one_of(group_cols)))
  
  # If no data meets the conditions
  if (nrow(trend_data) == 0) {
    warning("No data points available for trend analysis")
    return(list(trend_data = trend_data, 
                plot = NULL, 
                model_summary = NULL))
  }
  
  # Create base trend chart
  p <- ggplot2::ggplot(trend_data, 
         ggplot2::aes_string(x = time_var, y = "resistance_rate")) +
    ggplot2::labs(
      x = "Year",
      y = "Resistance Rate",
      title = "Antimicrobial Resistance Time Trends"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggplot2::theme_minimal()
  
  # Handle grouping variables visualization
  if (!is.null(group_vars) && length(group_vars) > 0) {
    # If only one grouping variable, use color to distinguish
    if (length(group_vars) == 1) {
      p <- p + 
        ggplot2::aes_string(color = group_vars[1], group = group_vars[1]) +
        ggplot2::geom_point(ggplot2::aes(size = total_samples), alpha = 0.7) +
        ggplot2::scale_size_continuous(name = "Sample Size")
    } 
    # If two grouping variables, use color and shape
    else if (length(group_vars) == 2) {
      p <- p + 
        ggplot2::aes_string(color = group_vars[1], 
                            shape = group_vars[2], 
                            group = paste0(group_vars[1], ":", group_vars[2])) +
        ggplot2::geom_point(ggplot2::aes(size = total_samples), alpha = 0.7) +
        ggplot2::scale_size_continuous(name = "Sample Size")
    }
    # If more than two variables, use facets
    else {
      facet_formula <- paste(group_vars[-1], collapse = " ~ ")
      facet_formula <- paste("~", facet_formula)
      p <- p + 
        ggplot2::aes_string(color = group_vars[1], 
                            group = group_vars[1]) +
        ggplot2::geom_point(ggplot2::aes(size = total_samples), alpha = 0.7) +
        ggplot2::facet_grid(stats::as.formula(facet_formula)) +
        ggplot2::scale_size_continuous(name = "Sample Size")
    }
  } else {
    # No grouping
    p <- p + 
      ggplot2::geom_point(ggplot2::aes(size = total_samples), alpha = 0.7) +
      ggplot2::scale_size_continuous(name = "Sample Size")
  }
  
  # Add trend lines
  model_summaries <- list()
  
  if (smooth_method == "loess") {
    if (!is.null(group_vars) && length(group_vars) > 0) {
      p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE)
    } else {
      p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE)
    }
  } else if (smooth_method == "lm") {
    if (!is.null(group_vars) && length(group_vars) > 0) {
      # Build linear models for each group
      groups <- unique(trend_data %>% 
                       dplyr::select_at(dplyr::vars(dplyr::one_of(group_vars))))
      
      for (i in 1:nrow(groups)) {
        # Create filter condition
        filter_cond <- rep(TRUE, nrow(trend_data))
        for (var in group_vars) {
          filter_cond <- filter_cond & (trend_data[[var]] == groups[[var]][i])
        }
        
        # Filter data
        group_data <- trend_data[filter_cond, ]
        
        if (nrow(group_data) >= 3) {  # Need at least 3 points for linear regression
          # Build model
          model <- stats::lm(resistance_rate ~ get(time_var), data = group_data)
          
          # Create model summary
          group_name <- paste(groups[i, ], collapse = "_")
          model_summaries[[group_name]] <- list(
            coefficients = stats::coef(model),
            p_value = stats::anova(model)$"Pr(>F)"[1],
            r_squared = summary(model)$r.squared
          )
        }
      }
      
      p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE)
    } else {
      # Non-grouped linear trend
      model <- stats::lm(resistance_rate ~ get(time_var), data = trend_data)
      model_summaries[["overall"]] <- list(
        coefficients = stats::coef(model),
        p_value = stats::anova(model)$"Pr(>F)"[1],
        r_squared = summary(model)$r.squared
      )
      
      p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE)
    }
  }
  
  # Return results
  return(list(
    trend_data = trend_data,
    plot = p,
    model_summary = model_summaries
  ))
}

#' Create antimicrobial resistance heatmap
#'
#' This function generates a heatmap showing resistance patterns between different pathogens and antibiotics.
#'
#' @param data Standardized AMR data frame
#' @param pathogen_var Pathogen variable name, default is "pathogen_name"
#' @param antibiotic_var Antibiotic variable name, default is "antibiotic_name"
#' @param filter_conditions Optional filter conditions to select specific data subsets
#'
#' @return ggplot2 heatmap object
#' @export
#'
#' @examples
#' \dontrun{
#' data(human_amr)
#' # Create heatmap of all data
#' heatmap <- create_amr_heatmap(human_amr)
#' 
#' # Create heatmap for a specific country
#' india_heatmap <- create_amr_heatmap(
#'   human_amr, 
#'   filter_conditions = list(country = "India")
#' )
#' 
#' # Show heatmap
#' print(heatmap)
#' }
create_amr_heatmap <- function(data, 
                              pathogen_var = "pathogen_name",
                              antibiotic_var = "antibiotic_name",
                              filter_conditions = NULL) {
  
  # Check if required packages are available
  if (!requireNamespace("dplyr", quietly = TRUE) || 
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("dplyr and ggplot2 packages are required")
  }
  
  # Validate input data
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }
  
  # Validate variables exist
  if (!pathogen_var %in% names(data)) {
    stop(paste0("Pathogen variable not found in data: ", pathogen_var))
  }
  
  if (!antibiotic_var %in% names(data)) {
    stop(paste0("Antibiotic variable not found in data: ", antibiotic_var))
  }
  
  # Apply filter conditions
  filtered_data <- data
  if (!is.null(filter_conditions) && is.list(filter_conditions)) {
    for (var_name in names(filter_conditions)) {
      if (var_name %in% names(filtered_data)) {
        filter_value <- filter_conditions[[var_name]]
        filtered_data <- filtered_data[filtered_data[[var_name]] %in% filter_value, ]
      }
    }
  }
  
  # Check if data remains after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data remains after applying filter conditions")
  }
  
  # Calculate resistance rate for each pathogen-antibiotic combination
  heatmap_data <- filtered_data %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(c(pathogen_var, antibiotic_var)))) %>%
    dplyr::summarise(
      studies = dplyr::n_distinct(study_id, na.rm = TRUE),
      total_samples = sum(sample_count, na.rm = TRUE),
      total_resistant = sum(resistant_count, na.rm = TRUE),
      resistance_rate = total_resistant / total_samples,
      .groups = "drop"
    ) %>%
    dplyr::filter(total_samples >= 10)  # Require at least 10 samples for inclusion in heatmap
  
  # Create heatmap
  p <- ggplot2::ggplot(heatmap_data, 
         ggplot2::aes_string(x = antibiotic_var, y = pathogen_var)) +
    ggplot2::geom_tile(ggplot2::aes(fill = resistance_rate), color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", resistance_rate * 100)), 
              color = "black", size = 3) +
    ggplot2::scale_fill_gradient2(
      name = "Resistance Rate",
      low = "green", mid = "yellow", high = "red",
      midpoint = 0.5,
      limits = c(0, 1),
      labels = scales::percent
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Antimicrobial Resistance Heatmap",
      x = "Antibiotic",
      y = "Pathogen"
    )
  
  return(p)
}

#' Perform multivariate analysis of antimicrobial resistance data
#'
#' This function uses logistic regression to analyze various factors affecting antimicrobial resistance.
#'
#' @param data Standardized AMR data frame
#' @param target_pathogen Target pathogen name
#' @param target_antibiotic Target antibiotic name
#' @param predictors Vector of predictor variable names, e.g., c("year", "country", "setting")
#' @param min_samples Minimum sample size to include in analysis, default is 30
#'
#' @return Logistic regression model results list
#' @export
#'
#' @examples
#' \dontrun{
#' data(human_amr)
#' # Analyze factors affecting E. coli resistance to ciprofloxacin
#' model_result <- analyze_amr_factors(
#'   human_amr,
#'   target_pathogen = "Escherichia coli",
#'   target_antibiotic = "Ciprofloxacin",
#'   predictors = c("year", "country", "setting", "population_type")
#' )
#' 
#' # View model summary
#' print(model_result$summary)
#' }
analyze_amr_factors <- function(data,
                               target_pathogen,
                               target_antibiotic,
                               predictors,
                               min_samples = 30) {
  
  # Validate input data
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }
  
  # Filter data for specific pathogen and antibiotic
  filtered_data <- data %>%
    dplyr::filter(
      pathogen_name == target_pathogen,
      antibiotic_name == target_antibiotic
    )
  
  # Check if sufficient data
  if (nrow(filtered_data) < min_samples) {
    stop(paste0("Insufficient data; at least ", min_samples, " records required"))
  }
  
  # Check if predictors exist
  missing_vars <- predictors[!predictors %in% names(filtered_data)]
  if (length(missing_vars) > 0) {
    stop("The following predictor variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Create response variable (binary)
  filtered_data$resistant <- filtered_data$resistant_count / filtered_data$sample_count
  
  # Build formula
  formula_str <- paste("resistant ~", paste(predictors, collapse = " + "))
  
  # Build weighted logistic regression model
  model <- stats::glm(
    stats::as.formula(formula_str),
    family = stats::binomial(),
    weights = sample_count,
    data = filtered_data
  )
  
  # Model summary
  model_summary <- summary(model)
  
  # Extract OR values and confidence intervals
  coef_table <- stats::coef(model_summary)
  odds_ratios <- exp(coef_table[, "Estimate"])
  ci_lower <- exp(coef_table[, "Estimate"] - 1.96 * coef_table[, "Std. Error"])
  ci_upper <- exp(coef_table[, "Estimate"] + 1.96 * coef_table[, "Std. Error"])
  
  # Integrate results table
  result_table <- data.frame(
    Variable = rownames(coef_table),
    OR = odds_ratios,
    CI_Lower = ci_lower,
    CI_Upper = ci_upper,
    p_value = coef_table[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  
  # Return results
  return(list(
    model = model,
    summary = model_summary,
    result_table = result_table,
    anova = stats::anova(model, test = "Chisq"),
    data_count = nrow(filtered_data)
  ))
} 