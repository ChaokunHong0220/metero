#' Calculate pooled antimicrobial resistance rates
#'
#' Performs meta-analysis to calculate pooled (combined) resistance rates 
#' across multiple studies, with options for stratification and different 
#' meta-analytic methods.
#'
#' @param data A data frame containing AMR data
#' @param by Character vector of variables to group by
#' @param method Meta-analysis method: "fixed" or "random"
#' @param measure Measure type: "PLO" (proportion logit), "PAS" (proportion arcsine), 
#'                or "PFT" (proportion Freeman-Tukey)
#' @param min_studies Minimum number of studies required for pooling
#' @param weights Weighting approach: "sample_size", "inverse_variance", or "equal"
#' @param conf_level Confidence level for intervals (0-1)
#'
#' @return A list with the meta-analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculate pooled resistance rates by pathogen and antibiotic
#' results <- calculate_pooled_rate(
#'   data = amr_data,
#'   by = c("pathogen", "specific_antibiotic"),
#'   method = "random"
#' )
#' }
calculate_pooled_rate <- function(data, by = NULL, method = c("random", "fixed"),
                                 measure = c("PLO", "PAS", "PFT"), 
                                 min_studies = 2, weights = "sample_size",
                                 conf_level = 0.95) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Match arguments
  method <- match.arg(method)
  measure <- match.arg(measure)
  
  # Check for required packages
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("Package 'meta' is required for meta-analysis")
  }
  
  # Check for required columns
  required_cols <- c("sample_count", "resistant_count")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Prepare grouping variables
  if (is.null(by)) {
    # If no grouping variables, treat entire dataset as one group
    results <- perform_meta_analysis(
      data, 
      method = method, 
      measure = measure,
      min_studies = min_studies,
      weights = weights,
      conf_level = conf_level
    )
    
    results$overall <- TRUE
    
    return(results)
  } else {
    # Check if all grouping variables exist in the data
    missing_vars <- setdiff(by, names(data))
    if (length(missing_vars) > 0) {
      stop("The following grouping variables are not in the data: ", 
           paste(missing_vars, collapse = ", "))
    }
    
    # Handle special case of study_id to identify multiple observations within studies
    has_study_id <- "study_id" %in% names(data)
    
    # Split data into groups and analyze each group
    # Using base R for compatibility
    group_data <- split(data, lapply(by, function(var) data[[var]]))
    
    # Initialize results list
    group_results <- list()
    
    # Process each group
    for (i in seq_along(group_data)) {
      group_df <- group_data[[i]]
      group_name <- names(group_data)[i]
      
      # Skip groups with too few studies
      study_count <- if (has_study_id) length(unique(group_df$study_id)) else nrow(group_df)
      if (study_count < min_studies) {
        next
      }
      
      # Perform meta-analysis for this group
      meta_result <- tryCatch({
        perform_meta_analysis(
          group_df, 
          method = method, 
          measure = measure,
          min_studies = min_studies,
          weights = weights,
          conf_level = conf_level
        )
      }, error = function(e) {
        warning("Error in meta-analysis for group ", group_name, ": ", e$message)
        NULL
      })
      
      # Only keep successful results
      if (!is.null(meta_result)) {
        # Add group identifiers
        group_vars <- strsplit(group_name, "\\.")[[1]]
        for (j in seq_along(by)) {
          meta_result[[by[j]]] <- group_vars[j]
        }
        
        meta_result$overall <- FALSE
        group_results[[group_name]] <- meta_result
      }
    }
    
    # Check if any groups were successfully analyzed
    if (length(group_results) == 0) {
      warning("No groups had enough studies for meta-analysis")
      return(NULL)
    }
    
    # Combine all results into a data frame
    combined_results <- lapply(seq_along(group_results), function(i) {
      res <- group_results[[i]]
      
      # Create summary row
      result_row <- data.frame(
        group = names(group_results)[i],
        studies = res$meta$k,
        total_samples = res$total_samples,
        pooled_rate = res$pooled_rate,
        ci_lower = res$ci_lower,
        ci_upper = res$ci_upper,
        i_squared = res$i_squared,
        heterogeneity_p = res$heterogeneity_p,
        stringsAsFactors = FALSE
      )
      
      # Add grouping variables
      for (var in by) {
        result_row[[var]] <- res[[var]]
      }
      
      result_row
    })
    
    results <- list(
      summary = do.call(rbind, combined_results),
      details = group_results,
      method = method,
      measure = measure,
      by = by
    )
    
    class(results) <- c("amr_meta_analysis", "list")
    return(results)
  }
}

#' Internal function to perform meta-analysis on a data frame
#'
#' @param data Data frame with AMR data
#' @param method Meta-analysis method
#' @param measure Effect measure type
#' @param min_studies Minimum studies required
#' @param weights Weighting approach
#' @param conf_level Confidence level
#'
#' @return List with meta-analysis results
#' @keywords internal
perform_meta_analysis <- function(data, method, measure, min_studies, weights, conf_level) {
  # Check if there are enough observations
  if (nrow(data) < min_studies) {
    return(NULL)
  }
  
  # Prepare data for meta-analysis
  meta_data <- data.frame(
    study = if ("study_id" %in% names(data)) data$study_id else paste0("Study", 1:nrow(data)),
    event = data$resistant_count,
    n = data$sample_count,
    stringsAsFactors = FALSE
  )
  
  # Apply weights if specified
  if (weights == "sample_size") {
    # Default weighting in meta package is by inverse variance
    # So no additional weights are needed for this option
    w <- NULL 
  } else if (weights == "equal") {
    w <- rep(1, nrow(meta_data))
  } else if (weights == "inverse_variance") {
    # Default in meta package, no need to specify
    w <- NULL
  } else {
    # Default to sample size weighting if invalid option
    w <- NULL
    warning("Invalid weighting method specified. Using sample size weighting.")
  }
  
  # Perform meta-analysis
  meta_result <- meta::metaprop(
    event = meta_data$event,
    n = meta_data$n,
    studlab = meta_data$study,
    method = method,
    sm = measure,
    level = conf_level,
    weight = w
  )
  
  # Extract and format results
  result <- list(
    meta = meta_result,
    pooled_rate = meta_result$TE.random,
    ci_lower = meta_result$lower.random,
    ci_upper = meta_result$upper.random,
    i_squared = meta_result$I2,
    heterogeneity_p = meta_result$pval.Q,
    total_samples = sum(meta_data$n),
    study_count = length(unique(meta_data$study))
  )
  
  # If fixed effects were requested, override with fixed effect results
  if (method == "fixed") {
    result$pooled_rate <- meta_result$TE.fixed
    result$ci_lower <- meta_result$lower.fixed
    result$ci_upper <- meta_result$upper.fixed
  }
  
  return(result)
}

#' Print method for AMR meta-analysis results
#'
#' @param x An amr_meta_analysis object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the meta-analysis object
#' @export
print.amr_meta_analysis <- function(x, ...) {
  cat("AMR Meta-Analysis Results\n")
  cat("========================\n\n")
  
  cat("Method:", x$method, "effects using", x$measure, "transformation\n\n")
  
  cat("Stratification:", paste(x$by, collapse = ", "), "\n\n")
  
  cat("Summary of Pooled Estimates:\n")
  
  # Format data frame for nice printing
  summary_df <- x$summary
  summary_df$pooled_rate <- sprintf("%.4f", summary_df$pooled_rate)
  summary_df$ci <- sprintf("(%.4f-%.4f)", summary_df$ci_lower, summary_df$ci_upper)
  summary_df$i_squared <- sprintf("%.1f%%", summary_df$i_squared)
  
  # Select and rename columns for printing
  print_df <- summary_df[, c(x$by, "studies", "total_samples", "pooled_rate", "ci", "i_squared")]
  names(print_df) <- c(x$by, "Studies", "Samples", "Rate", "95% CI", "I²")
  
  print(print_df, row.names = FALSE)
  
  invisible(x)
}

#' Analyze heterogeneity in AMR data
#'
#' Examines sources of heterogeneity in antimicrobial resistance data using
#' meta-regression and subgroup analyses.
#'
#' @param data A data frame containing AMR data
#' @param by Character vector of variables to group by
#' @param moderators Character vector of potential moderator variables
#' @param method Meta-analysis method: "fixed" or "random"
#' @param measure Measure type: "PLO", "PAS", or "PFT"
#'
#' @return A list with heterogeneity analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze heterogeneity by pathogen-antibiotic with country as moderator
#' results <- analyze_heterogeneity(
#'   data = amr_data,
#'   by = c("pathogen", "specific_antibiotic"),
#'   moderators = c("country")
#' )
#' }
analyze_heterogeneity <- function(data, by = NULL, moderators = NULL,
                                method = c("random", "fixed"),
                                measure = c("PLO", "PAS", "PFT")) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Match arguments
  method <- match.arg(method)
  measure <- match.arg(measure)
  
  # Check for required packages
  if (!requireNamespace("meta", quietly = TRUE) || 
      !requireNamespace("metafor", quietly = TRUE)) {
    stop("Packages 'meta' and 'metafor' are required for heterogeneity analysis")
  }
  
  # Check for required columns
  required_cols <- c("sample_count", "resistant_count", "study_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check moderators
  if (!is.null(moderators)) {
    missing_mods <- setdiff(moderators, names(data))
    if (length(missing_mods) > 0) {
      stop("The following moderator variables are not in the data: ", 
           paste(missing_mods, collapse = ", "))
    }
  }
  
  # Prepare results list
  result <- list(
    by = by,
    moderators = moderators,
    method = method,
    measure = measure,
    overall = NULL,
    subgroups = NULL,
    meta_regression = NULL
  )
  
  # Perform overall heterogeneity analysis
  overall_meta <- calculate_pooled_rate(
    data = data,
    by = by,
    method = method,
    measure = measure
  )
  
  if (is.null(overall_meta)) {
    warning("Could not calculate overall meta-analysis")
    return(NULL)
  }
  
  result$overall <- overall_meta
  
  # Perform subgroup analyses for each moderator
  if (!is.null(moderators)) {
    subgroup_results <- list()
    
    for (mod in moderators) {
      # Skip if moderator is also a grouping variable
      if (mod %in% by) {
        next
      }
      
      # Create subgroup formula
      subgroup_formula <- as.formula(paste("~", mod))
      
      # Extract data for meta-regression
      meta_data <- prepare_meta_data(data, overall_meta$by)
      
      # Perform subgroup analysis
      tryCatch({
        # Use metafor for subgroup analysis with meta-regression
        meta_obj <- metafor::rma(
          yi = meta_data$yi,
          vi = meta_data$vi,
          mods = subgroup_formula,
          data = meta_data$data,
          method = ifelse(method == "fixed", "FE", "REML")
        )
        
        # Extract subgroup results
        levels <- unique(meta_data$data[[mod]])
        subgroup_stats <- data.frame(
          moderator = mod,
          level = levels,
          studies = NA,
          pooled_rate = NA,
          ci_lower = NA,
          ci_upper = NA,
          stringsAsFactors = FALSE
        )
        
        # For each level, calculate subgroup meta-analysis
        for (i in seq_along(levels)) {
          level_data <- subset(data, data[[mod]] == levels[i])
          level_meta <- calculate_pooled_rate(
            data = level_data,
            by = by,
            method = method,
            measure = measure
          )
          
          if (!is.null(level_meta) && nrow(level_meta$summary) > 0) {
            subgroup_stats$studies[i] <- level_meta$summary$studies[1]
            subgroup_stats$pooled_rate[i] <- level_meta$summary$pooled_rate[1]
            subgroup_stats$ci_lower[i] <- level_meta$summary$ci_lower[1]
            subgroup_stats$ci_upper[i] <- level_meta$summary$ci_upper[1]
          }
        }
        
        # Add model information
        subgroup_result <- list(
          moderator = mod,
          stats = subgroup_stats,
          model = meta_obj,
          q_between = meta_obj$QM,
          p_between = meta_obj$QMp,
          test = ifelse(method == "fixed", "QM (omnibus)", "Likelihood ratio")
        )
        
        subgroup_results[[mod]] <- subgroup_result
        
      }, error = function(e) {
        warning("Subgroup analysis for moderator '", mod, "' failed: ", e$message)
      })
    }
    
    result$subgroups <- subgroup_results
    
    # Perform meta-regression with all moderators
    if (length(moderators) > 0) {
      tryCatch({
        # Create meta-regression formula with all moderators
        mr_formula <- as.formula(paste("~", paste(moderators, collapse = " + ")))
        
        # Extract data for meta-regression
        meta_data <- prepare_meta_data(data, overall_meta$by)
        
        # Perform meta-regression
        meta_reg <- metafor::rma(
          yi = meta_data$yi,
          vi = meta_data$vi,
          mods = mr_formula,
          data = meta_data$data,
          method = ifelse(method == "fixed", "FE", "REML")
        )
        
        # Extract and format results
        coef_table <- coef(summary(meta_reg))
        mr_results <- data.frame(
          moderator = rownames(coef_table),
          estimate = coef_table[, 1],
          std_error = coef_table[, 2],
          z_value = coef_table[, 3],
          p_value = coef_table[, 4],
          ci_lower = coef_table[, 5],
          ci_upper = coef_table[, 6],
          stringsAsFactors = FALSE
        )
        
        # Add model information
        result$meta_regression <- list(
          model = meta_reg,
          coefficients = mr_results,
          r_squared = meta_reg$R2,
          q_model = meta_reg$QM,
          p_model = meta_reg$QMp,
          test = ifelse(method == "fixed", "QM (omnibus)", "Likelihood ratio")
        )
        
      }, error = function(e) {
        warning("Meta-regression failed: ", e$message)
      })
    }
  }
  
  class(result) <- c("amr_heterogeneity", "list")
  return(result)
}

#' Prepare data for meta-regression analysis
#'
#' @param data AMR data frame
#' @param by Grouping variables
#'
#' @return List with meta-regression data
#' @keywords internal
prepare_meta_data <- function(data, by) {
  # Calculate effect sizes and variances for meta-regression
  # Using logit transformation for proportions
  data$yi <- log(data$resistant_count / (data$sample_count - data$resistant_count))
  data$vi <- 1/data$resistant_count + 1/(data$sample_count - data$resistant_count)
  
  # Handle infinite values (0 or 100% resistance)
  if (any(is.infinite(data$yi))) {
    # Add 0.5 to cells with zero counts (continuity correction)
    inf_rows <- which(is.infinite(data$yi))
    for (i in inf_rows) {
      if (data$resistant_count[i] == 0) {
        data$resistant_count[i] <- 0.5
        data$sample_count[i] <- data$sample_count[i] + 0.5
      } else if (data$resistant_count[i] == data$sample_count[i]) {
        data$resistant_count[i] <- data$sample_count[i] - 0.5
        data$sample_count[i] <- data$sample_count[i] + 0.5
      }
    }
    
    # Recalculate effect sizes with continuity correction
    data$yi <- log(data$resistant_count / (data$sample_count - data$resistant_count))
    data$vi <- 1/data$resistant_count + 1/(data$sample_count - data$resistant_count)
  }
  
  return(list(yi = data$yi, vi = data$vi, data = data))
}

#' Print method for AMR heterogeneity analysis
#'
#' @param x An amr_heterogeneity object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the heterogeneity analysis object
#' @export
print.amr_heterogeneity <- function(x, ...) {
  cat("AMR Heterogeneity Analysis\n")
  cat("==========================\n\n")
  
  cat("Method:", x$method, "effects using", x$measure, "transformation\n\n")
  
  # Print overall results
  cat("Overall heterogeneity:\n")
  cat("I² =", sprintf("%.1f%%", x$overall$summary$i_squared[1]), "\n")
  cat("Heterogeneity p-value =", sprintf("%.4f", x$overall$summary$heterogeneity_p[1]), "\n\n")
  
  # Print subgroup analyses
  if (!is.null(x$subgroups) && length(x$subgroups) > 0) {
    cat("Subgroup Analyses:\n")
    
    for (mod_name in names(x$subgroups)) {
      mod <- x$subgroups[[mod_name]]
      cat("\nModerator:", mod$moderator, "\n")
      cat("Between-group heterogeneity: Q =", sprintf("%.2f", mod$q_between), 
          ", p =", sprintf("%.4f", mod$p_between), "\n")
      
      # Print subgroup statistics
      subgroup_df <- mod$stats
      subgroup_df$ci <- sprintf("(%.4f-%.4f)", subgroup_df$ci_lower, subgroup_df$ci_upper)
      print_df <- subgroup_df[, c("level", "studies", "pooled_rate", "ci")]
      names(print_df) <- c("Level", "Studies", "Rate", "95% CI")
      print(print_df, row.names = FALSE)
    }
  }
  
  # Print meta-regression results
  if (!is.null(x$meta_regression)) {
    cat("\nMeta-Regression Results:\n")
    cat("R² =", sprintf("%.2f", x$meta_regression$r_squared), "\n")
    cat("Model test:", x$meta_regression$test, "=", sprintf("%.2f", x$meta_regression$q_model), 
        ", p =", sprintf("%.4f", x$meta_regression$p_model), "\n\n")
    
    # Print coefficients
    coef_df <- x$meta_regression$coefficients
    coef_df$ci <- sprintf("(%.4f-%.4f)", coef_df$ci_lower, coef_df$ci_upper)
    coef_df$p_value <- ifelse(coef_df$p_value < 0.001, "<0.001", sprintf("%.4f", coef_df$p_value))
    print_df <- coef_df[, c("moderator", "estimate", "std_error", "ci", "p_value")]
    names(print_df) <- c("Term", "Estimate", "Std.Error", "95% CI", "p-value")
    print(print_df, row.names = FALSE)
  }
  
  invisible(x)
}

#' Perform subgroup analysis of AMR data
#'
#' Conducts meta-analysis stratified by one or more categorical variables,
#' providing pooled estimates for each subgroup.
#'
#' @param data A data frame containing AMR data
#' @param by Character vector of variables to group by
#' @param subgroups Character vector of variables for subgrouping
#' @param method Meta-analysis method: "fixed" or "random"
#' @param measure Measure type: "PLO", "PAS", or "PFT"
#' @param min_studies Minimum number of studies required for each subgroup
#'
#' @return A list with subgroup analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Analyze resistance rates by setting for each pathogen-antibiotic combination
#' results <- perform_subgroup_analysis(
#'   data = amr_data,
#'   by = c("pathogen", "specific_antibiotic"),
#'   subgroups = c("setting")
#' )
#' }
perform_subgroup_analysis <- function(data, by = NULL, subgroups = NULL,
                                    method = c("random", "fixed"),
                                    measure = c("PLO", "PAS", "PFT"),
                                    min_studies = 2) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Match arguments
  method <- match.arg(method)
  measure <- match.arg(measure)
  
  # Check for required packages
  if (!requireNamespace("meta", quietly = TRUE)) {
    stop("Package 'meta' is required for subgroup analysis")
  }
  
  # Check subgroups
  if (is.null(subgroups)) {
    stop("At least one subgroup variable must be specified")
  }
  
  missing_vars <- setdiff(subgroups, names(data))
  if (length(missing_vars) > 0) {
    stop("The following subgroup variables are not in the data: ", 
         paste(missing_vars, collapse = ", "))
  }
  
  # Create overall grouping including both 'by' and 'subgroups'
  all_grouping <- c(by, subgroups)
  
  # Calculate pooled rates with combined grouping
  combined_results <- calculate_pooled_rate(
    data = data,
    by = all_grouping,
    method = method,
    measure = measure,
    min_studies = min_studies
  )
  
  if (is.null(combined_results)) {
    warning("No valid subgroups for analysis")
    return(NULL)
  }
  
  # Also calculate pooled rates without subgrouping for comparison
  if (!is.null(by)) {
    main_results <- calculate_pooled_rate(
      data = data,
      by = by,
      method = method,
      measure = measure,
      min_studies = min_studies
    )
  } else {
    main_results <- calculate_pooled_rate(
      data = data,
      method = method,
      measure = measure,
      min_studies = min_studies
    )
  }
  
  # Organize results
  result <- list(
    by = by,
    subgroups = subgroups,
    method = method,
    measure = measure,
    main_results = main_results,
    subgroup_results = combined_results
  )
  
  class(result) <- c("amr_subgroup_analysis", "list")
  return(result)
}

#' Print method for AMR subgroup analysis
#'
#' @param x An amr_subgroup_analysis object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the subgroup analysis object
#' @export
print.amr_subgroup_analysis <- function(x, ...) {
  cat("AMR Subgroup Analysis\n")
  cat("====================\n\n")
  
  cat("Method:", x$method, "effects using", x$measure, "transformation\n\n")
  
  cat("Main Grouping:", if (!is.null(x$by)) paste(x$by, collapse = ", ") else "None", "\n")
  cat("Subgroup Variables:", paste(x$subgroups, collapse = ", "), "\n\n")
  
  # Print main results
  cat("Main Results (without subgrouping):\n")
  summary_df <- x$main_results$summary
  summary_df$pooled_rate <- sprintf("%.4f", summary_df$pooled_rate)
  summary_df$ci <- sprintf("(%.4f-%.4f)", summary_df$ci_lower, summary_df$ci_upper)
  
  main_cols <- c(x$by, "studies", "total_samples", "pooled_rate", "ci", "i_squared")
  main_cols <- main_cols[main_cols %in% names(summary_df)]
  
  print_df <- summary_df[, main_cols]
  names(print_df) <- gsub("studies", "Studies", names(print_df))
  names(print_df) <- gsub("total_samples", "Samples", names(print_df))
  names(print_df) <- gsub("pooled_rate", "Rate", names(print_df))
  names(print_df) <- gsub("ci", "95% CI", names(print_df))
  names(print_df) <- gsub("i_squared", "I²", names(print_df))
  
  print(print_df, row.names = FALSE)
  
  # Print subgroup results
  cat("\nSubgroup Results:\n")
  subgroup_df <- x$subgroup_results$summary
  subgroup_df$pooled_rate <- sprintf("%.4f", subgroup_df$pooled_rate)
  subgroup_df$ci <- sprintf("(%.4f-%.4f)", subgroup_df$ci_lower, subgroup_df$ci_upper)
  subgroup_df$i_squared <- sprintf("%.1f%%", subgroup_df$i_squared)
  
  # Select and rename columns for printing
  sub_cols <- c(x$by, x$subgroups, "studies", "total_samples", "pooled_rate", "ci", "i_squared")
  sub_cols <- sub_cols[sub_cols %in% names(subgroup_df)]
  
  print_df <- subgroup_df[, sub_cols]
  names(print_df) <- gsub("studies", "Studies", names(print_df))
  names(print_df) <- gsub("total_samples", "Samples", names(print_df))
  names(print_df) <- gsub("pooled_rate", "Rate", names(print_df))
  names(print_df) <- gsub("ci", "95% CI", names(print_df))
  names(print_df) <- gsub("i_squared", "I²", names(print_df))
  
  print(print_df, row.names = FALSE)
  
  invisible(x)
} 