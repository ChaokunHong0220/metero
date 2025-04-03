#' Standardize AMR data format
#'
#' This function converts user-provided AMR data to the standard format used by metero.
#' It performs validation and standardization of variable names and data types.
#'
#' @param data A data frame containing AMR data
#' @param mapping A named list mapping user's column names to standard names
#' @param validate Logical; whether to validate the data structure (default: TRUE)
#'
#' @return A data frame with standardized variable names and structure
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a mapping from your data to the standard format
#' my_mapping <- list(
#'   "Study ID" = "study_id",
#'   "Bacteria" = "pathogen_name",
#'   "Drug" = "antibiotic_name"
#' )
#'
#' # Standardize your data
#' std_data <- standardize_amr_data(my_data, my_mapping)
#' }
standardize_amr_data <- function(data, mapping, validate = TRUE) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Create a new data frame with standardized column names
  result <- data.frame(row.names = 1:nrow(data))

  # Standard column names
  std_columns <- c(
    "study_id", "pathogen_name", "antibiotic_name",
    "sample_count", "resistant_count", "resistance_rate",
    "domain", "region", "author", "year", "country",
    "study_design", "setting", "population_type",
    "total_sample_size", "age_range"
  )

  # Apply mapping and copy data
  for (std_name in std_columns) {
    if (std_name %in% names(mapping) && mapping[[std_name]] %in% names(data)) {
      result[[std_name]] <- data[[mapping[[std_name]]]]
    } else if (std_name %in% names(data)) {
      result[[std_name]] <- data[[std_name]]
    } else {
      # For missing columns, add NA
      result[[std_name]] <- NA
    }
  }

  # Validation
  if (validate) {
    # Check required fields
    required_fields <- c("study_id", "pathogen_name", "antibiotic_name",
                         "sample_count", "resistant_count")
    missing_fields <- required_fields[!(required_fields %in% names(result) &
                                          !all(is.na(result[required_fields])))]

    if (length(missing_fields) > 0) {
      warning("Missing required fields: ", paste(missing_fields, collapse = ", "))
    }

    # Calculate resistance_rate if missing but sample and resistant counts are available
    if (all(is.na(result$resistance_rate)) &&
        !all(is.na(result$sample_count)) &&
        !all(is.na(result$resistant_count))) {
      result$resistance_rate <- result$resistant_count / result$sample_count
    }
  }

  return(result)
}

#' Get standard AMR data structure
#'
#' Returns the standard variable names and descriptions for AMR data in metero
#'
#' @return A data frame with variable names and descriptions
#' @export
get_standard_amr_structure <- function() {
  structure <- data.frame(
    variable_name = c(
      "study_id", "pathogen_name", "antibiotic_name",
      "sample_count", "resistant_count", "resistance_rate",
      "domain", "region", "author", "year", "country",
      "study_design", "setting", "population_type",
      "total_sample_size", "age_range"
    ),
    description = c(
      "Unique identifier for each study",
      "Full name of the pathogen",
      "Full name of the antibiotic",
      "Number of samples tested",
      "Number of resistant isolates",
      "Proportion of resistant isolates (0-1)",
      "Domain (human, animal, or environment)",
      "Geographical region",
      "First author of the study",
      "Year of publication",
      "Country where the study was conducted",
      "Type of study design",
      "Environment setting (e.g., rural, urban, hospital)",
      "Study population (e.g., adults, children)",
      "Total sample size of the study",
      "Age range of study participants"
    ),
    required = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE
    ),
    stringsAsFactors = FALSE
  )

  return(structure)
}
