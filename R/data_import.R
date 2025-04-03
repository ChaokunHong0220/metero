#' Import AMR data from various file formats or directly from a data frame
#'
#' Imports antimicrobial resistance data from CSV, Excel, RData files, or directly from an existing data frame,
#' and provides options for initial validation and standardization.
#'
#' @param file_path Path to the data file, or NULL if providing a data frame directly
#' @param data Optional data frame containing AMR data (if providing data directly)
#' @param file_type Type of file: "csv", "excel", "rdata", or NULL for auto-detect
#' @param sheet Sheet name or number (for Excel files)
#' @param mapping A named list mapping source column names to standard names
#' @param domain Domain of the data: "human", "animal", "environment"
#' @param header Logical; whether the file has a header row (for CSV)
#' @param skip Number of rows to skip before the header (for CSV and Excel)
#' @param validate Logical; whether to validate the imported data
#' @param ... Additional parameters passed to read functions
#'
#' @return A data frame containing the imported data
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Import CSV data with automatic standardization
#' mapping <- list(
#'   study_id = "Study_ID",
#'   pathogen = "Bacteria",
#'   specific_antibiotic = "Antibiotic",
#'   resistance_rate = "Resistance_Rate"
#' )
#' 
#' human_data <- import_amr_data(
#'   file_path = "my_amr_data.csv",
#'   mapping = mapping,
#'   domain = "human"
#' )
#'
#' # Example 2: Directly use an existing data frame
#' my_df <- data.frame(
#'   Study_ID = c("Study1", "Study1", "Study2"),
#'   Bacteria = c("E. coli", "S. aureus", "K. pneumoniae"),
#'   Antibiotic = c("Ciprofloxacin", "Oxacillin", "Meropenem"),
#'   Total = c(100, 50, 75),
#'   Resistant = c(34, 22, 12)
#' )
#'
#' processed_data <- import_amr_data(
#'   data = my_df,
#'   mapping = mapping,
#'   domain = "human"
#' )
#' }
import_amr_data <- function(file_path = NULL, data = NULL, file_type = NULL, sheet = 1, 
                            mapping = NULL, domain = c("human", "animal", "environment"),
                            header = TRUE, skip = 0, validate = TRUE, ...) {
  # Check if either file_path or data is provided
  if (is.null(file_path) && is.null(data)) {
    stop("Either 'file_path' or 'data' must be provided")
  }
  
  # Match domain argument
  domain <- match.arg(domain)
  
  # If data is directly provided, use it; otherwise import from file
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("If 'data' is provided, it must be a data frame")
    }
    # Use the provided data frame
    imported_data <- data
  } else {
    # Check file exists
    if (!file.exists(file_path)) {
      stop("File not found: ", file_path)
    }
    
    # Auto-detect file type if not specified
    if (is.null(file_type)) {
      file_ext <- tolower(tools::file_ext(file_path))
      if (file_ext %in% c("csv", "txt")) {
        file_type <- "csv"
      } else if (file_ext %in% c("xls", "xlsx")) {
        file_type <- "excel"
      } else if (file_ext == "rdata" || file_ext == "rda") {
        file_type <- "rdata"
      } else {
        stop("Could not automatically determine file type. Please specify file_type.")
      }
    }
    
    # Import data based on file type
    imported_data <- switch(tolower(file_type),
                   "csv" = utils::read.csv(file_path, header = header, skip = skip, ...),
                   "excel" = {
                     if (!requireNamespace("readxl", quietly = TRUE)) {
                       stop("Package 'readxl' is required to import Excel files.")
                     }
                     readxl::read_excel(file_path, sheet = sheet, skip = skip, ...)
                   },
                   "rdata" = {
                     env <- new.env()
                     load(file_path, envir = env)
                     
                     # If there's only one object in the environment, return it
                     if (length(ls(env)) == 1) {
                       get(ls(env)[1], envir = env)
                     } else {
                       # If there are multiple objects, try to find a data frame
                       data_frames <- ls(env)[sapply(ls(env), function(x) is.data.frame(get(x, envir = env)))]
                       if (length(data_frames) == 1) {
                         get(data_frames[1], envir = env)
                       } else if (length(data_frames) > 1) {
                         stop("Multiple data frames found in RData file. Please extract the desired data frame manually.")
                       } else {
                         stop("No data frames found in RData file.")
                       }
                     }
                   },
                   stop("Unsupported file type: ", file_type)
    )
  }
  
  # Ensure data is a data.frame (not a tibble or other class)
  if (!is.data.frame(imported_data)) {
    imported_data <- as.data.frame(imported_data)
  }
  
  # Apply mapping and standardization if provided
  if (!is.null(mapping)) {
    # Get data model for domain validation
    model <- get_data_model()
    domain_schema <- switch(domain,
                           "human" = model$human_schema,
                           "animal" = model$animal_schema,
                           "environment" = model$environment_schema)
    
    # Use the enhanced standardize_amr_data function from the current file instead of
    # potentially using the basic one from data_standardization.R
    imported_data <- metero:::standardize_amr_data(
      data = imported_data, 
      mapping = mapping, 
      validate = validate,
      domain = domain, 
      domain_schema = domain_schema
    )
  }
  
  return(imported_data)
}

#' Import local AMR data and provide quality assessment
#'
#' Imports local AMR data (either from a file or a direct data frame) and performs 
#' basic quality checks, providing a report on data completeness and potential issues.
#'
#' @param file_path Path to the data file, or NULL if providing a data frame directly
#' @param data Optional data frame containing AMR data (if providing data directly)
#' @param file_type Type of file: "csv", "excel", "rdata", or NULL for auto-detect
#' @param mapping A named list mapping source column names to standard names
#' @param domain Domain of the data: "human", "animal", "environment"
#' @param ... Additional parameters passed to import_amr_data
#'
#' @return A list containing the imported data and a quality assessment report
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Import and assess local file data quality
#' mapping <- list(
#'   study_id = "Study_ID",
#'   pathogen = "Bacteria",
#'   specific_antibiotic = "Antibiotic",
#'   resistance_rate = "Resistance_Rate"
#' )
#' 
#' result <- import_local_data(
#'   file_path = "local_hospital_data.csv",
#'   mapping = mapping,
#'   domain = "human"
#' )
#' 
#' # View quality assessment
#' print(result$quality_assessment)
#' 
#' # Example 2: Assessment with direct data input
#' my_df <- data.frame(
#'   Study_ID = c("Study1", "Study1", "Study2"),
#'   Bacteria = c("E. coli", "S. aureus", "K. pneumoniae"),
#'   Antibiotic = c("Ciprofloxacin", "Oxacillin", "Meropenem"),
#'   Total = c(100, 50, 75),
#'   Resistant = c(34, 22, 12)
#' )
#'
#' result <- import_local_data(
#'   data = my_df,
#'   mapping = mapping,
#'   domain = "human"
#' )
#' }
import_local_data <- function(file_path = NULL, data = NULL, file_type = NULL, mapping = NULL, 
                             domain = c("human", "animal", "environment"), ...) {
  # Match domain argument
  domain <- match.arg(domain)
  
  # Import data
  imported_data <- import_amr_data(file_path = file_path, data = data, 
                                 file_type = file_type, mapping = mapping, 
                                 domain = domain, ...)
  
  # Perform quality assessment
  quality_assessment <- check_data_quality(imported_data, domain = domain)
  
  # Return both data and quality assessment
  return(list(
    data = imported_data,
    quality_assessment = quality_assessment
  ))
}

#' Check data quality of AMR dataset
#'
#' Evaluates the quality of AMR data by checking for completeness,
#' consistency, and potential issues.
#'
#' @param data A data frame containing AMR data
#' @param domain Domain of the data: "human", "animal", "environment"
#'
#' @return A list containing quality assessment results
#' @export
#'
#' @examples
#' \dontrun{
#' # Check quality of imported data
#' quality_report <- check_data_quality(amr_data, domain = "human")
#' print(quality_report)
#' }
check_data_quality <- function(data, domain = c("human", "animal", "environment")) {
  # Match domain argument
  domain <- match.arg(domain)
  
  # Get data model for domain
  model <- get_data_model()
  
  # Get schema based on domain
  schema <- switch(domain,
                  "human" = model$human_schema,
                  "animal" = model$animal_schema,
                  "environment" = model$environment_schema)
  
  # Prepare results structure
  assessment <- list(
    completeness = NULL,
    consistency = NULL,
    issues = NULL,
    recommendations = NULL
  )
  
  # Check completeness
  required_fields <- schema$field_name[schema$required]
  present_fields <- required_fields[required_fields %in% names(data)]
  missing_fields <- setdiff(required_fields, present_fields)
  
  completeness <- data.frame(
    field = schema$field_name,
    required = schema$required,
    present = schema$field_name %in% names(data),
    stringsAsFactors = FALSE
  )
  
  completeness$missing_ratio <- sapply(schema$field_name, function(field) {
    if (field %in% names(data)) {
      sum(is.na(data[[field]])) / nrow(data)
    } else {
      1.0
    }
  })
  
  assessment$completeness <- completeness
  
  # Check consistency
  # Check for numeric fields with impossible values
  consistency_issues <- character()
  
  numeric_fields <- schema$field_name[schema$data_type %in% c("numeric", "integer")]
  for (field in numeric_fields) {
    if (field %in% names(data)) {
      # Special case for resistance rates: should be between 0 and 1
      if (field %in% c("resistance_rate", "detection_rate")) {
        invalid_values <- sum(data[[field]] < 0 | data[[field]] > 1, na.rm = TRUE)
        if (invalid_values > 0) {
          consistency_issues <- c(consistency_issues, 
                                 sprintf("%s: %d values outside valid range [0,1]", 
                                         field, invalid_values))
        }
      }
      # Check for negative values in count fields
      else if (grepl("count$|size$", field)) {
        invalid_values <- sum(data[[field]] < 0, na.rm = TRUE)
        if (invalid_values > 0) {
          consistency_issues <- c(consistency_issues, 
                                 sprintf("%s: %d negative values", 
                                         field, invalid_values))
        }
      }
    }
  }
  
  # Check for duplicates
  if ("record_id" %in% names(data)) {
    duplicates <- sum(duplicated(data$record_id))
    if (duplicates > 0) {
      consistency_issues <- c(consistency_issues, 
                             sprintf("Duplicate record_id values: %d", duplicates))
    }
  }
  
  assessment$consistency <- consistency_issues
  
  # Identify other issues
  issues <- character()
  
  # Check if resistance counts exceed sample counts
  if (all(c("sample_count", "resistant_count") %in% names(data))) {
    invalid_rows <- sum(data$resistant_count > data$sample_count, na.rm = TRUE)
    if (invalid_rows > 0) {
      issues <- c(issues, 
                 sprintf("Invalid resistant counts: %d rows have resistant_count > sample_count", 
                         invalid_rows))
    }
  }
  
  assessment$issues <- issues
  
  # Generate recommendations
  recommendations <- character()
  
  # Recommend adding missing required fields
  if (length(missing_fields) > 0) {
    recommendations <- c(recommendations, 
                        sprintf("Add missing required fields: %s", 
                                paste(missing_fields, collapse = ", ")))
  }
  
  # Recommend correcting data issues
  if (length(consistency_issues) > 0 || length(issues) > 0) {
    recommendations <- c(recommendations, 
                        "Correct the identified data issues for improved analysis accuracy")
  }
  
  # Recommend standardizing categorical variables
  if ("setting" %in% names(data)) {
    unique_settings <- unique(data$setting)
    if (length(unique_settings) > 5) {
      recommendations <- c(recommendations, 
                          "Consider standardizing 'setting' values to reduce variability")
    }
  }
  
  assessment$recommendations <- recommendations
  
  # Overall quality score (simple version)
  completeness_score <- sum(completeness$present & completeness$required) / sum(completeness$required)
  consistency_score <- 1 - min(1, length(consistency_issues) / 5) # Penalize for up to 5 issues
  issues_score <- 1 - min(1, length(issues) / 3) # Penalize for up to 3 issues
  
  assessment$quality_score <- (completeness_score * 0.5) + (consistency_score * 0.3) + (issues_score * 0.2)
  
  class(assessment) <- c("amr_quality_assessment", "list")
  return(assessment)
}

#' Print method for AMR quality assessment
#'
#' @param x An amr_quality_assessment object
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the assessment object
#' @export
print.amr_quality_assessment <- function(x, ...) {
  cat("AMR Data Quality Assessment\n")
  cat("==========================\n\n")
  
  cat("Overall Quality Score:", round(x$quality_score * 100), "/ 100\n\n")
  
  cat("Completeness of Required Fields:\n")
  required_fields <- x$completeness$field[x$completeness$required]
  required_present <- x$completeness$present[x$completeness$required]
  
  for (i in seq_along(required_fields)) {
    status <- if (required_present[i]) "[OK]" else "[MISSING]"
    cat(sprintf("  %s %s\n", status, required_fields[i]))
  }
  
  cat("\nData Issues:\n")
  if (length(x$consistency) == 0 && length(x$issues) == 0) {
    cat("  No major issues detected\n")
  } else {
    if (length(x$consistency) > 0) {
      cat("  Consistency issues:\n")
      for (issue in x$consistency) {
        cat(sprintf("    - %s\n", issue))
      }
    }
    
    if (length(x$issues) > 0) {
      cat("  Other issues:\n")
      for (issue in x$issues) {
        cat(sprintf("    - %s\n", issue))
      }
    }
  }
  
  cat("\nRecommendations:\n")
  if (length(x$recommendations) == 0) {
    cat("  No specific recommendations\n")
  } else {
    for (rec in x$recommendations) {
      cat(sprintf("  - %s\n", rec))
    }
  }
  
  invisible(x)
}

#' Enhanced standardize AMR data format (internal function)
#'
#' This function extends the basic standardize_amr_data function with additional 
#' validation and processing capabilities, including domain-specific standardization.
#' This is an internal function used by import_amr_data.
#'
#' @param data A data frame containing AMR data
#' @param mapping A named list mapping user's column names to standard names
#' @param validate Logical; whether to validate the data structure (default: TRUE)
#' @param domain Domain of the data: "human", "animal", "environment"
#' @param domain_schema Optional domain-specific schema
#' @param standardize_categorical Logical; whether to standardize categorical variables
#'
#' @return A data frame with standardized variable names and structure
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Create a mapping from your data to the standard format
#' my_mapping <- list(
#'   study_id = "Study_ID",
#'   pathogen = "Bacteria", 
#'   antibiotic = "Drug"
#' )
#' 
#' # This function is used internally by import_amr_data
#' std_data <- import_amr_data(
#'   my_data, 
#'   my_mapping, 
#'   domain = "human",
#'   standardize_categorical = TRUE
#' )
#' }
standardize_amr_data <- function(data, mapping, validate = TRUE,
                                domain = c("human", "animal", "environment"),
                                domain_schema = NULL, 
                                standardize_categorical = FALSE) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Match domain argument
  domain <- match.arg(domain)
  
  # Get schema if not provided
  if (is.null(domain_schema)) {
    model <- get_data_model()
    domain_schema <- switch(domain,
                           "human" = model$human_schema,
                           "animal" = model$animal_schema,
                           "environment" = model$environment_schema)
  }
  
  # Create a new data frame with standardized column names
  result <- data.frame(row.names = 1:nrow(data))
  
  # Extract the standard column names from schema
  std_columns <- domain_schema$field_name
  
  # Reverse the mapping to go from user columns to standard columns
  rev_mapping <- list()
  for (std_name in names(mapping)) {
    rev_mapping[[mapping[[std_name]]]] <- std_name
  }
  
  # Apply mapping and copy data
  for (std_name in std_columns) {
    user_name <- NULL
    
    # Try different ways to find the corresponding user column
    if (std_name %in% names(rev_mapping)) {
      user_name <- rev_mapping[[std_name]]
    } else if (std_name %in% names(data)) {
      user_name <- std_name
    }
    
    if (!is.null(user_name) && user_name %in% names(data)) {
      result[[std_name]] <- data[[user_name]]
    } else {
      # For missing columns, add NA
      result[[std_name]] <- NA
    }
  }
  
  # Add domain indicator
  result$domain <- domain
  
  # Validation
  if (validate) {
    # Check required fields based on schema
    required_fields <- domain_schema$field_name[domain_schema$required]
    missing_fields <- required_fields[!(required_fields %in% names(result) &
                                          !all(is.na(result[required_fields])))]
    
    if (length(missing_fields) > 0) {
      warning("Missing required fields: ", paste(missing_fields, collapse = ", "))
    }
    
    # Calculate resistance_rate/detection_rate if missing but counts are available
    if (domain %in% c("human", "animal")) {
      rate_field <- "resistance_rate"
    } else {
      rate_field <- "detection_rate"
    }
    
    if (all(is.na(result[[rate_field]])) &&
        "sample_count" %in% names(result) &&
        !all(is.na(result$sample_count)) &&
        ((domain %in% c("human", "animal") && "resistant_count" %in% names(result) && !all(is.na(result$resistant_count))) ||
         (domain == "environment" && "detected_count" %in% names(result) && !all(is.na(result$detected_count))))) {
      
      count_field <- if (domain %in% c("human", "animal")) "resistant_count" else "detected_count"
      result[[rate_field]] <- result[[count_field]] / result$sample_count
    }
    
    # Validate data types
    for (i in 1:nrow(domain_schema)) {
      field <- domain_schema$field_name[i]
      data_type <- domain_schema$data_type[i]
      
      if (field %in% names(result) && !all(is.na(result[[field]]))) {
        # Try to convert to correct data type if needed
        if (data_type == "numeric" && !is.numeric(result[[field]])) {
          result[[field]] <- as.numeric(result[[field]])
        } else if (data_type == "integer" && !is.integer(result[[field]])) {
          result[[field]] <- as.integer(result[[field]])
        } else if (data_type == "logical" && !is.logical(result[[field]])) {
          result[[field]] <- as.logical(result[[field]])
        } else if (data_type == "Date" && !inherits(result[[field]], "Date")) {
          # Try to convert to date
          tryCatch({
            result[[field]] <- as.Date(result[[field]])
          }, error = function(e) {
            warning("Could not convert ", field, " to Date type")
          })
        }
      }
    }
  }
  
  # Standardize categorical variables if requested
  if (standardize_categorical) {
    # Standardize setting
    if ("setting" %in% names(result) && !all(is.na(result$setting))) {
      result$setting <- toupper(result$setting)
      result$setting <- gsub("\\s+", "_", result$setting)
      result$setting <- gsub("HOSPITAL.*", "HOSPITAL", result$setting)
      result$setting <- gsub("RURAL.*", "RURAL", result$setting)
      result$setting <- gsub("URBAN.*", "URBAN", result$setting)
    }
    
    # Standardize population_type
    if ("population_type" %in% names(result) && !all(is.na(result$population_type))) {
      result$population_type <- toupper(result$population_type)
      result$population_type <- gsub("\\s+", "_", result$population_type)
      result$population_type <- gsub("CHILDREN.*|CHILD.*|PEDIATRIC.*", "CHILDREN", result$population_type)
      result$population_type <- gsub("ADULT.*", "ADULTS", result$population_type)
      result$population_type <- gsub("ALL.*|MIXED.*", "ALL_AGES", result$population_type)
    }
    
    # Domain-specific standardization
    if (domain == "animal" && "animal_type" %in% names(result) && !all(is.na(result$animal_type))) {
      result$animal_type <- toupper(result$animal_type)
      result$animal_type <- gsub("\\s+", "_", result$animal_type)
      result$animal_type <- gsub("CATTLE.*|COW.*|BOVINE.*", "CATTLE", result$animal_type)
      result$animal_type <- gsub("CHICKEN.*|POULTRY.*", "POULTRY", result$animal_type)
      result$animal_type <- gsub("PIG.*|SWINE.*|HOG.*", "SWINE", result$animal_type)
    }
    
    if (domain == "environment" && "sample_type" %in% names(result) && !all(is.na(result$sample_type))) {
      result$sample_type <- toupper(result$sample_type)
      result$sample_type <- gsub("\\s+", "_", result$sample_type)
      result$sample_type <- gsub("WATER.*|AQUATIC.*", "WATER", result$sample_type)
      result$sample_type <- gsub("SOIL.*|GROUND.*", "SOIL", result$sample_type)
      result$sample_type <- gsub("SEWAGE.*|WASTE.*", "SEWAGE", result$sample_type)
    }
  }
  
  return(result)
}

#' Connect to external AMR database
#'
#' Establishes a connection to an external AMR database using
#' provided credentials and configuration.
#'
#' @param db_type Type of database: "glass", "atlas", "ear", or other
#' @param credentials List containing authentication credentials
#' @param config Additional configuration parameters
#'
#' @return A connection object to the external database
#' @export
#'
#' @examples
#' \dontrun{
#' # Connect to WHO GLASS database
#' glass_credentials <- list(
#'   username = "myuser",
#'   password = "mypass",
#'   api_key = "my-api-key"
#' )
#' 
#' glass_conn <- connect_to_database(
#'   db_type = "glass",
#'   credentials = glass_credentials
#' )
#' }
connect_to_database <- function(db_type = c("glass", "atlas", "ear", "other"),
                                credentials = list(), config = list()) {
  # Match db_type argument
  db_type <- match.arg(db_type)
  
  # Check for required packages
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for database connections")
  }
  
  # Setup connection based on database type
  conn <- switch(db_type,
                "glass" = {
                  # WHO GLASS database connection
                  if (!requireNamespace("jsonlite", quietly = TRUE)) {
                    stop("Package 'jsonlite' is required for WHO GLASS connection")
                  }
                  
                  # This is a placeholder for actual WHO GLASS API connection
                  # In reality, this would be implemented based on their API documentation
                  message("Connecting to WHO GLASS database...")
                  
                  # Create connection object (this is a dummy implementation)
                  list(
                    type = "glass",
                    connected = TRUE,
                    credentials = credentials,
                    config = config,
                    fetch = function(query) {
                      # Actual implementation would fetch data based on query
                      message("Fetching data from WHO GLASS...")
                      # Return empty data frame for now
                      data.frame()
                    },
                    close = function() {
                      message("Closing WHO GLASS connection")
                    }
                  )
                },
                "atlas" = {
                  # ATLAS database connection (placeholder)
                  message("Connecting to ATLAS database...")
                  
                  # Create connection object
                  list(
                    type = "atlas",
                    connected = TRUE,
                    credentials = credentials,
                    config = config,
                    fetch = function(query) {
                      message("Fetching data from ATLAS...")
                      data.frame()
                    },
                    close = function() {
                      message("Closing ATLAS connection")
                    }
                  )
                },
                "ear" = {
                  # EAR network connection (placeholder)
                  message("Connecting to EAR network...")
                  
                  list(
                    type = "ear",
                    connected = TRUE,
                    credentials = credentials,
                    config = config,
                    fetch = function(query) {
                      message("Fetching data from EAR network...")
                      data.frame()
                    },
                    close = function() {
                      message("Closing EAR connection")
                    }
                  )
                },
                "other" = {
                  # Generic database connection
                  if (!("url" %in% names(config))) {
                    stop("'url' must be provided in config for 'other' database type")
                  }
                  
                  message("Connecting to external database...")
                  
                  list(
                    type = "other",
                    connected = TRUE,
                    credentials = credentials,
                    config = config,
                    fetch = function(query) {
                      message("Fetching data from external database...")
                      data.frame()
                    },
                    close = function() {
                      message("Closing external database connection")
                    }
                  )
                })
  
  class(conn) <- c("amr_db_connection", "list")
  return(conn)
}

#' Improve data quality through imputation and handling of missing values
#'
#' Applies various methods to handle missing data in AMR datasets,
#' including imputation techniques appropriate for AMR data.
#'
#' @param data A data frame containing AMR data
#' @param method Imputation method: "mean", "median", "knn", "mice" or "none"
#' @param fields Character vector of field names to impute, or NULL for all applicable fields
#' @param k Number of neighbors for KNN imputation
#' @param seed Random seed for reproducibility
#'
#' @return A data frame with imputed values for missing data
#' @export
#'
#' @examples
#' \dontrun{
#' # Impute missing resistance rates using median imputation
#' imputed_data <- impute_missing_data(
#'   amr_data,
#'   method = "median",
#'   fields = c("resistance_rate")
#' )
#' }
impute_missing_data <- function(data, method = c("mean", "median", "knn", "mice", "none"),
                               fields = NULL, k = 5, seed = 123) {
  # Match method argument
  method <- match.arg(method)
  
  # If no imputation requested, return original data
  if (method == "none") {
    return(data)
  }
  
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Identify numeric fields if fields parameter is NULL
  if (is.null(fields)) {
    numeric_fields <- names(data)[sapply(data, is.numeric)]
    # Exclude ID fields and other fields that should not be imputed
    exclude_pattern <- "^(id|record_id|study_id)$"
    fields <- numeric_fields[!grepl(exclude_pattern, numeric_fields)]
  } else {
    # Verify that specified fields exist in the data
    missing_fields <- fields[!fields %in% names(data)]
    if (length(missing_fields) > 0) {
      stop("The following specified fields do not exist in the data: ",
           paste(missing_fields, collapse = ", "))
    }
  }
  
  # Check if there's anything to impute
  if (length(fields) == 0) {
    warning("No applicable fields for imputation")
    return(data)
  }
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Apply imputation based on method
  result <- switch(method,
                  "mean" = {
                    for (field in fields) {
                      if (is.numeric(data[[field]])) {
                        field_mean <- mean(data[[field]], na.rm = TRUE)
                        data[[field]][is.na(data[[field]])] <- field_mean
                      }
                    }
                    data
                  },
                  "median" = {
                    for (field in fields) {
                      if (is.numeric(data[[field]])) {
                        field_median <- stats::median(data[[field]], na.rm = TRUE)
                        data[[field]][is.na(data[[field]])] <- field_median
                      }
                    }
                    data
                  },
                  "knn" = {
                    if (!requireNamespace("DMwR2", quietly = TRUE)) {
                      stop("Package 'DMwR2' is required for KNN imputation")
                    }
                    
                    # Extract numeric columns for KNN imputation
                    numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]
                    
                    # Check if there are enough numeric columns for KNN
                    if (ncol(numeric_data) < 2) {
                      stop("KNN imputation requires at least 2 numeric columns")
                    }
                    
                    # Apply KNN imputation to numeric columns with missing values
                    imputed_numeric <- DMwR2::knnImputation(numeric_data, k = k)
                    
                    # Update original data with imputed values
                    data[, names(imputed_numeric)] <- imputed_numeric
                    data
                  },
                  "mice" = {
                    if (!requireNamespace("mice", quietly = TRUE)) {
                      stop("Package 'mice' is required for MICE imputation")
                    }
                    
                    # Create a subset of data for imputation
                    impute_cols <- unique(c(fields, 
                                           "pathogen", "specific_antibiotic", 
                                           "sample_count", "resistant_count"))
                    impute_cols <- impute_cols[impute_cols %in% names(data)]
                    
                    # Check if there are enough columns for imputation
                    if (length(impute_cols) < 2) {
                      stop("MICE imputation requires at least 2 columns")
                    }
                    
                    # Perform imputation
                    imp <- mice::mice(data[, impute_cols, drop = FALSE], 
                                     m = 5, maxit = 20, printFlag = FALSE)
                    imputed_data <- mice::complete(imp)
                    
                    # Update original data with imputed values
                    data[, names(imputed_data)] <- imputed_data
                    data
                  },
                  # Default case should not happen due to match.arg
                  stop("Unsupported imputation method")
  )
  
  return(result)
} 