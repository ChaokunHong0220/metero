#' Basic standardize AMR data format
#'
#' Basic version of standardization for AMR data. For full functionality,
#' use the enhanced standardize_amr_data function in data_import.R.
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
#' std_data <- basic_standardize_amr_data(my_data, my_mapping)
#' }
basic_standardize_amr_data <- function(data, mapping, validate = TRUE) {
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

#' Antibiotic code to full name conversion
#'
#' Converts antibiotic codes/abbreviations to their full names and provides
#' classifications according to WHO or other standards.
#'
#' @param code Character vector of antibiotic codes or abbreviations
#' @param return_type Return type: "name" for full names, "class" for antibiotic class,
#'                    "who_class" for WHO classification, or "all" for all information
#' @param case Character case for returned names: "original", "lower", or "upper"
#'
#' @return A character vector, data frame, or list depending on return_type
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert abbreviations to full names
#' full_names <- antibiotic_code_to_name(c("AMP", "CIP", "MEM"))
#' 
#' # Get WHO classification
#' who_classes <- antibiotic_code_to_name(c("AMP", "CIP", "MEM"), return_type = "who_class")
#' }
antibiotic_code_to_name <- function(code, return_type = c("name", "class", "who_class", "all"),
                                   case = c("original", "lower", "upper")) {
  # Match arguments
  return_type <- match.arg(return_type)
  case <- match.arg(case)
  
  # Define antibiotic information
  antibiotic_data <- data.frame(
    code = c("AML", "AMP", "AMK", "AZM", "ATM", "CFZ", "FEP", "CFM", "CTX", "FOX", 
             "CAZ", "CRO", "LEX", "CHL", "CIP", "CLI", "CST", "SXT", "DOX", "ERY", 
             "GEN", "IPM", "KAN", "LVX", "MEM", "NAL", "NIT", "OFX", "OXA", "PEN", 
             "STR", "SOX", "TET", "TGC", "VAN"),
    name = c("Amoxyclav", "Ampicillin", "Amikacin", "Azithromycin", "Aztreonam", 
             "Cefazoline", "Cefepime", "Cefixime", "Cefotaxime", "Cefoxitin", 
             "Ceftazidime", "Ceftriaxone", "Cephalexin", "Chloramphenicol", "Ciprofloxacin", 
             "Clindamycin", "Colistin", "Cotrimoxazole", "Doxycyline", "Erythromycin", 
             "Gentamicin", "Imipenem", "Kanamycin", "Levofloxacin", "Meropenem", 
             "Nalidixic Acid", "Nitrofurantoin", "Ofloxacin", "Oxacillin", "Penicillin", 
             "Streptomycin", "Sulphafurazole", "Tetracycline", "Tigecycline", "Vancomycin"),
    class = c("Penicillin Combination", "Penicillin", "Aminoglycoside", "Macrolide", "Monobactam", 
              "Cephalosporin 1st", "Cephalosporin 4th", "Cephalosporin 3rd", "Cephalosporin 3rd", "Cephalosporin 2nd", 
              "Cephalosporin 3rd", "Cephalosporin 3rd", "Cephalosporin 1st", "Phenicol", "Fluoroquinolone", 
              "Lincosamide", "Polymyxin", "Sulfonamide/Trimethoprim", "Tetracycline", "Macrolide", 
              "Aminoglycoside", "Carbapenem", "Aminoglycoside", "Fluoroquinolone", "Carbapenem", 
              "Quinolone", "Nitrofuran", "Fluoroquinolone", "Penicillin", "Penicillin", 
              "Aminoglycoside", "Sulfonamide", "Tetracycline", "Glycylcycline", "Glycopeptide"),
    who_class = c("Access", "Access", "Access", "Watch", "Watch", 
                  "Access", "Watch", "Watch", "Watch", "Access", 
                  "Watch", "Watch", "Access", "Access", "Watch", 
                  "Access", "Reserve", "Access", "Access", "Watch", 
                  "Access", "Watch", "Access", "Watch", "Watch", 
                  "Access", "Access", "Watch", "Access", "Access", 
                  "Access", "Access", "Access", "Reserve", "Watch"),
    stringsAsFactors = FALSE
  )
  
  # Convert input to uppercase for matching
  code_upper <- toupper(code)
  
  # Match codes and return results
  result <- vector("list", length(code))
  
  for (i in seq_along(code)) {
    idx <- match(code_upper[i], antibiotic_data$code)
    
    if (is.na(idx)) {
      # If code not found, check if it's already a full name
      name_idx <- which(tolower(antibiotic_data$name) == tolower(code[i]))
      
      if (length(name_idx) > 0) {
        idx <- name_idx[1]
      } else {
        # Not found at all
        result[[i]] <- NA
        next
      }
    }
    
    # Apply case formatting
    name <- antibiotic_data$name[idx]
    if (case == "lower") {
      name <- tolower(name)
    } else if (case == "upper") {
      name <- toupper(name)
    }
    
    # Return based on return_type
    if (return_type == "name") {
      result[[i]] <- name
    } else if (return_type == "class") {
      result[[i]] <- antibiotic_data$class[idx]
    } else if (return_type == "who_class") {
      result[[i]] <- antibiotic_data$who_class[idx]
    } else {
      # Return all information
      result[[i]] <- list(
        code = antibiotic_data$code[idx],
        name = name,
        class = antibiotic_data$class[idx],
        who_class = antibiotic_data$who_class[idx]
      )
    }
  }
  
  # Format return value
  if (return_type == "all") {
    return(result)
  } else {
    return(unlist(result))
  }
}

#' Parse AMR data column names
#'
#' Parses column names in the format r_ANTIBIOTIC_PATHOGEN, n_ANTIBIOTIC_PATHOGEN,
#' or d_ANTIBIOTIC_PATHOGEN to extract antibiotic and pathogen information.
#'
#' @param column_names Character vector of column names to parse
#' @param expand_codes Logical; whether to expand antibiotic codes to full names
#' @param rate_prefix Prefix for rate columns (default: "r_")
#' @param total_prefix Prefix for total sample columns (default: "n_")
#' @param resistant_prefix Prefix for resistant count columns (default: "d_")
#'
#' @return A data frame with parsed information
#' @export
#'
#' @examples
#' \dontrun{
#' # Parse column names
#' cols <- c("r_AMP_Ecoli", "n_AMP_Ecoli", "d_AMP_Ecoli", 
#'           "r_CIP_Kpneumo", "n_CIP_Kpneumo", "d_CIP_Kpneumo")
#' parsed <- parse_amr_columns(cols, expand_codes = TRUE)
#' }
parse_amr_columns <- function(column_names, expand_codes = TRUE,
                             rate_prefix = "r_", total_prefix = "n_", resistant_prefix = "d_") {
  # Initialize results
  result <- data.frame(
    column = column_names,
    type = NA_character_,
    antibiotic_code = NA_character_,
    antibiotic_name = NA_character_,
    pathogen = NA_character_,
    stringsAsFactors = FALSE
  )
  
  # Parse columns
  for (i in seq_along(column_names)) {
    col <- column_names[i]
    
    # Determine type
    if (substr(col, 1, nchar(rate_prefix)) == rate_prefix) {
      result$type[i] <- "rate"
      parts <- strsplit(substr(col, nchar(rate_prefix) + 1, nchar(col)), "_")[[1]]
    } else if (substr(col, 1, nchar(total_prefix)) == total_prefix) {
      result$type[i] <- "total"
      parts <- strsplit(substr(col, nchar(total_prefix) + 1, nchar(col)), "_")[[1]]
    } else if (substr(col, 1, nchar(resistant_prefix)) == resistant_prefix) {
      result$type[i] <- "resistant"
      parts <- strsplit(substr(col, nchar(resistant_prefix) + 1, nchar(col)), "_")[[1]]
    } else {
      next  # Skip non-matching columns
    }
    
    # Extract antibiotic code and pathogen
    if (length(parts) >= 2) {
      result$antibiotic_code[i] <- parts[1]
      result$pathogen[i] <- paste(parts[-1], collapse = "_")
      
      # Expand antibiotic codes if requested
      if (expand_codes && !is.na(result$antibiotic_code[i])) {
        result$antibiotic_name[i] <- antibiotic_code_to_name(result$antibiotic_code[i])
      }
    }
  }
  
  return(result)
}

#' Convert AMR data from wide to long format
#'
#' Converts AMR data from wide format with columns like r_AMP_Ecoli to long format
#' with separate rows for each antibiotic and pathogen combination.
#'
#' @param data A data frame in wide format
#' @param expand_codes Logical; whether to expand antibiotic codes to full names
#' @param add_who_class Logical; whether to add WHO AWaRe classification
#' @param meta_columns Character vector of metadata column names to include
#'
#' @return A data frame in long format
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert wide format data to long format
#' long_data <- amr_wide_to_long(
#'   my_df, 
#'   expand_codes = TRUE,
#'   add_who_class = TRUE,
#'   meta_columns = c("year", "region")
#' )
#' }
amr_wide_to_long <- function(data, expand_codes = TRUE, add_who_class = TRUE,
                            meta_columns = NULL) {
  # Validate input
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # Parse column names
  parsed_columns <- parse_amr_columns(names(data), expand_codes = expand_codes)
  
  # Filter for AMR columns
  amr_cols <- parsed_columns[!is.na(parsed_columns$type), ]
  
  # Check if any AMR columns were found
  if (nrow(amr_cols) == 0) {
    stop("No AMR data columns found. Columns should be prefixed with r_, n_, or d_")
  }
  
  # Identify unique antibiotic-pathogen combinations
  combinations <- unique(amr_cols[, c("antibiotic_code", "pathogen")])
  combinations <- combinations[!is.na(combinations$antibiotic_code) & !is.na(combinations$pathogen), ]
  
  # Create result data frame
  if (is.null(meta_columns)) {
    meta_columns <- setdiff(names(data), amr_cols$column)
  } else {
    # Ensure meta_columns exist in data
    missing_meta <- meta_columns[!meta_columns %in% names(data)]
    if (length(missing_meta) > 0) {
      warning("Missing metadata columns: ", paste(missing_meta, collapse = ", "))
      meta_columns <- meta_columns[meta_columns %in% names(data)]
    }
  }
  
  # Initialize result list
  result_list <- list()
  
  # Process each antibiotic-pathogen combination
  for (i in 1:nrow(combinations)) {
    ab_code <- combinations$antibiotic_code[i]
    path <- combinations$pathogen[i]
    
    # Find rate, total, and resistant columns
    rate_col <- amr_cols$column[amr_cols$type == "rate" & 
                                amr_cols$antibiotic_code == ab_code &
                                amr_cols$pathogen == path]
    
    total_col <- amr_cols$column[amr_cols$type == "total" & 
                                 amr_cols$antibiotic_code == ab_code &
                                 amr_cols$pathogen == path]
    
    resistant_col <- amr_cols$column[amr_cols$type == "resistant" & 
                                     amr_cols$antibiotic_code == ab_code &
                                     amr_cols$pathogen == path]
    
    # Create row data
    row_data <- data[, meta_columns, drop = FALSE]
    
    # Add antibiotic and pathogen information
    row_data$antibiotic_code <- ab_code
    
    if (expand_codes) {
      row_data$antibiotic_name <- antibiotic_code_to_name(ab_code)
    }
    
    if (add_who_class) {
      row_data$who_class <- antibiotic_code_to_name(ab_code, return_type = "who_class")
      row_data$antibiotic_class <- antibiotic_code_to_name(ab_code, return_type = "class")
    }
    
    row_data$pathogen <- path
    
    # Add rate, total, resistant data
    if (length(rate_col) > 0) {
      row_data$resistance_rate <- data[[rate_col[1]]]
    }
    
    if (length(total_col) > 0) {
      row_data$sample_count <- data[[total_col[1]]]
    }
    
    if (length(resistant_col) > 0) {
      row_data$resistant_count <- data[[resistant_col[1]]]
    }
    
    # Add to result list
    result_list[[i]] <- row_data
  }
  
  # Combine all rows
  result <- do.call(rbind, result_list)
  rownames(result) <- NULL
  
  return(result)
}
