#' Define the standard data model for AMR data
#'
#' This function returns the standard data model structure for metero,
#' including metadata, human data, animal data, and environment data schemas.
#'
#' @return A list containing data model specifications
#' @export
#'
#' @examples
#' data_model <- get_data_model()
#' # View the metadata schema
#' data_model$metadata_schema
get_data_model <- function() {
  # Define standard data model
  data_model <- list(
    # Metadata schema
    metadata_schema = data.frame(
      field_name = c(
        "study_id", "first_author", "publication_year", "country", 
        "region", "continent", "study_design", "sampling_method",
        "sample_size", "quality_score", "data_collection_start", 
        "data_collection_end"
      ),
      data_type = c(
        "character", "character", "integer", "character",
        "character", "character", "character", "character",
        "integer", "numeric", "Date", "Date"
      ),
      required = c(
        TRUE, TRUE, TRUE, TRUE,
        FALSE, FALSE, TRUE, FALSE,
        TRUE, FALSE, FALSE, FALSE
      ),
      description = c(
        "Unique identifier for each study",
        "First author of the study",
        "Year of publication",
        "Country where the study was conducted",
        "Region within the country or geographical region",
        "Continent of the study location",
        "Study design (e.g., Cross-sectional, Cohort, Case-control)",
        "Method of sampling used in the study",
        "Total sample size of the study",
        "Quality score assigned to the study (0-10)",
        "Start date of data collection",
        "End date of data collection"
      ),
      stringsAsFactors = FALSE
    ),
    
    # Human AMR data schema
    human_schema = data.frame(
      field_name = c(
        "record_id", "study_id", "pathogen", "pathogen_group",
        "antibiotic_class", "specific_antibiotic", "resistance_rate",
        "ci_lower", "ci_upper", "population_type", "age_group",
        "setting", "previous_hospitalization", "previous_antibiotic_use"
      ),
      data_type = c(
        "character", "character", "character", "character",
        "character", "character", "numeric",
        "numeric", "numeric", "character", "character",
        "character", "logical", "character"
      ),
      required = c(
        TRUE, TRUE, TRUE, FALSE,
        TRUE, TRUE, TRUE,
        FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE
      ),
      description = c(
        "Unique identifier for each record",
        "Reference to study ID in metadata",
        "Name of the bacterial pathogen",
        "Group classification of the pathogen",
        "Class of the antibiotic",
        "Specific antibiotic name",
        "Rate of resistance (0-1)",
        "Lower bound of confidence interval",
        "Upper bound of confidence interval",
        "Type of population studied (e.g., Adults, Children)",
        "Age group of the population",
        "Setting (e.g., Rural, Urban, Hospital)",
        "Whether patients had previous hospitalization",
        "History of antibiotic use in the population"
      ),
      stringsAsFactors = FALSE
    ),
    
    # Animal AMR data schema
    animal_schema = data.frame(
      field_name = c(
        "record_id", "study_id", "pathogen", "pathogen_group",
        "antibiotic_class", "specific_antibiotic", "resistance_rate",
        "ci_lower", "ci_upper", "animal_type", "animal_category",
        "farming_system", "sample_type", "antibiotic_usage"
      ),
      data_type = c(
        "character", "character", "character", "character",
        "character", "character", "numeric",
        "numeric", "numeric", "character", "character",
        "character", "character", "character"
      ),
      required = c(
        TRUE, TRUE, TRUE, FALSE,
        TRUE, TRUE, TRUE,
        FALSE, FALSE, TRUE, FALSE,
        FALSE, FALSE, FALSE
      ),
      description = c(
        "Unique identifier for each record",
        "Reference to study ID in metadata",
        "Name of the bacterial pathogen",
        "Group classification of the pathogen",
        "Class of the antibiotic",
        "Specific antibiotic name",
        "Rate of resistance (0-1)",
        "Lower bound of confidence interval",
        "Upper bound of confidence interval",
        "Type of animal (e.g., Cattle, Poultry, Swine)",
        "Category of animal (e.g., Dairy, Beef, Layer)",
        "Farming system (e.g., Intensive, Extensive, Organic)",
        "Type of sample (e.g., Fecal, Nasal, Meat)",
        "Pattern of antibiotic use in the animals"
      ),
      stringsAsFactors = FALSE
    ),
    
    # Environment AMR data schema
    environment_schema = data.frame(
      field_name = c(
        "record_id", "study_id", "pathogen", "pathogen_group",
        "antibiotic_class", "specific_antibiotic", "detection_rate",
        "ci_lower", "ci_upper", "sample_type", "environment_category",
        "proximity_human", "proximity_animals", "pollution_source"
      ),
      data_type = c(
        "character", "character", "character", "character",
        "character", "character", "numeric",
        "numeric", "numeric", "character", "character",
        "character", "character", "character"
      ),
      required = c(
        TRUE, TRUE, TRUE, FALSE,
        TRUE, TRUE, TRUE,
        FALSE, FALSE, TRUE, TRUE,
        FALSE, FALSE, FALSE
      ),
      description = c(
        "Unique identifier for each record",
        "Reference to study ID in metadata",
        "Name of the bacterial pathogen",
        "Group classification of the pathogen",
        "Class of the antibiotic",
        "Specific antibiotic name",
        "Rate of detection (0-1)",
        "Lower bound of confidence interval",
        "Upper bound of confidence interval",
        "Type of sample (e.g., Water, Soil, Sewage)",
        "Category of environment (e.g., River, Farm, Hospital effluent)",
        "Proximity to human habitation",
        "Proximity to animal farming",
        "Source of pollution if applicable"
      ),
      stringsAsFactors = FALSE
    ),
    
    # Reference tables
    reference_tables = list(
      # Pathogen classification
      pathogen_classification = data.frame(
        pathogen_name = c(
          "Escherichia coli", "Klebsiella pneumoniae", 
          "Staphylococcus aureus", "Streptococcus pneumoniae"
        ),
        pathogen_group = c(
          "Enterobacteriaceae", "Enterobacteriaceae", 
          "Staphylococcaceae", "Streptococcaceae"
        ),
        gram_stain = c(
          "Negative", "Negative", "Positive", "Positive"
        ),
        who_priority = c(
          "Critical", "Critical", "High", "Medium"
        ),
        stringsAsFactors = FALSE
      ),
      
      # Antibiotic classification
      antibiotic_classification = data.frame(
        antibiotic_name = c(
          "Amoxicillin", "Ampicillin", "Amikacin", "Azithromycin",
          "Ceftriaxone", "Ciprofloxacin", "Gentamicin", "Imipenem",
          "Meropenem", "Tetracycline", "Vancomycin"
        ),
        antibiotic_class = c(
          "Penicillins", "Penicillins", "Aminoglycosides", "Macrolides",
          "Cephalosporins", "Fluoroquinolones", "Aminoglycosides", "Carbapenems",
          "Carbapenems", "Tetracyclines", "Glycopeptides"
        ),
        who_classification = c(
          "Access", "Access", "Watch", "Watch",
          "Watch", "Watch", "Access", "Watch",
          "Watch", "Access", "Watch"
        ),
        stringsAsFactors = FALSE
      ),
      
      # Geo hierarchy
      geo_hierarchy = data.frame(
        country = c(
          "India", "Pakistan", "Bangladesh", "Nepal", "Sri Lanka"
        ),
        region = c(
          "South Asia", "South Asia", "South Asia", "South Asia", "South Asia"
        ),
        continent = c(
          "Asia", "Asia", "Asia", "Asia", "Asia"
        ),
        stringsAsFactors = FALSE
      )
    )
  )
  
  return(data_model)
}

#' Create an empty metero dataset structure
#'
#' Creates an empty dataset structure according to the metero data model,
#' ready to be populated with data.
#'
#' @return A list containing empty data frames for metadata and domain-specific data
#' @export
#'
#' @examples
#' # Create an empty dataset
#' empty_dataset <- create_empty_dataset()
create_empty_dataset <- function() {
  # Get data model
  model <- get_data_model()
  
  # Create empty metadata dataframe
  metadata <- data.frame(matrix(ncol = nrow(model$metadata_schema), nrow = 0))
  colnames(metadata) <- model$metadata_schema$field_name
  
  # Create empty human data dataframe
  human_data <- data.frame(matrix(ncol = nrow(model$human_schema), nrow = 0))
  colnames(human_data) <- model$human_schema$field_name
  
  # Create empty animal data dataframe
  animal_data <- data.frame(matrix(ncol = nrow(model$animal_schema), nrow = 0))
  colnames(animal_data) <- model$animal_schema$field_name
  
  # Create empty environment data dataframe
  environment_data <- data.frame(matrix(ncol = nrow(model$environment_schema), nrow = 0))
  colnames(environment_data) <- model$environment_schema$field_name
  
  # Create empty molecular data dataframe (simplified for now)
  molecular_data <- data.frame(
    record_id = character(),
    study_id = character(),
    domain = character(),
    sample_id = character(),
    gene_name = character(),
    gene_family = character(),
    resistance_mechanism = character(),
    detection_frequency = numeric(),
    host_species = character(),
    stringsAsFactors = FALSE
  )
  
  # Compile into a metero dataset
  meteor_dataset <- list(
    metadata = metadata,
    human_data = human_data,
    animal_data = animal_data,
    environment_data = environment_data,
    molecular_data = molecular_data,
    reference_tables = model$reference_tables,
    analysis_results = list()
  )
  
  # Set class
  class(meteor_dataset) <- c("meteor_dataset", "list")
  
  return(meteor_dataset)
}

#' Check if an object is a valid metero dataset
#'
#' Validates whether an object conforms to the metero dataset structure.
#'
#' @param x Object to check
#' @return TRUE if the object is a valid metero dataset, FALSE otherwise
#' @export
#'
#' @examples
#' dataset <- create_empty_dataset()
#' is_meteor_dataset(dataset)
is_meteor_dataset <- function(x) {
  # Check class
  if (!inherits(x, "meteor_dataset")) {
    return(FALSE)
  }
  
  # Check required components
  required_components <- c("metadata", "human_data", "animal_data", 
                           "environment_data", "reference_tables")
  if (!all(required_components %in% names(x))) {
    return(FALSE)
  }
  
  # Check that each component is a data frame where expected
  data_frames <- c("metadata", "human_data", "animal_data", "environment_data")
  for (comp in data_frames) {
    if (!is.data.frame(x[[comp]])) {
      return(FALSE)
    }
  }
  
  # Check that reference_tables is a list
  if (!is.list(x$reference_tables)) {
    return(FALSE)
  }
  
  # All checks passed
  return(TRUE)
}

#' Print method for meteor dataset
#'
#' @param x A meteor dataset
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the meteor dataset
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- create_empty_dataset()
#' print(dataset)
#' }
print.meteor_dataset <- function(x, ...) {
  cat("METERO Dataset\n")
  cat("==============\n")
  cat("Metadata records:", nrow(x$metadata), "\n")
  cat("Human AMR records:", nrow(x$human_data), "\n")
  cat("Animal AMR records:", nrow(x$animal_data), "\n")
  cat("Environment AMR records:", nrow(x$environment_data), "\n")
  
  if (!is.null(x$molecular_data) && nrow(x$molecular_data) > 0) {
    cat("Molecular data records:", nrow(x$molecular_data), "\n")
  }
  
  if (!is.null(x$analysis_results) && length(x$analysis_results) > 0) {
    cat("Analysis results available:", length(x$analysis_results), "\n")
  }
  
  invisible(x)
}

#' Summary method for meteor dataset
#'
#' @param object A meteor dataset
#' @param ... Additional arguments passed to summary
#' @return A list containing summary statistics for the dataset
#' @export
#'
#' @examples
#' \dontrun{
#' dataset <- create_empty_dataset()
#' summary(dataset)
#' }
summary.meteor_dataset <- function(object, ...) {
  # Get counts
  metadata_count <- nrow(object$metadata)
  human_count <- nrow(object$human_data)
  animal_count <- nrow(object$animal_data)
  environment_count <- nrow(object$environment_data)
  
  # Get unique counts if data exists
  unique_studies <- if (metadata_count > 0) length(unique(object$metadata$study_id)) else 0
  
  human_summary <- if (human_count > 0) {
    data.frame(
      pathogens = length(unique(object$human_data$pathogen)),
      antibiotics = length(unique(object$human_data$specific_antibiotic)),
      countries = if ("country" %in% names(object$metadata)) length(unique(object$metadata$country)) else NA
    )
  } else {
    data.frame(pathogens = 0, antibiotics = 0, countries = 0)
  }
  
  animal_summary <- if (animal_count > 0) {
    data.frame(
      pathogens = length(unique(object$animal_data$pathogen)),
      antibiotics = length(unique(object$animal_data$specific_antibiotic)),
      animal_types = length(unique(object$animal_data$animal_type))
    )
  } else {
    data.frame(pathogens = 0, antibiotics = 0, animal_types = 0)
  }
  
  environment_summary <- if (environment_count > 0) {
    data.frame(
      pathogens = length(unique(object$environment_data$pathogen)),
      antibiotics = length(unique(object$environment_data$specific_antibiotic)),
      sample_types = length(unique(object$environment_data$sample_type))
    )
  } else {
    data.frame(pathogens = 0, antibiotics = 0, sample_types = 0)
  }
  
  # Create summary object
  summary_obj <- list(
    record_counts = data.frame(
      domain = c("metadata", "human", "animal", "environment"),
      records = c(metadata_count, human_count, animal_count, environment_count)
    ),
    study_count = unique_studies,
    human_summary = human_summary,
    animal_summary = animal_summary,
    environment_summary = environment_summary
  )
  
  class(summary_obj) <- c("meteor_summary", "list")
  return(summary_obj)
}

#' Print method for meteor summary
#'
#' @param x A meteor dataset summary
#' @param ... Additional arguments passed to print
#' @return Invisibly returns the summary object
#' @export
print.meteor_summary <- function(x, ...) {
  cat("METERO Dataset Summary\n")
  cat("=====================\n\n")
  
  cat("Record Counts:\n")
  print(x$record_counts)
  
  cat("\nUnique Studies:", x$study_count, "\n\n")
  
  if (x$human_summary$pathogens > 0) {
    cat("Human AMR Data Summary:\n")
    cat("  - Pathogens:", x$human_summary$pathogens, "\n")
    cat("  - Antibiotics:", x$human_summary$antibiotics, "\n")
    if (!is.na(x$human_summary$countries)) {
      cat("  - Countries:", x$human_summary$countries, "\n")
    }
    cat("\n")
  }
  
  if (x$animal_summary$pathogens > 0) {
    cat("Animal AMR Data Summary:\n")
    cat("  - Pathogens:", x$animal_summary$pathogens, "\n")
    cat("  - Antibiotics:", x$animal_summary$antibiotics, "\n")
    cat("  - Animal Types:", x$animal_summary$animal_types, "\n")
    cat("\n")
  }
  
  if (x$environment_summary$pathogens > 0) {
    cat("Environment AMR Data Summary:\n")
    cat("  - Pathogens:", x$environment_summary$pathogens, "\n")
    cat("  - Antibiotics:", x$environment_summary$antibiotics, "\n")
    cat("  - Sample Types:", x$environment_summary$sample_types, "\n")
  }
  
  invisible(x)
} 