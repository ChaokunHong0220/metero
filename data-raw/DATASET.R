## Process raw AMR data, converting to standard format dataset for the package

# Read raw data
raw_data <- read.csv("data-raw/Data_extraction_final.csv", stringsAsFactors = FALSE)

# Create empty dataframe for storing converted data
human_amr <- data.frame()

# Drug names and their abbreviations mapping
drug_map <- list(
  "AML" = "Amoxicillin",
  "AMP" = "Ampicillin",
  "AMK" = "Amikacin",
  "AZM" = "Azithromycin",
  "ATM" = "Aztreonam",
  "CFZ" = "Cefazolin",
  "FEP" = "Cefepime",
  "CFM" = "Cefixime",
  "CTX" = "Cefotaxime",
  "FOX" = "Cefoxitin",
  "CAZ" = "Ceftazidime",
  "CRO" = "Ceftriaxone",
  "LEX" = "Cephalexin",
  "CHL" = "Chloramphenicol",
  "CIP" = "Ciprofloxacin",
  "CLI" = "Clindamycin",
  "CST" = "Colistin",
  "SXT" = "Co-trimoxazole",
  "DOX" = "Doxycycline",
  "ERY" = "Erythromycin",
  "GEN" = "Gentamicin",
  "IPM" = "Imipenem",
  "KAN" = "Kanamycin",
  "LVX" = "Levofloxacin",
  "MEM" = "Meropenem",
  "NAL" = "Nalidixic Acid",
  "NIT" = "Nitrofurantoin",
  "OFX" = "Ofloxacin",
  "OXA" = "Oxacillin",
  "PEN" = "Penicillin",
  "STR" = "Streptomycin",
  "SOX" = "Sulfisoxazole",
  "TET" = "Tetracycline",
  "TGC" = "Tigecycline",
  "VAN" = "Vancomycin"
)

# Pathogen name abbreviations mapping
pathogen_map <- list(
  "Ecoil" = "Escherichia coli",
  "KP" = "Klebsiella pneumoniae",
  "SA" = "Staphylococcus aureus",
  "SP" = "Streptococcus pneumoniae"
)

# Process each row of data
for (i in 1:nrow(raw_data)) {
  study_id <- raw_data$study.ID[i]
  author <- raw_data$firstauthor_last[i]
  year <- raw_data$year_published[i]
  country <- raw_data$location[i]
  study_design <- raw_data$study_type[i]
  setting <- raw_data$Environment[i]
  population_type <- raw_data$population[i]
  total_sample_size <- raw_data$n_pop[i]
  age_range <- raw_data$age_range[i]
  
  # Process each drug and pathogen combination
  for (drug_code in names(drug_map)) {
    for (pathogen_code in names(pathogen_map)) {
      # Build column names
      r_col <- paste0("r_", drug_code, "_", pathogen_code)
      n_col <- paste0("n_", drug_code, "_", pathogen_code)
      
      # Check if columns exist and are not empty
      if (r_col %in% colnames(raw_data) && n_col %in% colnames(raw_data) &&
          !is.na(raw_data[[r_col]][i]) && !is.na(raw_data[[n_col]][i]) && 
          raw_data[[n_col]][i] > 0) {
        
        resistant_count <- raw_data[[r_col]][i] * raw_data[[n_col]][i]
        sample_count <- raw_data[[n_col]][i]
        
        # If resistant_count is not an integer, floor it
        resistant_count <- floor(resistant_count)
        
        # Ensure counts are valid
        if (!is.na(resistant_count) && !is.na(sample_count) && 
            resistant_count >= 0 && sample_count > 0) {
          
          # Add a row to the result dataframe
          new_row <- data.frame(
            study_id = study_id,
            pathogen_name = pathogen_map[[pathogen_code]],
            antibiotic_name = drug_map[[drug_code]],
            sample_count = sample_count,
            resistant_count = resistant_count,
            resistance_rate = resistant_count / sample_count,
            domain = "human",
            region = NA,  # Can be added later based on country
            author = author,
            year = year,
            country = country,
            study_design = study_design,
            setting = setting,
            population_type = population_type,
            total_sample_size = total_sample_size,
            age_range = age_range,
            stringsAsFactors = FALSE
          )
          
          human_amr <- rbind(human_amr, new_row)
        }
      }
    }
  }
}

# Filter by common and important drug-pathogen combinations
# Optional step, if data is too large, consider filtering important combinations
# This is just an example

# Handle missing values
human_amr$region <- ifelse(human_amr$country %in% c("India", "Pakistan", "Nepal", "Bangladesh", "Sri Lanka"), 
                        "South Asia", NA)

# Save processed data
usethis::use_data(human_amr, overwrite = TRUE)

# Print data summary
cat("Dataset summary:\n")
cat("Records:", nrow(human_amr), "\n")
cat("Studies:", length(unique(human_amr$study_id)), "\n")
cat("Pathogens:", length(unique(human_amr$pathogen_name)), "\n")
cat("Antibiotics:", length(unique(human_amr$antibiotic_name)), "\n")
