## Process raw AMR data, converting to standard format dataset for the package

# Load required packages for data processing
library(dplyr)
library(usethis)

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

# Antibiotic class mapping
antibiotic_class_map <- list(
  "Amoxicillin" = "Penicillins",
  "Ampicillin" = "Penicillins",
  "Amikacin" = "Aminoglycosides",
  "Azithromycin" = "Macrolides",
  "Aztreonam" = "Monobactams",
  "Cefazolin" = "Cephalosporins",
  "Cefepime" = "Cephalosporins",
  "Cefixime" = "Cephalosporins",
  "Cefotaxime" = "Cephalosporins",
  "Cefoxitin" = "Cephalosporins",
  "Ceftazidime" = "Cephalosporins",
  "Ceftriaxone" = "Cephalosporins",
  "Cephalexin" = "Cephalosporins",
  "Chloramphenicol" = "Phenicols",
  "Ciprofloxacin" = "Fluoroquinolones",
  "Clindamycin" = "Lincosamides",
  "Colistin" = "Polymyxins",
  "Co-trimoxazole" = "Sulfonamides",
  "Doxycycline" = "Tetracyclines",
  "Erythromycin" = "Macrolides",
  "Gentamicin" = "Aminoglycosides",
  "Imipenem" = "Carbapenems",
  "Kanamycin" = "Aminoglycosides",
  "Levofloxacin" = "Fluoroquinolones",
  "Meropenem" = "Carbapenems",
  "Nalidixic Acid" = "Quinolones",
  "Nitrofurantoin" = "Nitrofurans",
  "Ofloxacin" = "Fluoroquinolones",
  "Oxacillin" = "Penicillins",
  "Penicillin" = "Penicillins",
  "Streptomycin" = "Aminoglycosides",
  "Sulfisoxazole" = "Sulfonamides",
  "Tetracycline" = "Tetracyclines",
  "Tigecycline" = "Glycylcyclines",
  "Vancomycin" = "Glycopeptides"
)

# Pathogen group mapping
pathogen_group_map <- list(
  "Escherichia coli" = "Enterobacteriaceae",
  "Klebsiella pneumoniae" = "Enterobacteriaceae",
  "Staphylococcus aureus" = "Staphylococcaceae",
  "Streptococcus pneumoniae" = "Streptococcaceae"
)

# Generate unique record IDs
record_id_counter <- 1

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
        
        # Calculate counts
        resistance_rate <- raw_data[[r_col]][i]
        sample_count <- raw_data[[n_col]][i]
        resistant_count <- round(resistance_rate * sample_count)
        
        # Create confidence interval
        if (resistant_count > 0 && resistant_count < sample_count) {
          # Wilson score interval
          z <- qnorm(0.975)
          p_hat <- resistant_count / sample_count
          denom <- 1 + z^2/sample_count
          center <- (p_hat + z^2/(2*sample_count)) / denom
          error_term <- z * sqrt((p_hat * (1 - p_hat) + z^2/(4*sample_count)) / sample_count) / denom
          ci_lower <- max(0, center - error_term)
          ci_upper <- min(1, center + error_term)
        } else if (resistant_count == 0) {
          # One-sided CI for zero counts
          ci_lower <- 0
          ci_upper <- 1 - (0.05)^(1/sample_count)
        } else if (resistant_count == sample_count) {
          # One-sided CI for 100% resistance
          ci_lower <- (0.05)^(1/sample_count)
          ci_upper <- 1
        } else {
          ci_lower <- NA
          ci_upper <- NA
        }
        
        # Map to antibiotic class
        antibiotic_name <- drug_map[[drug_code]]
        antibiotic_class <- antibiotic_class_map[[antibiotic_name]]
        
        # Map to pathogen group
        pathogen_name <- pathogen_map[[pathogen_code]]
        pathogen_group <- pathogen_group_map[[pathogen_name]]
        
        # Generate unique record ID
        record_id <- paste0("REC", sprintf("%04d", record_id_counter))
        record_id_counter <- record_id_counter + 1
        
        # Add a row to the result dataframe
        new_row <- data.frame(
          record_id = record_id,
          study_id = study_id,
          pathogen = pathogen_name,
          pathogen_group = pathogen_group,
          antibiotic_class = antibiotic_class,
          specific_antibiotic = antibiotic_name,
          resistance_rate = resistance_rate,
          ci_lower = ci_lower,
          ci_upper = ci_upper,
          sample_count = sample_count,
          resistant_count = resistant_count,
          population_type = population_type,
          age_group = age_range,
          setting = setting,
          previous_hospitalization = NA,
          previous_antibiotic_use = NA,
          domain = "human",
          region = NA,
          author = author,
          year = year,
          country = country,
          study_design = study_design,
          total_sample_size = total_sample_size,
          stringsAsFactors = FALSE
        )
        
        human_amr <- rbind(human_amr, new_row)
      }
    }
  }
}

# Clean and standardize data

# Handle missing values
human_amr$region <- ifelse(human_amr$country %in% c("India", "Pakistan", "Nepal", "Bangladesh", "Sri Lanka"), 
                        "South Asia", NA)

# Standardize categorical variables
human_amr$setting <- toupper(human_amr$setting)
human_amr$setting <- gsub("\\s+", "_", human_amr$setting)
human_amr$setting <- gsub("HOSPITAL.*", "HOSPITAL", human_amr$setting)
human_amr$setting <- gsub("RURAL.*", "RURAL", human_amr$setting)
human_amr$setting <- gsub("URBAN.*", "URBAN", human_amr$setting)

human_amr$population_type <- toupper(human_amr$population_type)
human_amr$population_type <- gsub("\\s+", "_", human_amr$population_type)
human_amr$population_type <- gsub("CHILDREN.*|CHILD.*|PEDIATRIC.*", "CHILDREN", human_amr$population_type)
human_amr$population_type <- gsub("ADULT.*", "ADULTS", human_amr$population_type)
human_amr$population_type <- gsub("ALL.*|MIXED.*|WHOLE.*", "ALL_AGES", human_amr$population_type)

# Add continent information
human_amr$continent <- "Asia"

# Create metadata dataframe
metadata <- human_amr %>%
  select(study_id, author, year, country, region, continent, study_design, setting, total_sample_size) %>%
  distinct() %>%
  rename(
    first_author = author,
    publication_year = year,
    sample_size = total_sample_size
  ) %>%
  mutate(
    sampling_method = NA_character_,
    quality_score = NA_real_,
    data_collection_start = NA,
    data_collection_end = NA
  )

# Create animal AMR placeholder (empty dataset)
animal_amr <- data.frame(
  record_id = character(),
  study_id = character(),
  pathogen = character(),
  pathogen_group = character(),
  antibiotic_class = character(),
  specific_antibiotic = character(),
  resistance_rate = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  animal_type = character(),
  animal_category = character(),
  farming_system = character(),
  sample_type = character(),
  antibiotic_usage = character(),
  stringsAsFactors = FALSE
)

# Create environment AMR placeholder (empty dataset)
environment_amr <- data.frame(
  record_id = character(),
  study_id = character(),
  pathogen = character(),
  pathogen_group = character(),
  antibiotic_class = character(),
  specific_antibiotic = character(),
  detection_rate = numeric(),
  ci_lower = numeric(),
  ci_upper = numeric(),
  sample_type = character(),
  environment_category = character(),
  proximity_human = character(),
  proximity_animals = character(),
  pollution_source = character(),
  stringsAsFactors = FALSE
)

# Create reference tables
# Antibiotic classification
antibiotic_classification <- data.frame(
  antibiotic_name = names(antibiotic_class_map),
  antibiotic_class = unlist(antibiotic_class_map),
  stringsAsFactors = FALSE
)

# Add WHO classification (AWaRe)
who_classification <- c(
  rep("Access", 2),  # Amoxicillin, Ampicillin
  "Watch",           # Amikacin
  "Watch",           # Azithromycin
  "Watch",           # Aztreonam
  rep("Watch", 7),   # Cephalosporins (various)
  "Access",          # Chloramphenicol
  "Watch",           # Ciprofloxacin
  "Access",          # Clindamycin
  "Reserve",         # Colistin
  "Access",          # Co-trimoxazole
  "Access",          # Doxycycline
  "Watch",           # Erythromycin
  "Access",          # Gentamicin
  "Watch",           # Imipenem
  "Access",          # Kanamycin
  "Watch",           # Levofloxacin
  "Watch",           # Meropenem
  "Access",          # Nalidixic Acid
  "Access",          # Nitrofurantoin
  "Watch",           # Ofloxacin
  "Access",          # Oxacillin
  "Access",          # Penicillin
  "Access",          # Streptomycin
  "Access",          # Sulfisoxazole
  "Access",          # Tetracycline
  "Reserve",         # Tigecycline
  "Watch"            # Vancomycin
)

antibiotic_classification$who_classification <- who_classification

# Pathogen classification
pathogen_classification <- data.frame(
  pathogen_name = names(pathogen_group_map),
  pathogen_group = unlist(pathogen_group_map),
  gram_stain = c("Negative", "Negative", "Positive", "Positive"),
  who_priority = c("Critical", "Critical", "High", "Medium"),
  stringsAsFactors = FALSE
)

# Geo hierarchy
geo_hierarchy <- data.frame(
  country = c("India", "Pakistan", "Bangladesh", "Nepal", "Sri Lanka"),
  region = rep("South Asia", 5),
  continent = rep("Asia", 5),
  stringsAsFactors = FALSE
)

# Save processed data to the package
usethis::use_data(human_amr, overwrite = TRUE)
usethis::use_data(metadata, overwrite = TRUE)
usethis::use_data(antibiotic_classification, overwrite = TRUE)
usethis::use_data(pathogen_classification, overwrite = TRUE)
usethis::use_data(geo_hierarchy, overwrite = TRUE)

# Print data summary
cat("Dataset summary:\n")
cat("Records:", nrow(human_amr), "\n")
cat("Studies:", length(unique(human_amr$study_id)), "\n")
cat("Pathogens:", length(unique(human_amr$pathogen)), "\n")
cat("Antibiotics:", length(unique(human_amr$specific_antibiotic)), "\n")
cat("Countries:", length(unique(human_amr$country)), "\n")
cat("Years:", paste(range(human_amr$year), collapse = "-"), "\n")
