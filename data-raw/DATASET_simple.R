## code to prepare simple datasets for the package
library(usethis)

# Create human AMR dataset
set.seed(123)
n_rows <- 100

# Generate sample data
human_amr <- data.frame(
  study_id = paste0("Study", sample(1:10, n_rows, replace = TRUE)),
  pathogen_name = sample(
    c("Escherichia coli", "Staphylococcus aureus", "Klebsiella pneumoniae", "Pseudomonas aeruginosa"),
    n_rows, replace = TRUE
  ),
  antibiotic_name = sample(
    c("Ciprofloxacin", "Ampicillin", "Meropenem", "Vancomycin", "Oxacillin"),
    n_rows, replace = TRUE
  ),
  sample_count = sample(50:500, n_rows, replace = TRUE),
  resistant_count = NA,
  year = sample(2010:2020, n_rows, replace = TRUE),
  country = sample(
    c("India", "Pakistan", "Bangladesh", "Nepal", "Sri Lanka"),
    n_rows, replace = TRUE
  ),
  region = sample(
    c("South Asia", "Southeast Asia", "East Asia"),
    n_rows, replace = TRUE
  ),
  setting = sample(
    c("Hospital", "Community", "Rural", "Urban"),
    n_rows, replace = TRUE
  ),
  population_type = sample(
    c("Adults", "Children", "All Ages"),
    n_rows, replace = TRUE
  ),
  study_design = sample(
    c("Cross-sectional", "Cohort", "Case-control"),
    n_rows, replace = TRUE
  ),
  total_sample_size = NA,
  author = paste0("Author", sample(1:20, n_rows, replace = TRUE)),
  age_range = sample(
    c("0-18", "18+", "All ages"),
    n_rows, replace = TRUE
  ),
  stringsAsFactors = FALSE
)

# Calculate resistant counts and rates
human_amr$resistant_count <- round(human_amr$sample_count * runif(n_rows, 0.05, 0.8))
human_amr$resistance_rate <- human_amr$resistant_count / human_amr$sample_count
human_amr$total_sample_size <- human_amr$sample_count * sample(1:5, n_rows, replace = TRUE)
human_amr$domain <- "human"

# Create antibiotic classification
antibiotic_classification <- data.frame(
  antibiotic_name = c(
    "Ciprofloxacin", "Levofloxacin", "Norfloxacin",
    "Ampicillin", "Amoxicillin", "Penicillin",
    "Meropenem", "Imipenem", "Ertapenem",
    "Vancomycin", "Teicoplanin",
    "Oxacillin", "Methicillin", "Cloxacillin"
  ),
  antibiotic_class = c(
    rep("Fluoroquinolones", 3),
    rep("Penicillins", 3),
    rep("Carbapenems", 3),
    rep("Glycopeptides", 2),
    rep("Penicillinase-resistant penicillins", 3)
  ),
  atc_code = c(
    rep("J01MA", 3),
    rep("J01CA", 3),
    rep("J01DH", 3),
    rep("J01XA", 2),
    rep("J01CF", 3)
  ),
  stringsAsFactors = FALSE
)

# Create pathogen classification
pathogen_classification <- data.frame(
  pathogen_name = c(
    "Escherichia coli", "Klebsiella pneumoniae", "Proteus mirabilis", 
    "Staphylococcus aureus", "Staphylococcus epidermidis", 
    "Pseudomonas aeruginosa", "Acinetobacter baumannii",
    "Enterococcus faecalis", "Enterococcus faecium"
  ),
  pathogen_group = c(
    rep("Enterobacteriaceae", 3),
    rep("Staphylococci", 2),
    rep("Non-fermenting Gram-negative bacilli", 2),
    rep("Enterococci", 2)
  ),
  gram_stain = c(
    rep("Negative", 3),
    rep("Positive", 2),
    rep("Negative", 2),
    rep("Positive", 2)
  ),
  stringsAsFactors = FALSE
)

# Create sample metadata
metadata <- data.frame(
  study_id = unique(human_amr$study_id),
  title = paste("Study on antimicrobial resistance in", sample(unique(human_amr$country), length(unique(human_amr$study_id)), replace = TRUE)),
  year = sample(2010:2020, length(unique(human_amr$study_id)), replace = TRUE),
  first_author = paste0("Author", sample(1:10, length(unique(human_amr$study_id)), replace = TRUE)),
  journal = sample(c("J Antimicrob Chemother", "Int J Antimicrob Agents", "Lancet Infect Dis"), length(unique(human_amr$study_id)), replace = TRUE),
  country = sample(unique(human_amr$country), length(unique(human_amr$study_id)), replace = TRUE),
  study_period = paste(sample(2008:2018, length(unique(human_amr$study_id)), replace = TRUE), "-", sample(2009:2019, length(unique(human_amr$study_id)), replace = TRUE)),
  stringsAsFactors = FALSE
)

# Save the datasets
usethis::use_data(human_amr, overwrite = TRUE)
usethis::use_data(antibiotic_classification, overwrite = TRUE)
usethis::use_data(pathogen_classification, overwrite = TRUE)
usethis::use_data(metadata, overwrite = TRUE) 