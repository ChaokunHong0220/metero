# metero

## Description

metero (Meta-analysis Tool for Exploring Antimicrobial Resistance Outcomes Across Realms) is an R package for analyzing antimicrobial resistance (AMR) data. The package implements the One Health approach, integrating AMR data from human, animal, and environmental domains to help researchers and health workers analyze, visualize, and understand AMR distribution and trends.

## Installation

You can install the development version of metero from GitHub:

```r
# Install devtools if you don't have it
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install metero
devtools::install_github("ChaokunHong0220/metero")
```

## Main Features

### 1. Data Standardization and Import

Easily import and standardize AMR data from different sources:

```r
library(metero)

# Import data from a CSV file
my_mapping <- list(
  study_id = "Study_ID",
  pathogen = "Bacteria",
  specific_antibiotic = "Antibiotic",
  sample_count = "Total",
  resistant_count = "Resistant"
)

amr_data <- import_amr_data(
  file_path = "my_amr_data.csv",
  mapping = my_mapping,
  domain = "human"
)

# Check data quality
quality_report <- check_data_quality(amr_data)
print(quality_report)
```

### 2. AMR Data Analysis

Perform meta-analysis and trend analysis on AMR data:

```r
# Calculate pooled resistance rates by pathogen and antibiotic
meta_results <- calculate_pooled_rate(
  data = amr_data,
  by = c("pathogen", "specific_antibiotic"),
  method = "random"
)

# Print results
print(meta_results)

# Analyze heterogeneity with country as a moderator
heterogeneity <- analyze_heterogeneity(
  data = amr_data,
  by = c("pathogen", "specific_antibiotic"),
  moderators = c("country")
)

# Perform subgroup analysis
subgroup_results <- perform_subgroup_analysis(
  data = amr_data,
  by = c("pathogen", "specific_antibiotic"),
  subgroups = c("setting")
)

# Analyze time trends
trend_analysis <- analyze_amr_trends(
  data = amr_data,
  time_var = "year",
  group_vars = c("pathogen", "antibiotic_class"),
  smooth_method = "loess"
)
```

### 3. Visualization

Create informative visualizations to understand AMR patterns:

```r
# Create a resistance heatmap
heatmap <- create_amr_heatmap(
  data = amr_data,
  sort_pathogens = "resistance",
  sort_antibiotics = "class"
)
print(heatmap)

# Create a forest plot from meta-analysis results
forest <- create_forest_plot(meta_results)
print(forest)

# Create a geographic map of AMR data
map <- create_geo_map(
  data = amr_data,
  pathogen = "Escherichia coli",
  antibiotic = "Ciprofloxacin"
)
print(map)

# Create a time trend plot
trend_plot <- create_trend_plot(
  data = amr_data,
  time_var = "year",
  group_vars = c("pathogen_name")
)
print(trend_plot)
```

### 4. Integrated Analysis Example

Complete workflow combining multiple analysis approaches:

```r
library(metero)
library(dplyr)
library(ggplot2)

# Load included data
data(human_amr)

# 1. Filter data for interesting combinations
ec_data <- human_amr %>%
  filter(pathogen == "Escherichia coli")

# 2. Perform meta-analysis
ec_meta <- calculate_pooled_rate(
  data = ec_data,
  by = c("specific_antibiotic"),
  method = "random"
)

# 3. Visualize as heatmap
ec_heatmap <- create_amr_heatmap(ec_data)
print(ec_heatmap + labs(title = "E. coli Resistance Rates"))

# 4. Analyze time trends
ec_trends <- create_trend_plot(
  ec_data,
  time_var = "year",
  group_vars = c("specific_antibiotic"),
  smooth_method = "loess"
)
print(ec_trends)

# 5. Analyze factors affecting resistance rates
ec_factors <- analyze_amr_factors(
  ec_data,
  target_pathogen = "Escherichia coli",
  target_antibiotic = "Ciprofloxacin",
  predictors = c("year", "country", "setting", "population_type")
)

# Create forest plot of odds ratios
or_plot <- ggplot2::ggplot(
  ec_factors$result_table[-1,], 
  ggplot2::aes(x = OR, y = Variable, xmin = CI_Lower, xmax = CI_Upper)
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbarh(height = 0.2) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    title = "Factors Affecting E. coli Resistance to Ciprofloxacin",
    x = "Odds Ratio (OR)",
    y = "Variable"
  ) +
  ggplot2::theme_minimal()

print(or_plot)
```

## Included Data

The metero package includes the `human_amr` dataset containing AMR data from South Asian countries, covering 4 pathogens and 35 antibiotics.

```r
# View data structure
data(human_amr)
str(human_amr)

# View metadata
data(metadata)
str(metadata)

# View antibiotic classification
data(antibiotic_classification)
head(antibiotic_classification)

# View pathogen classification
data(pathogen_classification)
print(pathogen_classification)
```

## Data Model

The metero package implements a standardized data model for AMR data:

```r
# View the data model
model <- get_data_model()

# View the metadata schema
head(model$metadata_schema)

# View the human AMR data schema
head(model$human_schema)

# Create an empty dataset structure
empty_dataset <- create_empty_dataset()
```

## Contributing

Contributions to metero are welcome! Please submit issues and pull requests through GitHub.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

Developer: Chaokun Hong
Email: chaokun.hong@ndm.ox.ac.uk

