# metero

## Description

metero (Meta-analysis Tool for Exploring Antimicrobial Resistance Outcomes Across Realms) is an R package for analyzing antimicrobial resistance (AMR) data. The package implements the One Health approach, integrating AMR data from human, animal, and environmental domains to help researchers and health workers analyze, visualize, and understand AMR distribution and trends.

## Main Features

### 1. Data Standardization

Convert AMR data from different formats into a standard structure for unified analysis.

- `standardize_amr_data()`: Convert user data to the standard metero format
- `get_standard_amr_structure()`: Get information about the standard data structure

### 2. Data Comparison

Compare AMR data from different sources to identify differences and similarities in resistance patterns.

- `compare_with_package_data()`: Compare user-imported data with package data
- `plot_amr_comparison()`: Create visualizations comparing AMR datasets

### 3. Trend Analysis

Analyze AMR data changes over time to discover temporal patterns in resistance.

- `analyze_amr_trends()`: Analyze AMR data trends over different time periods
- `create_amr_heatmap()`: Create a heatmap of pathogen-antibiotic resistance rates

### 4. Multivariate Analysis

Analyze factors influencing AMR and evaluate the impact of different variables on resistance rates.

- `analyze_amr_factors()`: Use multivariate logistic regression to analyze factors affecting resistance

## Installation

```r
# Install dependencies
install.packages(c("dplyr", "ggplot2", "scales", "tidyr", "stats"))

# Install development version
devtools::install_github("ChaokunHong0220/metero")
```

## Detailed Usage Examples

### Data Standardization

Data standardization is the first step in analysis, converting various data formats into a unified structure.

```r
library(metero)

# View standard data structure
structure <- get_standard_amr_structure()
print(structure)

# Example data in a non-standard format
my_data <- data.frame(
  "Study ID" = c("S001", "S002", "S003"),
  "Bacteria" = c("E. coli", "K. pneumoniae", "S. aureus"),
  "Drug" = c("Ciprofloxacin", "Meropenem", "Vancomycin"),
  "Total" = c(100, 80, 120),
  "Resistant" = c(35, 5, 12),
  "Year" = c(2018, 2019, 2020),
  "Country" = c("India", "Pakistan", "Nepal")
)

# Create mapping relationship
my_mapping <- list(
  "study_id" = "Study ID",
  "pathogen_name" = "Bacteria",
  "antibiotic_name" = "Drug",
  "sample_count" = "Total",
  "resistant_count" = "Resistant",
  "year" = "Year",
  "country" = "Country"
)

# Standardize data
std_data <- standardize_amr_data(my_data, my_mapping)
print(head(std_data))
```

### Data Comparison

Compare your AMR data with package preset data to identify differences and similarities.

```r
# Load package data
data(human_amr)

# Create mapping from user data to standard format
my_mapping <- list(
  "study_id" = "Study ID",
  "pathogen_name" = "Bacteria",
  "antibiotic_name" = "Drug",
  "sample_count" = "Total",
  "resistant_count" = "Resistant"
)

# Compare data
comparison_results <- compare_with_package_data(
  my_data,
  my_mapping,
  comparison_vars = c("pathogen_name", "antibiotic_name")
)

# View comparison results
print(comparison_results$comparison_table)
print(comparison_results$user_data_summary)
print(comparison_results$ref_data_summary)

# Create scatter plot comparison
scatter_plot <- plot_amr_comparison(comparison_results, plot_type = "scatter")
print(scatter_plot)

# Create bar chart comparison
bar_plot <- plot_amr_comparison(comparison_results, plot_type = "bar")
print(bar_plot)

# Create forest plot comparison
forest_plot <- plot_amr_comparison(comparison_results, plot_type = "forest")
print(forest_plot)
```

### Trend Analysis

Analyze AMR changes over time to identify key patterns and changes.

```r
# Load package data
data(human_amr)

# Filter E. coli data
ec_data <- subset(human_amr, pathogen_name == "Escherichia coli")

# Analyze trends by antibiotic (LOESS smoothing)
ec_trends <- analyze_amr_trends(
  ec_data,
  time_var = "year",
  group_vars = "antibiotic_name",
  min_studies = 1,
  smooth_method = "loess"
)

# Display trend chart
print(ec_trends$plot +
      ggplot2::labs(title = "E. coli resistance trends for different antibiotics") +
      ggplot2::theme(legend.position = "bottom"))

# Use linear regression to analyze trends
ec_linear_trends <- analyze_amr_trends(
  ec_data,
  time_var = "year",
  group_vars = "antibiotic_name",
  min_studies = 1,
  smooth_method = "lm"
)

# View linear model results
print(ec_linear_trends$model_summary)

# Analyze resistance trends in different population groups
age_groups <- analyze_amr_trends(
  human_amr,
  time_var = "year",
  group_vars = c("population_type"),
  min_studies = 1,
  smooth_method = "lm"
)

# Display population trends
print(age_groups$plot +
      ggplot2::labs(title = "Antimicrobial resistance trends in different populations") +
      ggplot2::theme(legend.position = "bottom"))

# Multivariate grouping (by pathogen and country)
multi_trends <- analyze_amr_trends(
  human_amr,
  time_var = "year",
  group_vars = c("pathogen_name", "country"),
  min_studies = 1,
  smooth_method = "loess"
)

print(multi_trends$plot +
      ggplot2::labs(title = "Resistance trends across countries and pathogens"))
```

### Creating Heatmaps

Visualize resistance patterns between pathogens and antibiotics.

```r
# Create heatmap of all data
amr_heatmap <- create_amr_heatmap(human_amr)
print(amr_heatmap)

# Heatmap for a specific country - India
india_heatmap <- create_amr_heatmap(
  human_amr, 
  filter_conditions = list(country = "India")
)
print(india_heatmap + 
      ggplot2::labs(title = "Pathogen-Antibiotic Resistance Rates in India"))

# Heatmap for a specific country - Pakistan
pakistan_heatmap <- create_amr_heatmap(
  human_amr, 
  filter_conditions = list(country = "Pakistan")
)
print(pakistan_heatmap + 
      ggplot2::labs(title = "Pathogen-Antibiotic Resistance Rates in Pakistan"))

# Heatmap for a specific population - Children
children_heatmap <- create_amr_heatmap(
  human_amr, 
  filter_conditions = list(population_type = "Children")
)
print(children_heatmap + 
      ggplot2::labs(title = "Pathogen-Antibiotic Resistance Rates in Children"))
```

### Multivariate Analysis

Analyze various factors affecting AMR.

```r
# Analyze factors affecting E. coli resistance to ciprofloxacin
if (nrow(subset(human_amr, 
                pathogen_name == "Escherichia coli" & 
                antibiotic_name == "Ciprofloxacin")) > 30) {
  
  ec_cip_model <- analyze_amr_factors(
    human_amr,
    target_pathogen = "Escherichia coli",
    target_antibiotic = "Ciprofloxacin",
    predictors = c("year", "country", "population_type"),
    min_samples = 30
  )
  
  # View model summary
  print(ec_cip_model$summary)
  
  # View results table
  print(ec_cip_model$result_table)
  
  # Create forest plot of odds ratios
  or_plot <- ggplot2::ggplot(
    ec_cip_model$result_table[-1,], # Remove intercept
    ggplot2::aes(x = OR, y = Variable, xmin = CI_Lower, xmax = CI_Upper)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh(height = 0.2) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "Factors affecting E. coli resistance to ciprofloxacin",
      x = "Odds Ratio (OR)",
      y = "Variable"
    ) +
    ggplot2::theme_minimal()
  
  print(or_plot)
}

# Analyze factors affecting S. aureus resistance to erythromycin
if (nrow(subset(human_amr, 
                pathogen_name == "Staphylococcus aureus" & 
                antibiotic_name == "Erythromycin")) > 30) {
  
  sa_ery_model <- analyze_amr_factors(
    human_amr,
    target_pathogen = "Staphylococcus aureus",
    target_antibiotic = "Erythromycin",
    predictors = c("year", "setting", "country"),
    min_samples = 30
  )
  
  # View ANOVA results
  print(sa_ery_model$anova)
}
```

### Comprehensive Analysis Example

Below is a comprehensive analysis workflow demonstrating how to combine multiple features of the package for a complete analysis.

```r
library(metero)
library(dplyr)
library(ggplot2)

# Load data
data(human_amr)

# 1. Data exploration
summary_stats <- human_amr %>%
  group_by(pathogen_name, antibiotic_name) %>%
  summarise(
    studies = n_distinct(study_id),
    samples = sum(sample_count),
    resistance = mean(resistance_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(samples))

head(summary_stats, 10)

# 2. Create heatmap
main_heatmap <- create_amr_heatmap(human_amr)
print(main_heatmap + 
      labs(title = "Overall Resistance Rates in South Asia"))

# 3. Time trend analysis
# Select high-resistance pathogen-antibiotic combinations from the heatmap
high_resistance <- human_amr %>%
  filter(pathogen_name == "Escherichia coli", 
         antibiotic_name %in% c("Ampicillin", "Ciprofloxacin"))

# Analyze trends by country and antibiotic
trends_by_country <- analyze_amr_trends(
  high_resistance,
  time_var = "year",
  group_vars = c("country", "antibiotic_name"),
  smooth_method = "lm"
)

print(trends_by_country$plot + 
      labs(title = "E. coli resistance to key antibiotics over time (by country)"))

# 4. Multivariate analysis
amp_factors <- analyze_amr_factors(
  human_amr,
  target_pathogen = "Escherichia coli",
  target_antibiotic = "Ampicillin",
  predictors = c("year", "country", "setting", "population_type")
)

# Visualize results
or_plot <- ggplot2::ggplot(
  amp_factors$result_table[-1,], 
  ggplot2::aes(x = OR, y = Variable, xmin = CI_Lower, xmax = CI_Upper)
) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbarh(height = 0.2) +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  ggplot2::scale_x_log10() +
  ggplot2::labs(
    title = "Factors affecting E. coli resistance to ampicillin",
    x = "Odds Ratio (OR)",
    y = "Variable"
  ) +
  ggplot2::theme_minimal()

print(or_plot)
```

## Included Data

The metero package includes the `human_amr` dataset of human AMR data from South Asian countries, containing 591 records and 16 variables, covering resistance data for 4 pathogens and 35 antibiotics.

```r
# View data structure
data(human_amr)
str(human_amr)

# View data summary
summary(human_amr)

# View pathogen types
unique(human_amr$pathogen_name)

# View antibiotic types
unique(human_amr$antibiotic_name)

# View country distribution
table(human_amr$country)
```

## Contributing

Contributions via Github issues and pull requests are welcome. If you find a bug or have a feature suggestion, please submit an issue in the project repository.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact

Developer: Chaokun Hong
Email: chaokun.hong@ndm.ox.ac.uk

