# Trend Analysis Example for metero Package
# This script demonstrates how to use the metero package to analyze antimicrobial resistance data trends

# Load required packages
library(metero)
library(ggplot2)
library(dplyr)

# Load data
data(human_amr)

# 1. Analyze E. coli resistance trends to different antibiotics
# ----------------------------------------------
# Filter E. coli data
ec_data <- subset(human_amr, pathogen_name == "Escherichia coli")

# Analyze trends grouped by antibiotic
ec_trends <- analyze_amr_trends(
  ec_data,
  time_var = "year",
  group_vars = "antibiotic_name",
  min_studies = 1,
  smooth_method = "loess"
)

# Display trend chart
print(ec_trends$plot +
        labs(title = "E. coli Resistance Trends for Different Antibiotics") +
        theme(legend.position = "bottom"))

# You can save the chart
# ggsave("E_coli_trends.png", ec_trends$plot, width = 10, height = 7)

# 2. Create pathogen-antibiotic resistance heatmap
# ----------------------------------------------
# Create heatmap of all data
amr_heatmap <- create_amr_heatmap(human_amr)

# Display heatmap
print(amr_heatmap +
        labs(title = "Major Pathogen-Antibiotic Resistance Rates in South Asia"))

# 3. Analyze resistance differences across countries
# ----------------------------------------------
# Create heatmap by country
india_heatmap <- create_amr_heatmap(
  human_amr,
  filter_conditions = list(country = "India")
)

# Display heatmap
print(india_heatmap +
        labs(title = "Pathogen-Antibiotic Resistance Rates in India"))

# 4. Multivariate analysis: Factors affecting E. coli resistance to ciprofloxacin
# ----------------------------------------------------
# Create model - Analyze effects of year, country, and population type on resistance
if (nrow(subset(human_amr, 
                pathogen_name == "Escherichia coli" & 
                antibiotic_name == "Ciprofloxacin")) > 30) {
  
  model_result <- analyze_amr_factors(
    human_amr,
    target_pathogen = "Escherichia coli",
    target_antibiotic = "Ciprofloxacin",
    predictors = c("year", "country", "population_type"),
    min_samples = 30
  )
  
  # View results table
  print(model_result$result_table)
  
  # Create forest plot of odds ratios
  or_plot <- ggplot(model_result$result_table[-1,], # Remove intercept
                     aes(x = OR, y = Variable, xmin = CI_Lower, xmax = CI_Upper)) +
    geom_point() +
    geom_errorbarh(height = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    scale_x_log10() +
    labs(title = "Factors Affecting E. coli Resistance to Ciprofloxacin",
         x = "Odds Ratio (OR)",
         y = "Variable") +
    theme_minimal()
  
  print(or_plot)
}

# 5. Comparison of resistance rates across different age groups
# ----------------------------------------------
# Analyze resistance rates by population type
age_groups <- analyze_amr_trends(
  human_amr,
  time_var = "year",
  group_vars = c("population_type"),
  min_studies = 1,
  smooth_method = "lm"
)

# Display population group trend chart
print(age_groups$plot +
        labs(title = "Antimicrobial Resistance Trends in Different Populations") +
        theme(legend.position = "bottom"))

# View linear model summary
print(age_groups$model_summary) 