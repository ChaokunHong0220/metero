# Visualization Examples for metero Package
# This script demonstrates the various visualization capabilities of the metero package

# Load required packages
library(metero)
library(ggplot2)
library(dplyr)

# Load data
data(human_amr)
data(metadata)

# ---- Resistance Heatmap Examples ----

# Basic heatmap of all data
basic_heatmap <- create_amr_heatmap(human_amr)
print(basic_heatmap + 
      labs(title = "Overall Antimicrobial Resistance Rates"))

# Filter heatmap by country
india_heatmap <- create_amr_heatmap(
  human_amr, 
  filter_conditions = list(country = "India")
)
print(india_heatmap + 
      labs(title = "Antimicrobial Resistance Rates in India"))

# Filter by population type
children_heatmap <- create_amr_heatmap(
  human_amr, 
  filter_conditions = list(population_type = "CHILDREN")
)
print(children_heatmap + 
      labs(title = "Antimicrobial Resistance Rates in Children"))

# Change color palette
redblue_heatmap <- create_amr_heatmap(
  human_amr,
  color_palette = "red_blue",
  threshold = 0.5
)
print(redblue_heatmap + 
      labs(title = "Resistance Rates with Red-Blue Color Palette"))

# ---- Forest Plot Examples ----

# Perform meta-analysis for E. coli data
ec_data <- human_amr %>%
  filter(pathogen == "Escherichia coli")

ec_meta <- calculate_pooled_rate(
  data = ec_data,
  by = c("specific_antibiotic"),
  method = "random"
)

# Create forest plot
ec_forest <- create_forest_plot(ec_meta)
print(ec_forest)

# Forest plot sorted by resistance rate
ec_forest_sorted <- create_forest_plot(
  ec_meta, 
  sort_by = "rate"
)
print(ec_forest_sorted)

# Forest plot with points sized by number of studies
ec_forest_weighted <- create_forest_plot(
  ec_meta,
  weights = TRUE
)
print(ec_forest_weighted)

# ---- Geographic Map Examples ----

# Create map of E. coli resistance to ciprofloxacin
ec_cip_map <- create_geo_map(
  data = human_amr,
  pathogen = "Escherichia coli",
  antibiotic = "Ciprofloxacin",
  map_region = "south_asia"
)
print(ec_cip_map)

# Create map for S. aureus
sa_map <- create_geo_map(
  data = human_amr,
  pathogen = "Staphylococcus aureus",
  map_region = "south_asia",
  color_palette = "red_green"
)
print(sa_map)

# ---- Time Trend Examples ----

# Create trend plot for E. coli
ec_trend <- create_trend_plot(
  data = ec_data,
  time_var = "year",
  group_vars = c("specific_antibiotic"),
  smooth_method = "loess"
)
print(ec_trend + 
      labs(title = "E. coli Resistance Trends Over Time"))

# Create trend plot for different pathogens against ciprofloxacin
cip_data <- human_amr %>%
  filter(specific_antibiotic == "Ciprofloxacin")

cip_trend <- create_trend_plot(
  data = cip_data,
  time_var = "year",
  group_vars = c("pathogen"),
  smooth_method = "lm"
)
print(cip_trend + 
      labs(title = "Ciprofloxacin Resistance Trends By Pathogen"))

# ---- Combined Visualizations ----

# Combine meta-analysis with visualization
# Analyze resistance by setting
setting_analysis <- perform_subgroup_analysis(
  data = human_amr,
  by = c("pathogen"),
  subgroups = c("setting"),
  min_studies = 1
)

# Visualize setting differences
setting_data <- setting_analysis$subgroup_results$summary

# Convert to plotting format
setting_plot_data <- setting_data %>%
  mutate(
    setting = factor(setting, 
                   levels = c("HOSPITAL", "URBAN", "RURAL", "MIXED", "UNKNOWN")),
    pathogen = factor(pathogen)
  )

# Create grouped bar chart
setting_plot <- ggplot(setting_plot_data, 
                     aes(x = pathogen, y = pooled_rate*100, fill = setting)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Resistance Rates by Setting and Pathogen",
    x = "Pathogen",
    y = "Resistance Rate (%)",
    fill = "Setting"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(setting_plot)

# Save plots to PDF for reference
if (interactive()) {
  pdf("metero_visualization_examples.pdf", width = 10, height = 8)
  
  print(basic_heatmap + labs(title = "Overall AMR Heatmap"))
  print(india_heatmap + labs(title = "India AMR Heatmap"))
  print(ec_forest + labs(title = "E. coli Forest Plot"))
  print(ec_cip_map + labs(title = "E. coli Ciprofloxacin Map"))
  print(ec_trend + labs(title = "E. coli Resistance Trends"))
  print(setting_plot + labs(title = "Resistance by Setting"))
  
  dev.off()
  
  message("Plots saved to metero_visualization_examples.pdf")
} 