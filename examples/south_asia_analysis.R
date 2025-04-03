# South Asia AMR Data Analysis Example
library(metero)
library(ggplot2)

# Create simulated data for South Asia region
south_asia_amr <- data.frame(
  year = rep(c(2018, 2019, 2020), each = 4),
  region = rep("South Asia", 12),
  country = rep(c("India", "Pakistan", "Bangladesh", "Nepal"), 3),
  r_AML_Ecoli = c(0.45, 0.42, 0.48, 0.41, 0.47, 0.44, 0.50, 0.43, 0.48, 0.46, 0.52, 0.45),
  n_AML_Ecoli = c(200, 180, 150, 120, 210, 190, 160, 130, 220, 200, 170, 140),
  d_AML_Ecoli = c(90, 76, 72, 49, 99, 84, 80, 56, 106, 92, 88, 63),
  r_CIP_Ecoli = c(0.58, 0.55, 0.60, 0.52, 0.60, 0.57, 0.62, 0.54, 0.63, 0.60, 0.64, 0.56),
  n_CIP_Ecoli = c(200, 180, 150, 120, 210, 190, 160, 130, 220, 200, 170, 140),
  d_CIP_Ecoli = c(116, 99, 90, 62, 126, 108, 99, 70, 139, 120, 109, 78),
  r_CTX_Ecoli = c(0.52, 0.48, 0.56, 0.50, 0.54, 0.51, 0.58, 0.52, 0.57, 0.53, 0.60, 0.54),
  n_CTX_Ecoli = c(200, 180, 150, 120, 210, 190, 160, 130, 220, 200, 170, 140),
  d_CTX_Ecoli = c(104, 86, 84, 60, 113, 97, 93, 68, 125, 106, 102, 76),
  r_MEM_Ecoli = c(0.15, 0.12, 0.18, 0.14, 0.17, 0.13, 0.20, 0.16, 0.19, 0.15, 0.22, 0.18),
  n_MEM_Ecoli = c(200, 180, 150, 120, 210, 190, 160, 130, 220, 200, 170, 140),
  d_MEM_Ecoli = c(30, 22, 27, 17, 36, 25, 32, 21, 42, 30, 37, 25),
  r_CIP_Kpneumo = c(0.64, 0.60, 0.68, 0.62, 0.66, 0.62, 0.70, 0.64, 0.68, 0.65, 0.72, 0.66),
  n_CIP_Kpneumo = c(150, 140, 120, 100, 160, 150, 130, 110, 170, 160, 140, 120),
  d_CIP_Kpneumo = c(96, 84, 82, 62, 106, 93, 91, 70, 116, 104, 101, 79),
  r_MEM_Kpneumo = c(0.28, 0.25, 0.32, 0.26, 0.30, 0.27, 0.34, 0.28, 0.33, 0.30, 0.36, 0.30),
  n_MEM_Kpneumo = c(150, 140, 120, 100, 160, 150, 130, 110, 170, 160, 140, 120),
  d_MEM_Kpneumo = c(42, 35, 38, 26, 48, 41, 44, 31, 56, 48, 50, 36)
)

# Step 1: Convert wide format to long format
cat("Converting wide format data to long format...\n")
long_data <- amr_wide_to_long(
  south_asia_amr,
  expand_codes = TRUE,
  add_who_class = TRUE
)

# Print summary of data
cat("\nData summary:\n")
cat("Number of records:", nrow(long_data), "\n")
cat("Countries:", paste(unique(long_data$country), collapse=", "), "\n")
cat("Years:", paste(unique(long_data$year), collapse=", "), "\n")
cat("Pathogens:", paste(unique(long_data$pathogen), collapse=", "), "\n")
cat("Antibiotics:", paste(unique(long_data$antibiotic_name), collapse=", "), "\n\n")

# Step 2: Create a mapping for standardization
mapping <- list(
  year = "year",
  region = "region",
  country = "country",
  antibiotic_name = "antibiotic_name",
  pathogen_name = "pathogen",
  resistance_rate = "resistance_rate",
  sample_count = "sample_count",
  resistant_count = "resistant_count"
)

# Step 3: Import data using the standard function
cat("Standardizing data...\n")
std_data <- import_amr_data(
  data = long_data,
  mapping = mapping,
  domain = "human"
)

# Step 4: Create a South Asia map visualization using the specialized function
cat("Creating South Asia map for E. coli resistance to Ciprofloxacin...\n")
cat("(This would display a visualization if run in an interactive session)\n\n")
south_asia_map <- create_south_asia_map(
  data = std_data,
  pathogen = "Ecoli",
  antibiotic = "Ciprofloxacin",
  color_palette = "red_blue",
  title = "E. coli Resistance to Ciprofloxacin in South Asia (2018-2020)"
)

# Alternative: Using the regular geo_map function with south_asia region
cat("Alternative method using create_geo_map with south_asia region...\n")
regular_map <- create_geo_map(
  data = std_data,
  pathogen = "Ecoli",
  antibiotic = "Ciprofloxacin",
  map_region = "south_asia",
  color_palette = "red_blue"
)

# Step 5: Create trend analysis across years
cat("Creating resistance trend analysis by country...\n")
cat("(This would display a visualization if run in an interactive session)\n\n")
trend_plot <- create_trend_plot(
  data = std_data[std_data$pathogen == "Ecoli" & 
                std_data$antibiotic_name == "Ciprofloxacin", ],
  time_var = "year",
  group_vars = "country",
  smooth_method = "loess",
  conf_int = TRUE
)

# Step 6: Analyze WHO AWaRe categories
cat("Analyzing resistance by WHO AWaRe classification...\n")
who_summary <- aggregate(
  resistance_rate ~ who_class + year, 
  data = long_data, 
  FUN = function(x) mean(x, na.rm = TRUE)
)

print(who_summary)

cat("\nResistance to 'Watch' category antibiotics appears to be higher than 'Access' category.\n")
cat("This is consistent with WHO guidelines encouraging use of 'Access' antibiotics as first-line treatments.\n\n")

# Step 7: Save plots if needed
cat("To save visualizations, you could use:\n")
cat("ggplot2::ggsave('south_asia_map.png', south_asia_map, width = 10, height = 8, dpi = 300)\n")
cat("ggplot2::ggsave('resistance_trends.png', trend_plot, width = 10, height = 6, dpi = 300)\n\n")

cat("To perform meta-analysis with this data in the Shiny application, run:\n")
cat("launch_metero(data = std_data)\n") 