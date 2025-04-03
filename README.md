# metero

## Description

metero is an R package for analyzing antimicrobial resistance (AMR) data. The package provides tools for standardizing, analyzing, and visualizing AMR data to help researchers and public health professionals understand AMR distribution and trends globally.

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

### 1. Data Import and Standardization ✅

metero provides robust data import and standardization capabilities:

- **Multiple File Format Support**: Import from CSV, Excel, or R data files
- **Antibiotic Code Handling**: Automatic processing of antibiotic abbreviations (e.g., AML for Amoxyclav, AMP for Ampicillin)
- **WHO AWaRe Classification**: Integration with the WHO antibiotic classification system (Access, Watch, Reserve)
- **Wide-to-Long Format Conversion**: Process common wide-format AMR data
  - Support for patterns like `r_AML_Ecoli` (resistance rate of E. coli to Amoxyclav)
  - Support for count data in patterns like `n_AML_Ecoli` (total samples) and `d_AML_Ecoli` (resistant samples)

```r
# Import AMR data example
amr_data <- import_amr_data(
  "your_data.csv",
  mapping = list(
    pathogen = "bacteria",
    antibiotic = "drug",
    resistance_rate = "rate"
  ),
  domain = "human"
)

# Convert wide format to long format example
long_data <- amr_wide_to_long(
  wide_data,
  expand_codes = TRUE,
  add_who_class = TRUE
)
```

### 2. Interactive Exploration via Shiny Application ✅

metero includes a full-featured Shiny application for interactive data exploration:

- **Data Import Interface**: Upload and configure your AMR data
- **Visualization Dashboard**: Generate and customize visualizations
- **Interactive Mapping**: Create geographical visualizations of AMR patterns
- **Analysis Tools**: Perform analyses within the application

```r
# Launch the Shiny application
launch_metero()

# Launch with pre-loaded data
data <- import_amr_data("my_amr_data.csv")
launch_metero(data = data)
```

### 3. AMR Data Analysis ⚠️ (Under Development)

Meta-analysis and trend analysis features for AMR data:

```r
# Calculate pooled resistance rates
meta_results <- calculate_pooled_rate(
  data = amr_data,
  by = c("pathogen", "specific_antibiotic"),
  method = "random"
)

# Analyze heterogeneity
heterogeneity <- analyze_heterogeneity(
  data = amr_data,
  by = c("pathogen", "specific_antibiotic"),
  moderators = c("country")
)
```

### 4. Advanced Visualization ⚠️ (Under Development)

Create informative visualizations to understand AMR patterns:

```r
# Create a resistance heatmap
heatmap <- create_amr_heatmap(
  data = amr_data,
  sort_pathogens = "resistance",
  sort_antibiotics = "class"
)

# Create a geographic map of AMR data
map <- create_geo_map(
  data = amr_data,
  pathogen = "Escherichia coli",
  antibiotic = "Ciprofloxacin"
)
```

### 5. One Health Integration ⚠️ (Under Development)

Future features for integrating human, animal, and environmental AMR data:

- Cross-domain analysis
- Transmission pathway visualization
- Integrated reporting

## Included Data

The metero package includes sample AMR datasets to demonstrate functionality.

```r
# View data structure
data(human_amr)
str(human_amr)

# View antibiotic classification
data(antibiotic_classification)
head(antibiotic_classification)
```

## Data Model

metero implements a standardized data model for AMR data:

```r
# View the data model
model <- get_data_model()

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

