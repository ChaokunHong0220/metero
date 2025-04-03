# Frequently Asked Questions

## General Questions

### What is METEOR?
METEOR (Antimicrobial Resistance Meta-analysis and Integrated Analysis Toolkit) is an R package designed to facilitate the analysis of antimicrobial resistance (AMR) data. It provides tools for data import, standardization, analysis, and visualization.

### Who is METEOR for?
METEOR is designed for researchers, epidemiologists, public health professionals, and clinicians who work with AMR data, particularly those interested in conducting meta-analyses or comparing data across different studies or sources.

### Does METEOR require programming skills?
While METEOR is built in R, the Shiny application provides a user-friendly interface that doesn't require R programming knowledge. However, familiarity with basic statistical concepts and AMR data would be beneficial.

## Data-Related Questions

### What types of AMR data can I analyze with METEOR?
METEOR can analyze human AMR data (with future support planned for animal and environmental data). It supports various formats including structured datasets with resistance rates or raw counts.

### How should I format my data for METEOR?
Your data should include at minimum: pathogen names, antibiotic names, and either resistance rates or sample counts and resistant counts. Additional metadata like country, year, and study information will enable more comprehensive analyses.

### Can METEOR handle data with antibiotic abbreviations?
Yes, METEOR includes functionality to convert antibiotic abbreviations (like AMP for Ampicillin) to full names. It also provides WHO AWaRe classification for antibiotics.

### What should I do if my data has missing values?
METEOR can handle datasets with some missing values. The data quality assessment will highlight completeness issues, and for certain calculations (like resistance rates), the package can derive values if you have the necessary component data.

## Analysis Questions

### What meta-analysis methods does METEOR support?
METEOR supports both random effects and fixed effects models for meta-analysis of AMR data, with appropriate weighting based on sample sizes.

### How does METEOR assess heterogeneity?
METEOR calculates several heterogeneity statistics including I², Q-test, H², and Tau². These metrics help you understand the variability between studies or data sources.

### Can I perform subgroup analyses?
Yes, you can perform subgroup analyses based on factors like country, region, year, or other metadata variables to identify differences between groups.

## Visualization Questions

### What types of visualizations does METEOR offer?
METEOR provides several visualization types:
- Geographic maps (world or South Asia focused)
- Forest plots for meta-analysis results
- Resistance heatmaps
- Time trend plots

### Can I customize the visualizations?
Yes, each visualization type offers several customization options such as color schemes, clustering methods, smoothing techniques, and more.

### How can I focus on South Asian data?
When using the geographic map visualization, you can select "South Asia" as the map region to focus specifically on countries in this region.

## Technical Support

### What should I do if I encounter an error?
If you encounter errors, first check that your data is properly formatted and that you've mapped the columns correctly. Most errors are related to data structure or missing required fields.

### How can I report bugs or suggest features?
Please report bugs or feature requests through the GitHub repository issue tracker at [github.com/yourusername/metero](https://github.com/yourusername/metero). 