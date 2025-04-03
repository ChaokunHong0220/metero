# Analysis Help

## Meta-Analysis

metero provides powerful meta-analysis capabilities for calculating pooled resistance rates and related statistics:

### Analysis Methods

- **Random Effects Model**: Suitable when heterogeneity is expected across studies (typically preferred)
- **Fixed Effects Model**: Suitable when studies are expected to share the same true effect

### Grouping Options

You can group results by one or more of the following variables:

- **Pathogen**: Group by pathogen type (e.g., E. coli, K. pneumoniae)
- **Antibiotic**: Group by antibiotic (e.g., Ciprofloxacin, Ceftriaxone)
- **Country**: Group by country
- **Year**: Group by year of data collection

### Heterogeneity Analysis

Selecting the "Analyze Heterogeneity" option will calculate the following statistics:

- **I² Statistic**: Measures the percentage of variation across studies due to heterogeneity
- **Q Test**: Assesses whether observed heterogeneity is larger than expected by chance
- **H² Statistic**: Ratio of observed variance to expected variance
- **Tau²**: Estimate of between-study variance

### Subgroup Analysis

Selecting "Perform Subgroup Analysis" and choosing a subgroup variable allows you to:

- Examine differences between subgroups (such as different countries or years)
- Determine if differences between subgroups are significant
- Identify potential sources of heterogeneity

## Running Analysis

1. Select the appropriate meta-analysis method
2. Choose one or more grouping variables
3. Decide whether to analyze heterogeneity
4. Set subgroup analysis options if needed
5. Click the "Run Analysis" button

## Interpreting Results

After analysis is complete, you will see the following result tabs:

- **Summary**: Overall summary of the analysis
- **Pooled Rates**: Shows the calculated pooled resistance rates and confidence intervals
- **Forest Plot**: Graphical representation of the meta-analysis results
- **Heterogeneity**: Detailed heterogeneity statistics (if this option was selected)

### Interpreting Pooled Rates

- **Pooled Rate**: The combined estimate of resistance rate across studies
- **Confidence Interval**: 95% confidence interval for the pooled rate
- **Weight**: Relative contribution of each study to the pooled estimate
- **p-value**: Statistical significance 