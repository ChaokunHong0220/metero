% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trend_analysis.R
\name{analyze_amr_factors}
\alias{analyze_amr_factors}
\title{Perform multivariate analysis of antimicrobial resistance data}
\usage{
analyze_amr_factors(
  data,
  target_pathogen,
  target_antibiotic,
  predictors,
  min_samples = 30
)
}
\arguments{
\item{data}{Standardized AMR data frame}

\item{target_pathogen}{Target pathogen name}

\item{target_antibiotic}{Target antibiotic name}

\item{predictors}{Vector of predictor variable names, e.g., c("year", "country", "setting")}

\item{min_samples}{Minimum sample size to include in analysis, default is 30}
}
\value{
Logistic regression model results list
}
\description{
This function uses logistic regression to analyze various factors affecting antimicrobial resistance.
}
\examples{
\dontrun{
data(human_amr)
# Analyze factors affecting E. coli resistance to ciprofloxacin
model_result <- analyze_amr_factors(
  human_amr,
  target_pathogen = "Escherichia coli",
  target_antibiotic = "Ciprofloxacin",
  predictors = c("year", "country", "setting", "population_type")
)

# View model summary
print(model_result$summary)
}
}
