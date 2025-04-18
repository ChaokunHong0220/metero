% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trend_analysis.R
\name{analyze_amr_trends}
\alias{analyze_amr_trends}
\title{Analyze AMR data time trends}
\usage{
analyze_amr_trends(
  data,
  time_var = "year",
  group_vars = NULL,
  min_studies = 1,
  smooth_method = "loess"
)
}
\arguments{
\item{data}{Standardized AMR data frame}

\item{time_var}{Time variable column name, default is "year"}

\item{group_vars}{Vector of variable names for grouping, such as c("pathogen_name", "country")}

\item{min_studies}{Minimum number of studies required for each time point, default is 1}

\item{smooth_method}{Trend line smoothing method, options: "loess" or "lm"}
}
\value{
A list containing time trend analysis results:
\item{trend_data}{Analyzed data frame with time points and corresponding resistance rates}
\item{plot}{Time trend visualization ggplot object}
\item{model_summary}{If smooth_method="lm", includes linear model summary}
}
\description{
This function analyzes changes in antimicrobial resistance (AMR) data over different time periods,
and can perform grouped analysis based on specified variables (such as pathogen, antibiotic, or country).
}
\examples{
\dontrun{
# Analyze E. coli resistance trends across different antibiotics
data(human_amr)
ec_data <- subset(human_amr, pathogen_name == "Escherichia coli")
trends <- analyze_amr_trends(ec_data, group_vars = "antibiotic_name")

# Show trend chart
print(trends$plot)
}
}
