#' @keywords internal
"_PACKAGE"

#' @name metero-package
#' @title Meta-analysis Tool for Exploring Antimicrobial Resistance Outcomes Across Realms
#'
#' @description A comprehensive tool for analysis and visualization of antimicrobial resistance (AMR)
#' meta-analysis data. The package implements the One Health approach, integrating human,
#' animal, and environmental AMR data.
#'
#' @section Core Functions:
#' \itemize{
#'   \item \code{\link{standardize_amr_data}}: Convert user data to the standard metero format
#'   \item \code{\link{get_standard_amr_structure}}: Get information about the standard data structure
#'   \item \code{\link{compare_with_package_data}}: Compare user-imported data with package data
#'   \item \code{\link{plot_amr_comparison}}: Create visualizations comparing AMR datasets
#'   \item \code{\link{analyze_amr_trends}}: Analyze AMR trends over time
#'   \item \code{\link{create_amr_heatmap}}: Create heatmap of AMR rates
#'   \item \code{\link{analyze_amr_factors}}: Analyze factors affecting AMR rates
#' }
#'
#' @section Included Datasets:
#' \itemize{
#'   \item \code{\link{human_amr}}: AMR data from human populations
#' }
#'
#' @author Chaokun Hong \email{chaokun.hong@ndm.ox.ac.uk}
NULL
