# Script to install all dependencies for the metero package

# Required CRAN packages
cran_packages <- c(
  # Core dependencies
  "dplyr", "ggplot2", "scales", "stats", "tidyr", "tools", "utils",
  "meta", "metafor", "maps", "httr", "grDevices", "methods",
  
  # Development and testing
  "devtools", "roxygen2", "testthat", "knitr", "rmarkdown",
  
  # Suggested packages
  "readxl", "viridis", "mgcv", "jsonlite"
)

# Check if DMwR2 and mice are available on CRAN
if ("DMwR2" %in% rownames(available.packages())) {
  cran_packages <- c(cran_packages, "DMwR2")
} else {
  message("Package 'DMwR2' not found on CRAN. You may need to install from GitHub if required.")
}

if ("mice" %in% rownames(available.packages())) {
  cran_packages <- c(cran_packages, "mice")
} else {
  message("Package 'mice' not found on CRAN. You may need to install from GitHub if required.")
}

# Install missing CRAN packages
missing_packages <- cran_packages[!cran_packages %in% installed.packages()[,"Package"]]
if (length(missing_packages) > 0) {
  message("Installing missing packages from CRAN: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
} else {
  message("All CRAN packages are already installed.")
}

# Check if any GitHub packages are needed
if (!"DMwR2" %in% installed.packages()[,"Package"]) {
  message("Installing DMwR2 from GitHub")
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  devtools::install_github("cran/DMwR2")
}

# Check all packages can be loaded
message("\nValidating package installations:")
for (pkg in unique(c(cran_packages, "DMwR2"))) {
  result <- tryCatch({
    requireNamespace(pkg, quietly = TRUE)
    message(pkg, ": OK")
    TRUE
  }, error = function(e) {
    message(pkg, ": FAILED - ", conditionMessage(e))
    FALSE
  })
}

message("\nInstallation complete. You can now build and install the metero package.")
message("Run the following commands to build and install:")
message("devtools::document()")
message("devtools::install()") 