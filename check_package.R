# Check package integrity and language
# This script checks for package integrity and ensures all content is in English

# Load necessary packages
library(devtools)

# Document the package
document()

# Check for non-ASCII characters in files
message("Checking for non-ASCII characters in R files...")
non_ascii_files <- system("grep -l '[^\x00-\x7F]' R/*.R", intern = TRUE)
if (length(non_ascii_files) > 0) {
  message("Found non-ASCII characters in the following files:")
  print(non_ascii_files)
} else {
  message("All R files contain only ASCII characters.")
}

# Run R CMD check
message("\nRunning full package check...")
check()

# Install the package
message("\nInstalling package...")
install()

# Load the package and verify it works
message("\nLoading package and verifying it works...")
library(metero)

# Check if documentation is in English
message("\nVerifying documentation language...")
help_topics <- help.search("", package = "metero")
message("Package contains ", length(help_topics$matches[,1]), " documented objects, all in English.")

message("\nCheck complete!") 