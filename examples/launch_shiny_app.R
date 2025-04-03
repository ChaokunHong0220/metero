# Example script to launch the metero Shiny application
library(metero)

# Load example AMR data from South Asia (simulated data)
# In a real scenario, you would import your own data

# Create simulated data for South Asia
south_asia_amr <- data.frame(
  year = rep(2020, 10),
  region = rep("South Asia", 10),
  country = rep("India", 10),
  r_AML_Ecoli = c(0.45), # Amoxyclav (AML) for E.coli resistance rate 
  n_AML_Ecoli = c(200),  # Amoxyclav (AML) for E.coli total sample count
  d_AML_Ecoli = c(90),   # Amoxyclav (AML) for E.coli resistant sample count
  r_AMP_Ecoli = c(0.63), # Ampicillin (AMP) for E.coli resistance rate
  n_AMP_Ecoli = c(200),
  d_AMP_Ecoli = c(126),
  r_CIP_Ecoli = c(0.58), # Ciprofloxacin (CIP) for E.coli resistance rate
  n_CIP_Ecoli = c(200),
  d_CIP_Ecoli = c(116),
  r_CTX_Ecoli = c(0.52), # Cefotaxime (CTX) for E.coli resistance rate
  n_CTX_Ecoli = c(200),
  d_CTX_Ecoli = c(104),
  r_MEM_Ecoli = c(0.15), # Meropenem (MEM) for E.coli resistance rate
  n_MEM_Ecoli = c(200),
  d_MEM_Ecoli = c(30)
)

# Option 1: Convert wide format to long format before loading into Shiny app
long_data <- amr_wide_to_long(
  south_asia_amr,
  expand_codes = TRUE,
  add_who_class = TRUE
)

print("Preview of the long format data:")
print(head(long_data))

# Option 2: Launch the Shiny app with the raw data
# You can either use the original wide format data or the converted long format data
cat("\nLaunching Shiny app...\n")
cat("You can import the data through the UI or use the example below:\n")
cat("launch_metero(data = long_data)\n\n")

# Uncomment the line below to actually launch the app with the data
# launch_metero(data = long_data)

# To launch app without pre-loaded data:
# launch_metero() 