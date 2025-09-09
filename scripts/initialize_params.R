# Initialize ASF Risk Framework Parameters
# This script serves as a wrapper to generate the shared parameter file from the 
# 00_asf_risk_parameters.Rmd file. This ensures a single source of truth for parameters.

# Initialize ASF Risk Framework Parameters
# This script serves as a wrapper to generate the shared parameter file from the 
# 00_asf_risk_parameters.Rmd file. This ensures a single source of truth for parameters.

# Load necessary packages
library(here)
library(rmarkdown)

# Create output directory if it doesn't exist
dir.create(here::here("output", "intermediate files"), showWarnings = FALSE, recursive = TRUE)

# Source the parameters by knitting the Rmd file
params_rmd_path <- here::here("scripts", "00_asf_risk_parameters.Rmd")

if (!file.exists(params_rmd_path)) {
  stop("Parameter file not found at: ", params_rmd_path)
}

# Render the Rmd file silently to generate the parameters
temp_output <- tempfile(fileext = ".html")
rmarkdown::render(
  params_rmd_path,
  output_file = temp_output,
  quiet = TRUE
)

# Verify that parameters were created
params_file <- here::here("output", "intermediate files", "asf_risk_params.rds")
if (!file.exists(params_file)) {
  stop("Failed to generate parameter file. Check 00_asf_risk_parameters.Rmd for errors.")
}

# Load and display parameter values
asf_params <- readRDS(params_file)

# Display important simulation parameters
cat("ASF Risk Framework parameters have been initialized from 00_asf_risk_parameters.Rmd\n")
cat("Parameters saved to:", params_file, "\n\n")
cat("Key parameter values:\n")
cat("- Simulation runs (n_sims_for_volume):", asf_params$n_sims_for_volume, "\n")
cat("- US grid cell size:", asf_params$US_grid_cell_size, "\n")
cat("- Date range:", asf_params$begin_date, "to", asf_params$end_date, "\n")
cat("- Risk quantification method:", asf_params$risk_quantification, "\n")

cat("\nTotal number of parameters defined:", length(asf_params), "\n")
cat("To run the pipeline with these parameters, use: source('scripts/run_pipeline.R'); run_pipeline()\n")
