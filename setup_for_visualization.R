# setup_for_visualization.R
# This script prepares the environment for running the ASF risk visualization Shiny app
# It checks for common issues and fixes them when possible

# Load required libraries
library(here)
library(sf)
library(dplyr)

# Check if required directories exist and create them if needed
check_create_dir <- function(path) {
  if (!dir.exists(path)) {
    message("Creating missing directory: ", path)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# Ensure all required directories exist
check_create_dir(here::here("output", "aqim total distributed risk"))
check_create_dir(here::here("output", "spatial layers", "risk at ports"))
check_create_dir(here::here("output", "spatial layers", "risk at final destinations"))

# Check for required distributed_risk_summary files
check_risk_summary_files <- function() {
  base_path <- here::here("output", "aqim total distributed risk")
  summary_file <- file.path(base_path, "distributed_risk_summary.csv")
  
  # If the main summary file doesn't exist, check for scenario-specific files
  if (!file.exists(summary_file)) {
    current_file <- file.path(base_path, "distributed_risk_summary_current.csv")
    mex_can_file <- file.path(base_path, "distributed_risk_summary_mex_can.csv")
    
    # If at least one scenario file exists, create a combined summary file
    if (file.exists(current_file) || file.exists(mex_can_file)) {
      message("Creating combined distributed_risk_summary.csv from scenario files...")
      
      # Load scenario files that exist
      summaries <- list()
      if (file.exists(current_file)) {
        summaries$current <- read.csv(current_file)
        summaries$current$scenario <- "current"
      }
      if (file.exists(mex_can_file)) {
        summaries$mex_can <- read.csv(mex_can_file)
        summaries$mex_can$scenario <- "mex_can"
      }
      
      # If any summaries were loaded, combine them
      if (length(summaries) > 0) {
        combined <- bind_rows(summaries)
        write.csv(combined, summary_file, row.names = FALSE)
        message("Combined summary file created at: ", summary_file)
      } else {
        message("No scenario summary files found to combine.")
      }
    } else {
      message("No distributed_risk_summary files found. The app may not function correctly.")
    }
  } else {
    message("Main distributed_risk_summary.csv file exists.")
  }
}

# Check for required layer_summary files for port risk tab
check_port_layer_summary_files <- function() {
  base_path <- here::here("output", "spatial layers", "risk at ports")
  
  # Check for scenario-specific summary files
  scenarios <- c("current", "mex_can", "EAN", "IBL", "other")
  found_any <- FALSE
  
  for (scenario in scenarios) {
    summary_file <- file.path(base_path, paste0("layer_summary_", scenario, ".csv"))
    if (file.exists(summary_file)) {
      message("Found port layer summary for scenario: ", scenario)
      found_any <- TRUE
    }
  }
  
  if (!found_any) {
    message("No port layer summary files found. The Port Risk tab may not function correctly.")
    
    # Try to create a basic layer summary from available gpkg files
    message("Attempting to create basic layer summary files from available gpkg files...")
    
    gpkg_files <- list.files(base_path, pattern = "*.gpkg$")
    if (length(gpkg_files) > 0) {
      # Extract scenario information from filenames
      scenario_pattern <- "_(current|mex_can|EAN|IBL|other)\\.gpkg$"
      scenarios_found <- unique(gsub(".*_(current|mex_can|EAN|IBL|other)\\.gpkg$", "\\1", 
                                    grep(scenario_pattern, gpkg_files, value = TRUE)))
      
      # For each found scenario, create a layer summary
      for (scenario in scenarios_found) {
        scenario_files <- grep(paste0("_", scenario, "\\.gpkg$"), gpkg_files, value = TRUE)
        
        if (length(scenario_files) > 0) {
          layer_summary <- data.frame(
            layer_name = gsub(paste0("_", scenario, "\\.gpkg$"), "", scenario_files),
            scenario = scenario,
            point_count = NA,
            total_risk = NA
          )
          
          # Add additional columns for filtering
          layer_summary$pathway <- sapply(strsplit(layer_summary$layer_name, "_"), function(x) if(length(x) > 0) x[1] else NA)
          layer_summary$container <- sapply(strsplit(layer_summary$layer_name, "_"), function(x) if(length(x) > 1) x[2] else NA)
          
          # Save the layer summary
          summary_file <- file.path(base_path, paste0("layer_summary_", scenario, ".csv"))
          write.csv(layer_summary, summary_file, row.names = FALSE)
          message("Created basic layer summary for scenario: ", scenario)
        }
      }
    }
  }
}

# Run all checks
message("Checking and preparing environment for ASF risk visualization app...")
check_risk_summary_files()
check_port_layer_summary_files()
message("Setup complete. You can now run the Shiny app.")
