# ASF Risk Framework Pipeline Runner
# This script allows running the entire ASF risk assessment pipeline 
# or individual components with shared parameters.
# 
# The parameter system uses a single source of truth:
# 1. 00_asf_risk_parameters.Rmd - Defines all parameters (source of truth)
# 2. initialize_params.R - Renders the Rmd file to generate parameter file
# 3. This script (run_pipeline.R) - Uses the parameters to run the pipeline
# Run ASF Risk Framework Pipeline
# This script provides functions to run the entire ASF Risk Framework pipeline
# or individual scripts using shared parameters.

library(here)
library(rmarkdown)
library(yaml)

# Function to extract parameter names from an Rmd file's YAML header
extract_param_names <- function(rmd_path) {
  if (!file.exists(rmd_path)) {
    stop("File not found: ", rmd_path)
  }
  
  # Read the Rmd file
  rmd_lines <- readLines(rmd_path, n = 100)  # Read first 100 lines to find YAML header
  
  # Find YAML header boundaries
  yaml_start <- which(rmd_lines == "---")[1]
  yaml_end <- which(rmd_lines == "---")[2]
  
  if (length(yaml_start) == 0 || length(yaml_end) == 0) {
    warning("Could not find YAML header in ", rmd_path)
    return(character(0))
  }
  
  # Extract YAML content
  yaml_content <- rmd_lines[(yaml_start+1):(yaml_end-1)]
  
  # Parse YAML
  tryCatch({
    yaml_data <- yaml::yaml.load(paste(yaml_content, collapse = "\n"))
    if (!is.null(yaml_data$params)) {
      return(names(yaml_data$params))
    } else {
      return(character(0))
    }
  }, error = function(e) {
    warning("Error parsing YAML in ", rmd_path, ": ", e$message)
    return(character(0))
  })
}

# Function to run a specific script with filtered parameters
run_script <- function(script_number, params_override = NULL) {
  # Format script number with leading zero if needed
  script_num_str <- sprintf("%02d", as.integer(script_number))
  
  # Find the script file
  script_pattern <- paste0("^01_", script_num_str, ".*\\.Rmd$")
  script_files <- list.files(here::here("scripts"), pattern = script_pattern)
  
  if (length(script_files) == 0) {
    stop("Script not found: ", script_pattern)
  }
  
  script_file <- script_files[1]
  script_path <- here::here("scripts", script_file)
  
  cat("Running script:", script_file, "\n")
  
  # Load shared parameters
  params_file <- here::here("output", "intermediate files", "asf_risk_params.rds")
  if (!file.exists(params_file)) {
    stop("Shared parameter file not found. Run initialize_params.R first.")
  }
  
  all_params <- readRDS(params_file)
  
  # Override parameters if provided
  if (!is.null(params_override) && is.list(params_override)) {
    for (param_name in names(params_override)) {
      all_params[[param_name]] <- params_override[[param_name]]
    }
  }
  
  # Extract parameter names from the script
  script_param_names <- extract_param_names(script_path)
  
  # Filter parameters to only those used by the script
  if (length(script_param_names) > 0) {
    filtered_params <- all_params[names(all_params) %in% script_param_names]
    
    # Check if we're missing any parameters declared in the script
    missing_params <- script_param_names[!script_param_names %in% names(filtered_params)]
    if (length(missing_params) > 0) {
      warning("The following parameters declared in ", script_file, 
              " are not found in the shared parameters: ", 
              paste(missing_params, collapse = ", "))
    }
  } else {
    # If no parameters found in YAML, pass an empty list
    filtered_params <- list()
    cat("No parameters found in YAML header of", script_file, "\n")
  }
  
  # Create output directory if it doesn't exist
  output_dir <- here::here("output", "html_reports")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Render the script with filtered parameters
  cat("Rendering", script_file, "with filtered parameters\n")
  
  output_file <- paste0(tools::file_path_sans_ext(script_file), "_output.html")
  
  rmarkdown::render(
    script_path,
    output_file = here::here("output", "html_reports", output_file),
    params = filtered_params,
    envir = new.env()
  )
  
  cat("Completed running:", script_file, "\n")
}

# Function to run the entire pipeline
run_pipeline <- function(start_script = 1, end_script = 7, params_override = NULL) {
  # Create output directory for HTML reports
  output_dir <- here::here("output", "html_reports")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Run each script in sequence
  for (i in start_script:end_script) {
    tryCatch({
      run_script(i, params_override)
    }, error = function(e) {
      cat("Error running script", i, ":", e$message, "\n")
      cat("Continuing with next script...\n")
    })
  }
  
  cat("Pipeline execution completed.\n")
}

# Example usage:
# Source this file: source("scripts/run_pipeline.R")
# Then:
# run_pipeline() - Run the entire pipeline
# run_script(3) - Run the third script only
# - run_pipeline() to run everything
# - run_script(3) to run just the third script
