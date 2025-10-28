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

# Get the list of pipeline scripts from set_asf_risk_parameters.Rmd
get_pipeline_scripts <- function() {
  # First check if we have the parameters file which should have the script list
  params_file <- here::here("output", "intermediate files", "asf_risk_params.rds")
  
  # Try to load the scripts list
  if (file.exists(params_file)) {
    params <- readRDS(params_file)
    if (!is.null(params$pipeline_scripts)) {
      return(params$pipeline_scripts)
    }
  }
  
  # If we can't get it from params, use the hardcoded list matching set_asf_risk_parameters.Rmd
  return(c(
    "01_01 aqim pathway passenger layers.Rmd",
    "01_02 aqim pathway data prep.Rmd",
    "01_03 aqim pathway approach rates.Rmd",
    "01_04 aqim volume distribution.Rmd",
    "01_05 aqim pathway passenger volumes.Rmd",
    "01_06 aqim pathway risk_PoE.Rmd",
    "01_07 aqim volume spatial layers.Rmd",
    "03_01 ean data.Rmd",
    "03_02 ean geolocation.Rmd",
    "03_03 ean port and destination layers.Rmd",
    "04_01 IBL data.Rmd",
    "04_02 IBL port and destination layers.Rmd",
    "04_03 IBL report output.R"
  ))
}

# Function to run a specific script with filtered parameters
run_script <- function(script_number, params_override = NULL) {
  # Get the list of pipeline scripts
  pipeline_scripts <- get_pipeline_scripts()
  
  # Check if script_number is valid
  if (script_number < 1 || script_number > length(pipeline_scripts)) {
    stop("Invalid script number: ", script_number, 
         ". Must be between 1 and ", length(pipeline_scripts))
  }
  
  # Get the script file name
  script_file <- pipeline_scripts[script_number]
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

# Function to run the entire pipeline or a section of it
run_pipeline <- function(start_script = 1, end_script = NULL, params_override = NULL) {
  # Get the list of pipeline scripts
  pipeline_scripts <- get_pipeline_scripts()
  
  # If end_script is not specified, run all scripts from start_script to the end
  if (is.null(end_script)) {
    end_script <- length(pipeline_scripts)
  }
  
  # Validate script range
  if (start_script < 1 || start_script > length(pipeline_scripts)) {
    stop("Invalid start_script: ", start_script, 
         ". Must be between 1 and ", length(pipeline_scripts))
  }
  
  if (end_script < start_script || end_script > length(pipeline_scripts)) {
    stop("Invalid end_script: ", end_script, 
         ". Must be between ", start_script, " and ", length(pipeline_scripts))
  }
  
  # Create output directory for HTML reports
  output_dir <- here::here("output", "html_reports")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat(paste0("Running pipeline scripts ", start_script, " to ", end_script, ":\n"))
  for (i in start_script:end_script) {
    cat(paste0(i, ". ", pipeline_scripts[i], "\n"))
  }
  cat("\n")
  
  # Run each script in sequence
  for (i in start_script:end_script) {
    tryCatch({
      cat(paste0("\n--- Running script ", i, "/", end_script, " ---\n"))
      run_script(i, params_override)
    }, error = function(e) {
      cat("Error running script", i, ":", e$message, "\n")
      cat("Continuing with next script...\n")
    })
  }
  
  cat("\nPipeline execution completed.\n")
}

# Example usage:
# Source this file: source("scripts/run_pipeline.R")
# Then:
# - run_pipeline() - Run the entire pipeline (all 10 scripts)
# - run_pipeline(start_script = 1, end_script = 7) - Run only the AQIM scripts (1-7)
# - run_pipeline(start_script = 8, end_script = 10) - Run only the EAN scripts (8-10)
# - run_script(3) - Run just script #3 (01_03 aqim pathway approach rates.Rmd)
# - run_script(8) - Run just script #8 (03_01 ean data.Rmd)
