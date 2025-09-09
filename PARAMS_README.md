# ASF Risk Framework Parameter System

This document explains how the parameter system works in the ASF Risk Framework.

## Overview

The ASF Risk Framework uses a parameter system that allows you to:

1. Define all parameters in one central place
2. Use consistent parameter values across all scripts
3. Run individual scripts or the entire pipeline with the same parameters
4. Override parameter values when needed

## How It Works

1. **Single Source of Truth**: All parameters are defined in `00_asf_risk_parameters.Rmd`
2. **Parameter Storage**: Parameters are saved as an RDS file in `output/intermediate files/asf_risk_params.rds`
3. **Intelligent Parameter Passing**: When scripts are run, they only receive the parameters they need
4. **Default Values**: Each script defines default values for its parameters in case shared parameters aren't available

## Files in the Parameter System

- **`00_asf_risk_parameters.Rmd`**: Defines all parameters used across the pipeline
- **`initialize_params.R`**: Script to generate and save the shared parameter file
- **`run_pipeline.R`**: Functions to run scripts with the correct parameters

## Using the Parameter System

### 1. Initialize Parameters

First, run the initialization script to create the shared parameter file:

```r
source(here::here("scripts", "initialize_params.R"))
```

This will:
1. Execute `00_asf_risk_parameters.Rmd` to define the parameters 
2. Create a file at `output/intermediate files/asf_risk_params.rds` that contains all the parameters

## Running the Pipeline

You can run the entire pipeline or individual scripts using the `run_pipeline.R` script:

1. To run the entire pipeline:
   ```r
   source("scripts/run_pipeline.R")
   run_pipeline()  # Runs all scripts
   ```

2. To run a specific script (e.g., the third script):
   ```r
   source(here::here("scripts", "run_pipeline.R"))
   run_script(1)  # Runs script 01_01
   ```
3. To run a range of scripts:
    ```r
    run_pipeline(start_script = 2, end_script = 4)  # Runs scripts 01_02 through 01_04
    ```

## Modifying Parameters

If you need to modify parameters:

1. Edit the `00_asf_risk_parameters.Rmd` file to change the parameter values
2. Run the `initialize_params.R` script again to update the parameter file

Alternatively, can change the parameters when running specific scripts:
```r
run_script(1, params_override = list(US_grid_cell_size = 50000))
```

Or for the entire pipeline:
```r
run_pipeline(params_override = list(affected_countries = "mex_can"))
```

Alternatively, for temporary changes, you can modify parameters in memory:
```r
# Load existing parameters
params <- readRDS("output/intermediate files/asf_risk_params.rds")

# Modify a parameter
params$n_sims_for_volume <- 200

# Run with modified parameters
run_pipeline(params)
```



## Running Scripts Individually

Each script has been updated to check for the existence of the shared parameter file. If the file exists, the script will use those parameters. If not, it will fall back to its built-in default parameters.

This means you can still run individual scripts directly, without using the `run_pipeline.R` script, and they will use the shared parameters if available.

4. Multiple Parameter Sets

For different scenarios or testing, you can create multiple parameter sets:

```r
# Load existing parameters
params <- readRDS(here::here("output", "intermediate files", "asf_risk_params.rds"))

# Modify parameters for a specific scenario
params$n_sims_for_volume <- 500
params$US_grid_cell_size <- 10000

# Save with a scenario name
saveRDS(params, here::here("output", "intermediate files", "asf_risk_params_high_resolution.rds"))

# Later, to use these parameters:
high_res_params <- readRDS(here::here("output", "intermediate files", "asf_risk_params_high_resolution.rds"))
run_pipeline(params_override = high_res_params)
```

5. Adding New Parameters
To add new parameters:

Edit 00_asf_risk_parameters.Rmd to add the new parameter
Run initialize_params.R to update the shared parameter file
Add the parameter to the YAML header of any script that needs it
Script-Specific Parameters
Each script only receives the parameters it actually declares in its YAML header. This means:

Each script should only declare parameters it actually uses
You don't need to update all scripts when you add a new parameter
Each script can have its own default values for parameters
Dependencies
This parameterization system requires the following R packages:

here
rmarkdown
yaml
Make sure these packages are installed:


## Dependencies

This parameterization system requires the following R packages:
- here
- rmarkdown
- yaml

Make sure these packages are installed:
```r
install.packages(c("here", "rmarkdown", "yaml"))
```
