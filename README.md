# asf-risk-framework2

## 00 files
Files beginning in 00_ were for testing and comparison; they were for exploratory analyses. I typically took them out of the file I was working on and saved them elsewhere so that they would not keep running in the pipeline. I tried to save them in a standalone fashion.
- Not pipeline files
## 01 files
Files beginning in 01_ refer the AQIM data quantification
- Pipeline files
## 02 files
Regulated garbage visualization
- Not pipeline files
## 03 files
EAN data quantification
- Pipeline files
## 04 files
PR data layers
- Pipeline files

# Order of Operations

## set parameters and prepare the pipeline to be run

### set_asf_risk_parameters.Rmd
 Run set_asf_risk_parameters.Rmd
 - Set all the parameters for the pipeline within this.
   - All parameters should be set here, which saves them to a file. Each script then calls the file. Each parameter within this script also needs to have a yaml header in the script that is being run. It doesn't have to have the correct information in the given pipeline script's yaml header, but it needs to initialize from it
- Any new pipeline files must be added by name to the `pipeline_scripts` vector within to be run

### initialize parameters
Run initialize_params.R
- This script serves as a wrapper to generate the shared parameter file from the set_asf_risk_parameters.Rmd file. This ensures a single source of truth for parameters.

### create pipeline functions
Run or source run_pipeline.R ('Run', or 'source("scripts/run_pipeline.R")' )
- This holds the functions for running single scripts or the pipeline

## Visualize results
`ASF risk map shiny app.R` holds the shiny app server logic and ui

### Run the batch file
Navigate to the base directory and run the batch file `ASF_visualization_app.bat`