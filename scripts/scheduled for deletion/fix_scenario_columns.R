# Script to add missing 'scenario' column to distributed_risk_summary CSV files
# This fixes the rbind error in the Shiny app

library(here)
library(dplyr)

# Define files that need fixing
files_to_fix <- list(
  list(file = "distributed_risk_summary_current.csv", scenario = "current"),
  list(file = "distributed_risk_summary_IBL.csv", scenario = "IBL"),
  list(file = "distributed_risk_summary_mex_can.csv", scenario = "mex_can")
)

# Fix each file
for (file_info in files_to_fix) {
  file_path <- here("output", "aqim total distributed risk", file_info$file)
  
  # Read the file
  data <- read.csv(file_path)
  
  # Check if 'scenario' column already exists
  if (!"scenario" %in% colnames(data)) {
    # Add the scenario column
    data$scenario <- file_info$scenario
    
    # Write back to file
    write.csv(data, file_path, row.names = FALSE)
    
    cat("Added 'scenario' column to", file_info$file, "\n")
  } else {
    cat("'scenario' column already exists in", file_info$file, "\n")
  }
}

cat("\nDone! All files now have the 'scenario' column.\n")
