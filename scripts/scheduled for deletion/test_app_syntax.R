# Test script to check for syntax errors in the Shiny app
# This will help identify any issues that might prevent the app from running

# Load required libraries (comment out if not installed)
tryCatch({
  library(shiny)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(here)
  library(viridis)
  library(DT)
  library(rlang)
  cat("All required packages loaded successfully\n")
}, error = function(e) {
  cat("Error loading packages:", e$message, "\n")
})

# Test parsing the app file
tryCatch({
  source("scripts/ASF risk map shiny app.R", local = TRUE)
  cat("App file parsed successfully - no syntax errors detected\n")
}, error = function(e) {
  cat("Syntax error in app file:", e$message, "\n")
})
