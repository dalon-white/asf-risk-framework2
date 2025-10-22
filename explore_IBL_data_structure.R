# Explore IBL GeoPackage Data Structure
# This script examines the IBL destination risk layers and compares them to the US hex grid structure

library(sf)
library(here)
library(dplyr)

# ==============================================================================
# 1. Load the US hex grid reference
# ==============================================================================

cat("Loading US hex grid reference...\n")
us_grid <- readRDS(here("output", "intermediate files", "US hex grid", "us_grid.rds"))

cat("\nUS Hex Grid Structure:\n")
cat("- Number of features:", nrow(us_grid), "\n")
cat("- Geometry type:", st_geometry_type(us_grid, by_geometry = FALSE), "\n")
cat("- CRS:", st_crs(us_grid)$input, "\n")
cat("- Columns:", paste(names(us_grid), collapse = ", "), "\n")
cat("- Bounding box:\n")
print(st_bbox(us_grid))

# ==============================================================================
# 2. Load the IBL GeoPackage file
# ==============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("Loading IBL GeoPackage file...\n")
ibl_gpkg_path <- here("output", "spatial layers", "risk at final destinations", "risk_IBL_bag_swi_per_1.gpkg")

# Check what layers are in the file
ibl_layers <- st_layers(ibl_gpkg_path)
cat("\nLayers in the GeoPackage:\n")
print(ibl_layers)

# Read the IBL data
ibl_data <- st_read(ibl_gpkg_path, quiet = TRUE)

cat("\n\nIBL Data Structure:\n")
cat("- Number of features:", nrow(ibl_data), "\n")
cat("- Geometry type:", st_geometry_type(ibl_data, by_geometry = FALSE), "\n")
cat("- CRS:", st_crs(ibl_data)$input, "\n")
cat("- Columns:", paste(names(ibl_data), collapse = ", "), "\n")
cat("- Bounding box:\n")
print(st_bbox(ibl_data))

# ==============================================================================
# 3. Compare geometry types
# ==============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("GEOMETRY TYPE COMPARISON:\n")
cat("- US Grid geometry type: ", st_geometry_type(us_grid, by_geometry = FALSE), "\n")
cat("- IBL data geometry type: ", st_geometry_type(ibl_data, by_geometry = FALSE), "\n")

if (st_geometry_type(us_grid, by_geometry = FALSE) != st_geometry_type(ibl_data, by_geometry = FALSE)) {
  cat("\n⚠️  WARNING: Geometry types DO NOT match!\n")
  cat("   US Grid uses:", st_geometry_type(us_grid, by_geometry = FALSE), "\n")
  cat("   IBL data uses:", st_geometry_type(ibl_data, by_geometry = FALSE), "\n")
} else {
  cat("\n✓ Geometry types match\n")
}

# ==============================================================================
# 4. Compare CRS
# ==============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("CRS COMPARISON:\n")
cat("- US Grid CRS: ", st_crs(us_grid)$input, "\n")
cat("- IBL data CRS: ", st_crs(ibl_data)$input, "\n")

if (st_crs(us_grid) != st_crs(ibl_data)) {
  cat("\n⚠️  WARNING: CRS DO NOT match!\n")
  cat("   Consider transforming IBL data to match US Grid CRS\n")
} else {
  cat("\n✓ CRS match\n")
}

# ==============================================================================
# 5. Examine column structure
# ==============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("COLUMN STRUCTURE COMPARISON:\n\n")

cat("US Grid columns:\n")
str(st_drop_geometry(us_grid))

cat("\n\nIBL data columns:\n")
str(st_drop_geometry(ibl_data))

# Check for risk_value column
if ("risk_value" %in% names(ibl_data)) {
  cat("\n✓ IBL data has 'risk_value' column\n")
  cat("  - Summary of risk_value:\n")
  print(summary(ibl_data$risk_value))
} else {
  cat("\n⚠️  WARNING: IBL data does NOT have 'risk_value' column\n")
  cat("   Available columns:", paste(names(ibl_data), collapse = ", "), "\n")
}

# ==============================================================================
# 6. Load a comparison file (non-IBL destination risk layer)
# ==============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("LOADING COMPARISON FILE (non-IBL layer)...\n")

# Find a non-IBL destination risk file for comparison
dest_risk_files <- list.files(
  here("output", "spatial layers", "risk at final destinations"),
  pattern = "^risk_(?!IBL).*\\.gpkg$",
  full.names = TRUE
)

if (length(dest_risk_files) > 0) {
  comparison_file <- dest_risk_files[1]
  cat("Loading:", basename(comparison_file), "\n")
  
  comparison_data <- st_read(comparison_file, quiet = TRUE)
  
  cat("\nComparison Data Structure:\n")
  cat("- Number of features:", nrow(comparison_data), "\n")
  cat("- Geometry type:", st_geometry_type(comparison_data, by_geometry = FALSE), "\n")
  cat("- CRS:", st_crs(comparison_data)$input, "\n")
  cat("- Columns:", paste(names(comparison_data), collapse = ", "), "\n")
  
  cat("\n\nColumn structure of comparison file:\n")
  str(st_drop_geometry(comparison_data))
  
  # Compare geometry types
  cat("\n\nGEOMETRY COMPARISON WITH OTHER DESTINATION LAYERS:\n")
  cat("- Comparison file geometry: ", st_geometry_type(comparison_data, by_geometry = FALSE), "\n")
  cat("- IBL data geometry:        ", st_geometry_type(ibl_data, by_geometry = FALSE), "\n")
  
  if (st_geometry_type(comparison_data, by_geometry = FALSE) == st_geometry_type(ibl_data, by_geometry = FALSE)) {
    cat("\n✓ IBL data matches other destination risk layers' geometry type\n")
  } else {
    cat("\n⚠️  WARNING: IBL data geometry type DIFFERS from other destination layers!\n")
  }
  
} else {
  cat("No non-IBL destination risk files found for comparison\n")
}

# ==============================================================================
# 7. Spatial relationship check
# ==============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("SPATIAL RELATIONSHIP CHECK:\n")

# Transform IBL data to match US grid CRS for comparison
if (st_crs(us_grid) != st_crs(ibl_data)) {
  cat("Transforming IBL data to US Grid CRS for comparison...\n")
  ibl_data_transformed <- st_transform(ibl_data, st_crs(us_grid))
} else {
  ibl_data_transformed <- ibl_data
}

# Check if IBL points fall within US grid cells
cat("\nChecking if IBL geometries intersect with US Grid...\n")
intersects <- st_intersects(ibl_data_transformed, us_grid, sparse = FALSE)
num_intersecting <- sum(apply(intersects, 1, any))

cat("- Total IBL features:", nrow(ibl_data), "\n")
cat("- Features intersecting with US Grid:", num_intersecting, "\n")
cat("- Percentage:", round(100 * num_intersecting / nrow(ibl_data), 2), "%\n")

if (num_intersecting == 0) {
  cat("\n⚠️  CRITICAL: No IBL features intersect with US Grid!\n")
  cat("   This suggests the IBL data uses a completely different spatial structure\n")
} else if (num_intersecting < nrow(ibl_data)) {
  cat("\n⚠️  WARNING: Some IBL features do not intersect with US Grid\n")
} else {
  cat("\n✓ All IBL features intersect with US Grid\n")
}

# ==============================================================================
# 8. Summary and Recommendations
# ==============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("SUMMARY AND RECOMMENDATIONS:\n\n")

cat("Key Findings:\n")
cat("1. US Grid uses", st_geometry_type(us_grid, by_geometry = FALSE), "geometries\n")
cat("2. IBL data uses", st_geometry_type(ibl_data, by_geometry = FALSE), "geometries\n")
cat("3. ", num_intersecting, "out of", nrow(ibl_data), "IBL features intersect with US Grid\n")

if (st_geometry_type(us_grid, by_geometry = FALSE) != st_geometry_type(ibl_data, by_geometry = FALSE)) {
  cat("\n⚠️  ISSUE IDENTIFIED: Geometry type mismatch\n")
  cat("\nThe IBL data needs to be converted to match the US hex grid structure.\n")
  cat("Options:\n")
  cat("1. Spatial join: Assign each IBL point to the hex grid cell it falls within\n")
  cat("2. Aggregate: Sum risk values for all IBL points within each hex cell\n")
  cat("3. This will create polygon-based risk layers compatible with other destination layers\n")
}

cat("\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("Exploration complete!\n")

# ==============================================================================
# 9. Create a simple visualization
# ==============================================================================

cat("\nCreating basic visualization...\n")

# Plot IBL data
par(mfrow = c(1, 2))
plot(st_geometry(ibl_data_transformed), main = "IBL Data Geometry", 
     pch = 20, cex = 0.5, col = "red")

# Plot a sample of US grid
us_grid_sample <- us_grid[sample(1:nrow(us_grid), min(1000, nrow(us_grid))), ]
plot(st_geometry(us_grid_sample), main = "US Hex Grid (sample)", 
     border = "blue", col = NA)

cat("\nPlots created. Check the Plots pane in RStudio.\n")
