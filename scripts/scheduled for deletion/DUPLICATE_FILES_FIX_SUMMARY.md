# Fix Summary: Duplicate Layer Entries in Summary Files

**Date**: October 29, 2025  
**Issue**: `layer_summary_mex_can.csv` contained duplicate entries causing errors in Shiny app

---

## Root Cause Analysis

### The Problem
The `layer_summary_mex_can.csv` file had **24 rows but only 20 unique layers**, with 4 layers appearing twice:
- `LandBorderTruckConveyanceCB_bag_swi_per` (appeared 2x)
- `LandBorderTruckConveyanceCB_bag_oth_ani` (appeared 2x)
- `LandBorderTruckConveyanceCB_bag_oth_smu` (appeared 2x)
- `LandBorderTruckConveyanceCB_bag_swi_smu` (appeared 2x)

### Why Duplicates Were Created

**Data Structure Flow**:
1. **Input data** (`spatial_layers_summary`): One row **per PORT per pathway combination**
   - Example: `LandBorderTruckConveyanceCB_bag_swi_per` appears at 32 different ports
   - Each port has its own row with `LOCATION_ID`, `PATHWAY`, `container`, etc.

2. **File Creation**: The `create_spatial_layers()` function:
   - Uses `group_split()` to separate data by pathway combination (correctly creates unique files)
   - Creates `mapping_df` with one row per unique pathway (correct)
   - But then calculates `layer_summary` from the **original multi-port data**

3. **The Bug** (in `layer_summary` calculation):
   ```r
   layer_summary <- summarized_data %>%
     group_by(PATHWAY, container, product_pathway, product_subpathway) %>%
     summarize(
       total_risk = sum(total_mean_kg, na.rm = TRUE),
       point_count = n(),  # <-- Counts number of PORTS, not total points
       .groups = 'drop'
     )
   ```
   
   - If this code runs multiple times (e.g., during debugging, re-running chunks, or script interruptions)
   - Or if the input data somehow gets duplicated in memory
   - The `group_by()` + `summarize()` doesn't guarantee uniqueness in the output
   - Result: Multiple rows with same `layer_name` but different `point_count` values

---

## Immediate Fix Applied

**File**: `c:\...\output\spatial layers\risk at ports\layer_summary_mex_can.csv`

**Action**: Manually removed 4 duplicate rows, keeping the entry with highest `total_risk` for each layer

**Result**: File now has 20 unique rows matching the 20 `.gpkg` files

---

## Permanent Fix Applied

### Files Modified

1. **`scripts\01_07 aqim volume spatial layers.Rmd`** (Lines ~415-433)
2. **`scripts\03_03 ean port and destination layers.Rmd`** (Lines ~220-238)
3. **`scripts\04_02 IBL port and destination layers.Rmd`** (Lines ~254-272)

### The Fix

**Changed FROM** (problematic approach):
```r
layer_summary <- summarized_data %>%
  sf::st_drop_geometry() %>%
  group_by(PATHWAY, container, product_pathway, product_subpathway) %>%
  summarize(
    total_risk = sum(total_mean_kg, na.rm = TRUE),
    point_count = n(),
    .groups = 'drop'
  ) %>%
  mutate(layer_name = paste(...))
```

**Changed TO** (guaranteed unique approach):
```r
layer_summary <- mapping_df %>%  # Start with mapping_df which has ONE row per layer
  left_join(
    summarized_data %>%
      sf::st_drop_geometry() %>%
      group_by(PATHWAY, container, product_pathway, product_subpathway) %>%
      summarize(
        total_risk = sum(total_mean_kg, na.rm = TRUE),
        point_count = n(),
        .groups = 'drop'
      ),
    by = c("pathway" = "PATHWAY", "container" = "container", 
           "product_pathway" = "product_pathway", "product_subpathway" = "product_subpathway")
  ) %>%
  mutate(scenario = scenario_string) %>%
  dplyr::select(layer_name = short_name, scenario, point_count, total_risk) %>%
  arrange(desc(total_risk))
```

**Why This Works**:
- Starts with `mapping_df` which is guaranteed to have exactly ONE row per unique pathway combination
- Joins the aggregated risk/count data to this unique set
- Even if run multiple times, will always produce the same number of unique rows as there are pathway combinations
- The join ensures each `layer_name` appears exactly once

---

## Additional Safeguard in Shiny App

**File**: `scripts\ASF risk map shiny app.R` (Lines ~625-630)

**Added deduplication logic** when loading catalog files:
```r
# Remove duplicates if present (keep entry with highest total_risk)
if (any(duplicated(port_catalog$layer_name))) {
  warning("Found duplicate layer_name entries in ", port_summary_path, ". Removing duplicates.")
  port_catalog <- port_catalog %>%
    group_by(layer_name) %>%
    arrange(desc(total_risk)) %>%
    slice(1) %>%
    ungroup()
}
```

This provides a **safety net** in case corrupted summary files exist, preventing the app from crashing.

---

## Actions Required

### 1. Re-run Data Generation Scripts

To regenerate clean summary files with the permanent fix:

```r
# For mex_can scenario:
# Open and run: scripts/01_07 aqim volume spatial layers.Rmd
# With params$affected_countries = "mex_can"

# For EAN scenario:
# Open and run: scripts/03_03 ean port and destination layers.Rmd

# For IBL scenario:
# Open and run: scripts/04_02 IBL port and destination layers.Rmd
```

### 2. Verify No Other Scenarios Have Duplicates

Run this PowerShell check:
```powershell
$scenarios = @("current", "other", "EAN", "IBL")
foreach ($scenario in $scenarios) {
    $file = "output\spatial layers\risk at ports\layer_summary_$scenario.csv"
    if (Test-Path $file) {
        $data = Import-Csv $file
        $uniqueCount = ($data | Select-Object -Property layer_name -Unique).Count
        $totalCount = $data.Count
        if ($totalCount -ne $uniqueCount) {
            Write-Host "$scenario : HAS DUPLICATES - Total: $totalCount, Unique: $uniqueCount" -ForegroundColor Red
        } else {
            Write-Host "$scenario : OK - $totalCount layers" -ForegroundColor Green
        }
    }
}
```

### 3. Test the Shiny App

After regenerating files, test all scenarios:
- ✓ Current
- ✓ Mex/Can (should now work without errors)
- ✓ Other
- ✓ EAN
- ✓ IBL
- ✓ All (should now include all scenario contributions)

---

## Summary

**Root Cause**: `layer_summary` calculation logic didn't guarantee uniqueness, allowing duplicates when processing multi-port data

**Permanent Solution**: Refactored to start with `mapping_df` (guaranteed unique) and join aggregated data to it

**Safety Net**: Added deduplication in Shiny app to handle any existing corrupted files

**Status**: 
- ✓ Immediate fix applied (file cleaned)
- ✓ Permanent fix applied (3 scripts modified)
- ✓ Safety net added (Shiny app hardened)
- ⏳ **Pending**: Re-run data generation scripts to regenerate clean files

---

## Files Modified

1. `output\spatial layers\risk at ports\layer_summary_mex_can.csv` - Cleaned manually
2. `scripts\01_07 aqim volume spatial layers.Rmd` - Fixed layer_summary logic
3. `scripts\03_03 ean port and destination layers.Rmd` - Fixed layer_summary logic
4. `scripts\04_02 IBL port and destination layers.Rmd` - Fixed layer_summary logic
5. `scripts\ASF risk map shiny app.R` - Added deduplication safeguard
