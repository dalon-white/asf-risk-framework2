
# wasn't sure how to filter EAN dates - just testing it here to determine the best approach

```{r setup, include=FALSE}
library(tidyverse)
library(odbc)
library(DBI)
library(ggplot2)
library(here)
```

# load-params}
# Function to load the shared parameters if available
load_shared_params <- function() {
  params_file <- here::here("output", "intermediate files", "asf_risk_params.rds")
  
  if (file.exists(params_file)) {
    message("Loading shared parameters from 00_asf_risk_parameters.Rmd")
    return(readRDS(params_file))
  } else {
    message("Shared parameter file not found. Using script-specific parameters.")
    return(NULL)
  }
}

# Try to load shared params
shared_params <- load_shared_params()

# Use shared params if available, otherwise use script-specific params
if (!exists("params")) {
  if (!is.null(shared_params)) {
    params <- shared_params
  } else {
    # Default params specific to this script
    params <- list(
      affected_countries = "current", # "current" or "mex_can" or "other" 
      years = 2023:2024, #The years to include which countries have ASF
      other_affected_countries = c("Country1", "Country2"),
      affected_product_file = "asf affected product name reference table.csv",
      affected_country_file = "asf affected countries.csv",
    )
  }
}
```


# ARM AQIM data}
ean_cols <- c(
  "ID",
  "COMMODITY_ID",
   "CONVEYANCE_ID",
  "ISSUED_DATETIME",
  "ISSUED_DATETIME_FISCAL_YEAR",
  "ISSUED_DATETIME_FISCAL_MONTH",
  "CATEGORY",
  "SUBCATEGORY",
  "PATHWAY_ID", #PATHWAY ID can distinguish commerce and express carrier; probably need a reference table because the naming conventions aren't consistent
  "PATHWAY",
   "REMARKS",
   "DESTINATION_OF_ARTICLES_ID",
     "REGULATORY_ACTION_TYPE"
)

conveyance_cols <- c(
  "ID",
  "INSPECTION_ID",
  "CONVEYANCE_TYPE",
  "AIRCRAFT_TYPE",
  "VEHICLE_TYPE" #if !NULL, then it's a vehicle. Need to explore if it includes cargo trucks
)

inspection_cols <- c(
  "ID",

  "INSPECTION_NUMBER", 
  "IS_AQIM", 
  "PORT_OF_ENTRY_ID",
  
  "INSPECTION_DATETIME", 
  "INSPECTION_DATETIME_FISCAL_YEAR", 
  "INSPECTION_DATETIME_FISCAL_MONTH",
  "INSPECTION_LOCATION_ID", 
  "INSPECTION_LOCATION_NAME", 
  "INSPECTION_LOCATION_STATE_CODE", 
  "COUNTRY_OF_ORIGIN_NAME", 
 # "COUNTRY_OF_RESIDENCE_NAME", 
  
 # "FINAL_DESTINATION_STATE_NAME", 
 # "FINAL_DESTINATION_STATE_ID", 
 # "FINAL_DESTINATION_CITY", 
 # "PASSENGER_DISTANCE_DESTINATION_NAME",
  "NUMBER_OF_PASSENGERS")


commodity_cols <- c(
  "ID",
  "INSPECTION_ID",
  "IS_SMUGGLED",
  "IS_CONTAMINANT_FOUND",
  "COMMODITY_CLASSIFICATION",
  "COUNTRY_OF_ORIGIN_NAME", 
  "REF_COMMODITY_ID",
  "COMMODITY_DISPLAY_NAME",
  "MISC_ANIMAL_COMMODITY_NAME",
  "QUANTITY",
  "QUANTITY_UNITS_NAME"
)

#columns from the brg_pathway table that have more colloquial explanations to PATHWAY
pathway_cols <- c(
  "PATHWAY",
  "DISPLAY_NAME",
  "PORT_TYPE",
  "INSPECTION_TARGET",
  "MODE_OF_TRANSPORT",
  "DEFAULT_EXCLUSION_OPTIONS"
)
#-- Pull AQIM inspection records from ARM  ---- 
ean <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_FACT_EAN]")) |> 
              dplyr::select(all_of(ean_cols)) |> 
              #rename to fit AQAS data structure
              rename(EAN_ID = ID)

inspections <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_FACT_INSPECTION]")) |> 
              dplyr::filter(CATEGORY != "AQIM") |>
              dplyr::select(all_of(inspection_cols)) |>
              rename(INSPECTION_ID = ID,
                    #rename to match AQAS data structure
                     product_origin = COUNTRY_OF_ORIGIN_NAME)
 
#-- Load Bridge Commodity dataset ----
commodity <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_BRG_COMMODITY]")) |> 
              dplyr::select(all_of(commodity_cols)) |>
              rename(COMMODITY_ID = ID)

conveyance <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_BRG_CONVEYANCE]")) |>
              dplyr::select(all_of(conveyance_cols)) |>
              rename(CONVEYANCE_ID = ID)

#-- Load Bridge Pathway dataset ----
pathway_ref <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_BRG_PATHWAY]")) |> 
              dplyr::select(all_of(pathway_cols))

# Join ean records with commodity table info
## Identify columns to join by
intersect_cols <- intersect(colnames(ean), colnames(commodity))
ean_records_tmp <- ean |>
  left_join(commodity, by = intersect_cols, relationship = "many-to-many")

# Join ean and commodity with inspection details
## identify columns to join by
intersect_cols <- intersect(colnames(ean_records_tmp), colnames(inspections))
ean_records_tmp <-ean_records_tmp |>
  left_join(inspections, by = intersect_cols, relationship = "many-to-many") 

# Join ean, commodity, and inspection details with conveyance details
## identify columns to join by
intersect_cols <- intersect(colnames(ean_records_tmp), colnames(conveyance))
ean_records <- ean_records_tmp |>
  left_join(conveyance, by = intersect_cols, relationship = "many-to-many")

# Join ean, commodity, inspection, and conveyance details with pathway details
## identify columns to join by
intersect_cols <- intersect(colnames(ean_records), colnames(pathway_ref))
ean_records <- ean_records |>
  left_join(pathway_ref, by = intersect_cols, relationship = "many-to-many")

#using intersect columns above will ensure that multiple overlapping columns won't create a .x and .y version of the same column



# # Collect data
# I have compared filtering in the database vs. collecting and filtering in R. Collecting is only a couple of seconds, so it's fast enough and filtering is much easier for some operations that require complex manipulations or functions not supported by the SQL. The filtering by datetime is particularly easier when done in R due to the need for lubridate functions and having to otherwise manipulate the datetime fiscal year & month columns separately.



#open connection
db_conn <- dbConnect(odbc::odbc(),.connection_string = 
                      "Driver=SQL Server;
                        Server=AAP00VA3PPQSQL0\\MSSQLSERVER,1433;
                        Database=PPQ_AQI_ARMDMV2;
                        trusted_connection=yes")

ean_records <- ean_records |> collect()

# Disconnect from the database
dbDisconnect(db_conn)



# filter to relevant records
## filter by years

### Use the ISSUED_DATETIME or the INSPECTION_DATETIME ?

datetime_comparison <- ean_records %>%
  summarize(
    total_records = n(),
    issued_na_count = sum(is.na(ISSUED_DATETIME)),
    inspection_na_count = sum(is.na(INSPECTION_DATETIME)),
    both_na_count = sum(is.na(ISSUED_DATETIME) & is.na(INSPECTION_DATETIME)),
    only_issued_na = sum(is.na(ISSUED_DATETIME) & !is.na(INSPECTION_DATETIME)),
    only_inspection_na = sum(!is.na(ISSUED_DATETIME) & is.na(INSPECTION_DATETIME)),
    both_present = sum(!is.na(ISSUED_DATETIME) & !is.na(INSPECTION_DATETIME))
  ) %>%
  mutate(
    issued_na_percent = round(issued_na_count / total_records * 100, 2),
    inspection_na_percent = round(inspection_na_count / total_records * 100, 2),
    both_na_percent = round(both_na_count / total_records * 100, 2),
    both_present_percent = round(both_present / total_records * 100, 2)
  )

# Create a visual representation
na_comparison <- data.frame(
  datetime_field = c("ISSUED_DATETIME", "INSPECTION_DATETIME", "Both NA", "Both Present"),
  na_count = c(datetime_comparison$issued_na_count, 
               datetime_comparison$inspection_na_count,
               datetime_comparison$both_na_count,
               datetime_comparison$both_present),
  percentage = c(datetime_comparison$issued_na_percent,
                 datetime_comparison$inspection_na_percent,
                 datetime_comparison$both_na_percent,
                 datetime_comparison$both_present_percent)
)

# Print the detailed summary
print(na_comparison)


# Calculate time difference between ISSUED_DATETIME and INSPECTION_DATETIME
datetime_diff <- head(ean_records, 10000) %>%
  # Filter to records where both datetime fields are present
  filter(!is.na(ISSUED_DATETIME) & !is.na(INSPECTION_DATETIME)) %>%
  # Calculate the difference in hours
  mutate(
    time_diff_hours = as.numeric(difftime(ISSUED_DATETIME, INSPECTION_DATETIME, units = "hours")),
    time_diff_days = as.numeric(difftime(ISSUED_DATETIME, INSPECTION_DATETIME, units = "days")),
    # Create categories for analysis
    time_diff_category = case_when(
      time_diff_hours < 0 ~ "Issued before inspection (error?)",
      time_diff_hours == 0 ~ "Same time",
      time_diff_hours > 0 & time_diff_hours <= 1 ~ "Within 1 hour",
      time_diff_hours > 1 & time_diff_hours <= 24 ~ "1-24 hours",
      time_diff_hours > 24 & time_diff_hours <= 168 ~ "1-7 days",
      time_diff_hours > 168 ~ "Over 7 days"
    )
  )

  # Distribution by category
time_diff_distribution <- datetime_diff %>%
  group_by(time_diff_category) %>%
  summarize(
    count = n(),
    percentage = n() / nrow(datetime_diff) * 100
  ) %>%
  arrange(factor(time_diff_category, levels = c(
    "Issued before inspection (error?)", 
    "Same time", 
    "Within 1 hour", 
    "1-24 hours", 
    "1-7 days", 
    "Over 7 days"
  )))


# Summary statistics
time_diff_summary <- datetime_diff %>%
  summarize(
    n_records = n(),
    min_diff_hours = min(time_diff_hours, na.rm = TRUE),
    max_diff_hours = max(time_diff_hours, na.rm = TRUE),
    median_diff_hours = median(time_diff_hours, na.rm = TRUE),
    mean_diff_hours = mean(time_diff_hours, na.rm = TRUE),
    sd_diff_hours = sd(time_diff_hours, na.rm = TRUE),
    q25_diff_hours = quantile(time_diff_hours, 0.25, na.rm = TRUE),
    q75_diff_hours = quantile(time_diff_hours, 0.75, na.rm = TRUE)
  )


print("Time difference summary statistics (in hours):")
print(time_diff_summary)

print("Distribution by time difference category:")
print(time_diff_distribution)


# In most cases, it looks like inspection datetime and issued datetime are pretty much the same, with 5% delayed issuance dates >7 days
# So, results suggest filter by INSPECTION_DATETIME first then by ISSUED_DATETIME if needed


