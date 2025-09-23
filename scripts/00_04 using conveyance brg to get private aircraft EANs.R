# I tried using BRG_CONVEYANCE to get more information about trucks and aircraft but CONVEYANCE_TYPE was coming out as NULL
# I stepped through why below

# Conclusion -----
# Dates ranged 2015 - 2024, but there were onl 2511 with a CONVEYANCE_ID that matched to a record in BRG_CONVEYANCE
## There were only 6 of these after 2022; They were all 2024, 3 Vessel and 3 non-port-EPP
# So, not a useful brg table for these purposes


library(tidyverse)
library(odbc)
library(DBI)


#open connection
db_conn <- dbConnect(odbc::odbc(),.connection_string = 
                      "Driver=SQL Server;
                        Server=AAP00VA3PPQSQL0\\MSSQLSERVER,1433;
                        Database=PPQ_AQI_ARMDMV2;
                        trusted_connection=yes")

```

# Identify columns of interest
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




# Identify all EANs
ean <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_FACT_EAN]")) |> 
              dplyr::select(all_of(ean_cols)) |> 
              #rename to fit AQAS data structure
              rename(EAN_ID = ID)


# Identify all CONVEYANCE_IDs
conveyance <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[SYS2_BRG_CONVEYANCE]")) |>
              dplyr::select(all_of(conveyance_cols)) |>
              rename(CONVEYANCE_ID = ID)




# Pull all CONVEYANCE_IDs that are not NA
conv_id_test <- ean |> filter(!is.na(CONVEYANCE_ID)) |> pull(CONVEYANCE_ID)
conv_w_ean_test <- conveyance |> filter(CONVEYANCE_ID %in% conv_id_test)
ean_w_conv_test <- ean |> inner_join(conv_w_ean_test, by = "CONVEYANCE_ID") |> collect()
View(ean_w_conv_test)
dbDisconnect(db_conn)