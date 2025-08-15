
#-- Connect to ARM ---- 
#ARM data base version 2
db_conn <- dbConnect(odbc::odbc(),.connection_string = 
                       "Driver=SQL Server;
                        Server=AAP00VA3PPQSQL0\\MSSQLSERVER,1433;
                        Database=PPQ_AQI_ARMDMV2;
                        trusted_connection=yes")

inspection_cols <- c(
  "ID",
  "PATHWAY", 
  "FINAL_DESTINATION_STATE_ID", 
  "FINAL_DESTINATION_CITY")


#-- Pull AQIM inspection records from ARM  ---- 
system.time(wads_codes <- tbl(db_conn, sql("SELECT * FROM [PPQ_AQI_ARMDMV2].[ARMDATADM].[REF_WADS_CODE]")) |>  
              dplyr::filter(IS_ACTIVE == 1)
)

setwd(here::here('input','data','wads codes'))

write.csv(wads_codes, 'wads reference codes for processing.csv')

