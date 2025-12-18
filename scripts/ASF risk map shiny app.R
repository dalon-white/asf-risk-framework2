# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(here)
library(viridis)
library(DT)
library(rlang)  # For sym() function

# Ensure required output directories exist
check_create_dir <- function(path) {
  if (!dir.exists(path)) {
    message("Creating missing directory: ", path)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# Create any missing directories the app depends on
check_create_dir(here("output", "aqim total distributed risk"))
check_create_dir(here("output", "spatial layers", "risk at ports"))
check_create_dir(here("output", "spatial layers", "risk at final destinations"))

# Suppress R CMD check NOTEs about non-standard evaluation
globalVariables(c(
  "pathway", "container", "geom", "risk_value", "short_name", "layer_name", 
  "total_risk", "risk_percentage", "components", "LOCATION_NAME", 
  "LOCATION_STATE_CODE", "Risk", "Layer", "Location", "State", "lon", "lat",
  "lon_rounded", "lat_rounded", "percentage", "scenario", "combined_risk",
  "product_pathway", "product_subpathway", "layer_count", "location_key",
  "entry_type", "port", "conveyance_pathway", "conveyance_subpathway",
  "PATHWAY", "display_name", "marker_id"
))

# UI definition with tabbed interface
ui <- navbarPage(
  title = "ASF Risk Explorer",
    # Tab 1: Destination Risk
  tabPanel(
    "Destination Risk", 
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # Scenario selection dropdown
          selectInput("dest_scenario_filter", "Select Scenario:", 
                      choices = c("All" = "all",
                                  "Current" = "current", 
                                  "Mexico/Canada" = "mex_can", 
                                  "Illegal Boat Landings" = "IBL"),
                      selected = "all"),          hr(),
          # Dynamic checkboxes for layer selection
          uiOutput("layer_selector"),
          hr(),
          # Category filters
          selectInput("pathway_filter", "Filter by Pathway:", choices = NULL, multiple = TRUE),
          selectInput("container_filter", "Filter by Container:", choices = NULL, multiple = TRUE),
          selectInput("product_pathway_filter", "Filter by Product Pathway:", choices = NULL, multiple = TRUE),
          selectInput("product_subpathway_filter", "Filter by Product Use:", choices = NULL, multiple = TRUE),
          hr(),
          # Conveyance and entry type filters
          selectInput("entry_type_filter", "Filter by Entry Type:", choices = NULL, multiple = TRUE),
          selectInput("port_filter", "Filter by Port Type:", choices = NULL, multiple = TRUE),
          selectInput("conveyance_pathway_filter", "Filter by Conveyance:", choices = NULL, multiple = TRUE),
          selectInput("conveyance_subpathway_filter", "Filter by Vehicle Type:", choices = NULL, multiple = TRUE),
          hr(),
          checkboxInput("auto_update", "Auto-update map", value = FALSE),
          actionButton("update_map", "Update Map")
        ),
        mainPanel(
          leafletOutput("risk_map", height = "600px"),
          hr(),
          h4("Active Layers Summary"),
          tableOutput("active_layers_summary")
        )
      )
    )
  ),
    # Tab 2: Port Risk
  tabPanel(
    "Port Risk",
    fluidPage(
      sidebarLayout(
        sidebarPanel(          # Scenario selection dropdown
          selectInput("scenario_filter", "Select Scenario:", 
                      choices = c("All" = "all",
                                  "Current" = "current", 
                                  "Mexico/Canada" = "mex_can", 
                                  "Other" = "other",
                                  "EANs" = "EAN",
                                  "Illegal Boat Landings" = "IBL"),
                      selected = "current"),
          hr(),
          # Dynamic checkboxes for port layer selection
          uiOutput("port_layer_selector"),
          hr(),
          # Category filters for port data
          selectInput("port_pathway_filter", "Filter by Pathway:", choices = NULL, multiple = TRUE),
          selectInput("port_container_filter", "Filter by Container:", choices = NULL, multiple = TRUE),
          hr(),
          checkboxInput("port_auto_update", "Auto-update map", value = FALSE),
          actionButton("port_update_map", "Update Map"),          hr(),          # Point size control
          sliderInput("point_size", "Base Marker Size:", min = 5, max = 30, value = 15),
          helpText("Marker sizes and transparency scale with risk values."),
          helpText("Higher risk = larger, more opaque markers."),
          # Color scheme selection
          selectInput("color_scheme", "Color Scheme:", 
                      choices = c("viridis", "magma", "plasma", "inferno", "cividis"),
                      selected = "viridis"),
          hr(),
          # Risk threshold slider
          checkboxInput("enable_risk_threshold", "Enable Risk Threshold Filter", value = FALSE),
          sliderInput("risk_threshold", "Max Risk Value to Display:",
                     min = 0, max = 100, value = 100, 
                     step = 1, post = "%",
                     animate = animationOptions(interval = 300))        ),        mainPanel(
          leafletOutput("port_risk_map", height = "600px"),
          hr(),
          h4("Port Risk Summary"),
          fluidRow(
            column(6, 
                  wellPanel(
                    h5("Scenario Information"),
                    textOutput("scenario_info"),
                    hr(),
                    h5("Active Layers"),
                    textOutput("active_port_layers_count")
                  )
            ),
            column(6, DTOutput("port_data_table"))
          ),
          hr(),
          # Location Details Panel (shown when a location is clicked)
          conditionalPanel(
            condition = "output.location_clicked",
            wellPanel(
              h4("Location Details", style = "color: #337ab7;"),
              fluidRow(
                column(6,
                  h5("Location Information"),
                  verbatimTextOutput("clicked_location_info")
                ),
                column(6,
                  h5("Component Breakdown"),
                  DTOutput("clicked_location_components")
                )
              ),
              actionButton("clear_selection", "Clear Selection", 
                          icon = icon("times"), class = "btn-warning btn-sm")
            )
          )
        )
      )
    )
  ),
  
  # Tab 3: About
  tabPanel(
    "About",
    fluidPage(
      h2("About the ASF Risk Explorer"),
      p("This application visualizes African Swine Fever (ASF) risk data across the United States."),
      p("The data is derived from the ASF Risk Framework, which models risk pathways for the introduction of ASF."),
      hr(),
      h3("Data Sources"),
      p("This application uses two main data sources:"),
      tags$ul(
        tags$li(strong("Destination Risk Data:"), "Kernel density estimation of risk at final destinations"),
        tags$li(strong("Port Risk Data:"), "Point-based risk assessments at ports of entry")
      ),
      hr(),
      h3("Usage Instructions"),
      tags$ul(
        tags$li("Use the 'Destination Risk' tab to explore risk distribution across the US"),
        tags$li("Use the 'Port Risk' tab to examine risk at specific ports of entry"),
        tags$li("Filter data by pathway or container type using the sidebar controls"),
        tags$li("Select layers to display using the checkboxes"),
        tags$li("Click 'Update Map' to refresh the display after making selections")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  #===============================
  # DESTINATION RISK TAB FUNCTIONS
  #===============================
  
  # Load pathway reference table for enriching layer metadata
  pathway_reference <- reactive({
    ref_path <- here::here("input", "data", "PATHWAY name reference", "pathway to entry type reference table.csv")
    if (file.exists(ref_path)) {
      read.csv(ref_path, stringsAsFactors = FALSE)
    } else {
      warning("Pathway reference table not found")
      data.frame(
        PATHWAY = character(),
        entry_type = character(),
        port = character(),
        conveyance_pathway = character(),
        conveyance_subpathway = character(),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Load all available layers
  available_layers <- reactive({
    # Find all distributed risk summary files
    all_summary_files <- list.files(here("output", "aqim total distributed risk"), 
                                    pattern = "distributed_risk_summary.*\\.csv$", 
                                    full.names = TRUE)
    
    # If no files found, return empty data frame
    if (length(all_summary_files) == 0) {
      warning("No distributed_risk_summary files found")
      return(data.frame(short_name = character(), 
                        layer_name = character(), 
                        total_risk = numeric(),
                        pathway = character(),
                        container = character()))
    }
    
    # Read and combine all summary files
    all_catalogs <- lapply(all_summary_files, function(file_path) {
      tryCatch({
        read.csv(file_path)
      }, error = function(e) {
        warning("Error reading ", file_path, ": ", e$message)
        NULL
      })
    })
      # Remove any NULL entries (failed reads)
    all_catalogs <- all_catalogs[!sapply(all_catalogs, is.null)]
    
    # If all reads failed, return empty data frame
    if (length(all_catalogs) == 0) {
      warning("Failed to read any distributed_risk_summary files")
      return(data.frame(short_name = character(), 
                        layer_name = character(), 
                        total_risk = numeric(),
                        pathway = character(),
                        container = character()))
    }
    
    # Combine all catalogs into one using bind_rows to handle different column structures
    layer_catalog <- bind_rows(all_catalogs)
    
    # Enrich with pathway reference data
    pathway_ref <- pathway_reference()
    if (nrow(pathway_ref) > 0) {
      # Clean column names in pathway_ref to avoid spaces
      names(pathway_ref) <- gsub(" ", "_", names(pathway_ref))
      
      layer_catalog <- layer_catalog %>%
        left_join(pathway_ref, by = c("pathway" = "PATHWAY"), relationship = "many-to-one")
    }
    
    # Return the combined catalog
    return(layer_catalog)
  })
    # Helper function to create readable layer names for destination risk
  create_dest_readable_name <- function(pathway, container, product_pathway, product_subpathway, 
                                        entry_type = NA, port_type = NA, conveyance_pathway = NA, conveyance_subpathway = NA) {
    parts <- c()
    
    # Helper function to check if a value is valid (not NA, not empty, not "NA" string)
    is_valid <- function(x) {
      if (length(x) == 0) return(FALSE)
      if (is.na(x[1])) return(FALSE)
      if (x[1] == "") return(FALSE)
      if (x[1] == "NA") return(FALSE)
      return(TRUE)
    }
    
    # Add port type if available
    if (is_valid(port_type)) {
      parts <- c(parts, paste0("Port: ", port_type))
    }
    
    # Add conveyance pathway if available
    if (is_valid(conveyance_pathway)) {
      parts <- c(parts, paste0("Conveyance: ", conveyance_pathway))
    }
    
    # Add conveyance subpathway if available
    if (is_valid(conveyance_subpathway)) {
      parts <- c(parts, paste0("Vehicle: ", conveyance_subpathway))
    }
    
    # Add entry type if available
    if (is_valid(entry_type)) {
      parts <- c(parts, paste0("Type: ", entry_type))
    }
    
    # Add container if available
    if (is_valid(container)) {
      parts <- c(parts, paste0("Container: ", container))
    }
    
    # Add product pathway if available
    if (is_valid(product_pathway)) {
      parts <- c(parts, paste0("Product: ", product_pathway))
    }
    
    # Add product subpathway if available
    if (is_valid(product_subpathway)) {
      parts <- c(parts, paste0("Use: ", product_subpathway))
    }
    
    # If no parts, return pathway
    if (length(parts) == 0) {
      return(ifelse(is_valid(pathway), pathway, "Unknown"))
    }
    
    # Join all parts with " | "
    return(paste(parts, collapse = " | "))
  }
  # Create dynamic layer checkboxes based on available layers
  output$layer_selector <- renderUI({
    catalog <- available_layers()
    
    # Filter by scenario if selected (not "all")
    if (!is.null(input$dest_scenario_filter) && input$dest_scenario_filter != "all") {
      # Look for scenario in pathway or in summary file name embedded in the layer name
      if (input$dest_scenario_filter == "IBL") {
        # For IBL, look for "IBL" at the start of layer name
        catalog <- catalog %>% filter(grepl("^IBL_", layer_name) | pathway == "IBL")
      } else {
        # For other scenarios, check if the layer source contains the scenario name
        catalog <- catalog %>% filter(grepl(input$dest_scenario_filter, layer_name, ignore.case = TRUE) | 
                                     pathway == input$dest_scenario_filter)
      }
    }
    
    # Apply pathway filter if selected
    if (!is.null(input$pathway_filter) && length(input$pathway_filter) > 0 && 
        !("All" %in% input$pathway_filter)) {
      catalog <- catalog %>% filter(pathway %in% input$pathway_filter)
    }
    
    # Apply container filter if selected
    if (!is.null(input$container_filter) && length(input$container_filter) > 0 && 
        !("All" %in% input$container_filter)) {
      catalog <- catalog %>% filter(container %in% input$container_filter)
    }
    
    # Apply product pathway filter if selected
    if (!is.null(input$product_pathway_filter) && length(input$product_pathway_filter) > 0 && 
        !("All" %in% input$product_pathway_filter)) {
      catalog <- catalog %>% filter(product_pathway %in% input$product_pathway_filter)
    }
    
    # Apply product subpathway filter if selected
    if (!is.null(input$product_subpathway_filter) && length(input$product_subpathway_filter) > 0 && 
        !("All" %in% input$product_subpathway_filter)) {
      catalog <- catalog %>% filter(product_subpathway %in% input$product_subpathway_filter)
    }
    
    # Apply entry type filter if selected
    if (!is.null(input$entry_type_filter) && length(input$entry_type_filter) > 0 && 
        !("All" %in% input$entry_type_filter)) {
      catalog <- catalog %>% filter(entry_type %in% input$entry_type_filter)
    }
    
    # Apply port filter if selected
    if (!is.null(input$port_filter) && length(input$port_filter) > 0 && 
        !("All" %in% input$port_filter)) {
      catalog <- catalog %>% filter(port %in% input$port_filter)
    }
    
    # Apply conveyance pathway filter if selected
    if (!is.null(input$conveyance_pathway_filter) && length(input$conveyance_pathway_filter) > 0 && 
        !("All" %in% input$conveyance_pathway_filter)) {
      catalog <- catalog %>% filter(conveyance_pathway %in% input$conveyance_pathway_filter)
    }
    
    # Apply conveyance subpathway filter if selected
    if (!is.null(input$conveyance_subpathway_filter) && length(input$conveyance_subpathway_filter) > 0 && 
        !("All" %in% input$conveyance_subpathway_filter)) {
      catalog <- catalog %>% filter(conveyance_subpathway %in% input$conveyance_subpathway_filter)
    }
    
    # Create readable display names
    catalog <- catalog %>%
      rowwise() %>%
      mutate(
        display_name = create_dest_readable_name(
          pathway, container, product_pathway, product_subpathway,
          entry_type, port, conveyance_pathway, conveyance_subpathway
        )
      ) %>%
      ungroup()
    
    # Create checkboxes with readable names
    checkboxGroupInput(
      "selected_layers",
      "Select Layers:",
      choices = setNames(catalog$short_name, catalog$display_name),
      selected = NULL
    )
  })
    # Update filter choices when data loads
  observe({
    catalog <- available_layers()
    
    # Get unique values for each filter, removing NAs and empty strings
    pathways <- unique(catalog$pathway)
    pathways <- pathways[!is.na(pathways) & pathways != ""]
    
    containers <- unique(catalog$container)
    containers <- containers[!is.na(containers) & containers != ""]
    
    product_pathways <- unique(catalog$product_pathway)
    product_pathways <- product_pathways[!is.na(product_pathways) & product_pathways != ""]
    
    product_subpathways <- unique(catalog$product_subpathway)
    product_subpathways <- product_subpathways[!is.na(product_subpathways) & product_subpathways != ""]
    
    entry_types <- unique(catalog$entry_type)
    entry_types <- entry_types[!is.na(entry_types) & entry_types != "" & entry_types != "NA"]
    
    ports <- unique(catalog$port)
    ports <- ports[!is.na(ports) & ports != "" & ports != "NA"]
    
    conv_pathways <- unique(catalog$conveyance_pathway)
    conv_pathways <- conv_pathways[!is.na(conv_pathways) & conv_pathways != "" & conv_pathways != "NA"]
    
    conv_subpathways <- unique(catalog$conveyance_subpathway)
    conv_subpathways <- conv_subpathways[!is.na(conv_subpathways) & conv_subpathways != "" & conv_subpathways != "NA"]
    
    # Update all filter inputs
    updateSelectInput(session, "pathway_filter", choices = c("All", sort(pathways)))
    updateSelectInput(session, "container_filter", choices = c("All", sort(containers)))
    updateSelectInput(session, "product_pathway_filter", choices = c("All", sort(product_pathways)))
    updateSelectInput(session, "product_subpathway_filter", choices = c("All", sort(product_subpathways)))
    updateSelectInput(session, "entry_type_filter", choices = c("All", sort(entry_types)))
    updateSelectInput(session, "port_filter", choices = c("All", sort(ports)))
    updateSelectInput(session, "conveyance_pathway_filter", choices = c("All", sort(conv_pathways)))
    updateSelectInput(session, "conveyance_subpathway_filter", choices = c("All", sort(conv_subpathways)))
  })
  
  # Load the selected layers and combine them
  combined_layer <- reactive({
    # Only update when button is pressed (unless auto-update is enabled)
    if (!input$auto_update) {
      input$update_map
    }
    
    req(input$selected_layers)
    isolate({
      # Get the selected layer filenames
      selected <- input$selected_layers
      
      if (length(selected) == 0) {
        return(NULL)
      }
        # Load and combine the selected layers
      all_layers <- lapply(selected, function(layer_name) {
        # Try first with direct naming pattern
        file_path <- here("output", "spatial layers", "risk at final destinations", 
                          paste0("risk_", layer_name, ".gpkg"))
        
        # Check if file exists
        if (!file.exists(file_path)) {
          warning("File not found: ", file_path)
          return(NULL)
        }
        
        # Read the layer and transform to WGS84
        tryCatch({
          layer <- st_read(file_path, quiet = TRUE)
          
          # Debug info - uncomment if needed
          message(paste("Loaded layer:", layer_name, "with", nrow(layer), "features"))
          # print(paste("Original CRS:", st_crs(layer)$input))
          
          # Transform to WGS84 for Leaflet compatibility
          layer <- st_transform(layer, 4326)
          
          return(layer)
        }, error = function(e) {
          warning("Error reading ", file_path, ": ", e$message)
          return(NULL)
        })
      })
      
      # Remove any NULL layers (files that couldn't be loaded)
      all_layers <- all_layers[!sapply(all_layers, is.null)]
      
      if (length(all_layers) == 0) {
        return(NULL)
      }
        # Combine layers by summing risk values at each location
      combined <- tryCatch({
        if (length(all_layers) == 0) {
          return(NULL)
        }
        
        # Check if risk_value column exists in all layers
        has_risk_value <- sapply(all_layers, function(layer) {
          "risk_value" %in% colnames(layer)
        })
        
        # If not all layers have risk_value, add it with NA values
        for (i in which(!has_risk_value)) {
          message("Adding missing risk_value column to layer ", i)
          all_layers[[i]]$risk_value <- NA
        }
        
        combined_data <- bind_rows(all_layers) 
        
        # Check if we have a valid combined dataset
        if (is.null(combined_data) || nrow(combined_data) == 0) {
          warning("No valid data after combining layers")
          return(NULL)
        }
        
        # Group and summarize
        combined_data %>%
          group_by(geom) %>%
          summarize(combined_risk = sum(risk_value, na.rm = TRUE))
      }, error = function(e) {
        warning("Error combining layers: ", e$message)
        return(NULL)
      })
      
      # Ensure final data is in WGS84
      if (!is.null(combined)) {
        combined <- st_transform(combined, 4326)
      }
      
      # Debug info - uncomment if needed
      # bbox <- st_bbox(combined)
      # print(paste("Bounds:", 
      #            "Lon:", bbox["xmin"], "-", bbox["xmax"],
      #            "Lat:", bbox["ymin"], "-", bbox["ymax"]))
      
      return(combined)
    })
  })
  
  # Render the destination risk map
  output$risk_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -95, lat = 39, zoom = 4)
  })
  
  # Update destination risk map when combined layer changes
  observe({
    layer <- combined_layer()
    
    if (is.null(layer)) {
      leafletProxy("risk_map") %>%
        clearShapes()
      return()
    }
    
    # Create color palette based on risk values
    pal <- colorNumeric(palette = "viridis", domain = layer$combined_risk)
    
    leafletProxy("risk_map") %>%
      clearShapes() %>%
      addPolygons(
        data = layer,
        fillColor = ~pal(combined_risk),
        weight = 1,
        opacity = 0.7,
        color = "white",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        popup = ~paste0("Combined Risk: ", round(combined_risk, 4))
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = layer$combined_risk,
        title = "Risk Level",
        opacity = 0.7
      )
  })
  
  # Summary table of active layers
  output$active_layers_summary <- renderTable({
    req(input$selected_layers)
    
    catalog <- available_layers()
    active_layers <- catalog %>%
      filter(short_name %in% input$selected_layers) %>%
      select(layer_name = layer_name, total_risk)
    
    active_layers %>%
      arrange(desc(total_risk)) %>%
      mutate(
        risk_percentage = 100 * total_risk / sum(total_risk),
        total_risk = round(total_risk, 4),
        risk_percentage = round(risk_percentage, 2)
      ) %>%
      rename(
        "Layer" = layer_name,
        "Risk Value" = total_risk,
        "% of Total" = risk_percentage
      )
  })
  
  #=========================
  # PORT RISK TAB FUNCTIONS
  #=========================
    # Load available port risk layers
  available_port_layers <- reactive({
    # Get the selected scenario
    scenario <- input$scenario_filter
    
    # Try to read the mapping file first (preferred source for full names)
    mapping_path <- here("output", "spatial layers", "risk at ports", 
                        paste0("filename_mapping_", scenario, ".csv"))
    
    # First try to read the summary file
    port_summary_path <- here("output", "spatial layers", "risk at ports", 
                             paste0("layer_summary_", scenario, ".csv"))
    
    if (file.exists(port_summary_path)) {
      # Read from summary file if it exists
      port_catalog <- read.csv(port_summary_path, stringsAsFactors = FALSE)
      
      # Try to enrich with mapping file data
      if (file.exists(mapping_path)) {
        mapping_data <- read.csv(mapping_path, stringsAsFactors = FALSE)
        
        # Join with mapping data to get full component names
        port_catalog <- port_catalog %>%
          left_join(mapping_data, by = c("layer_name" = "short_name"), 
                   relationship = "many-to-one")
        
        # If join was successful, we'll have pathway, container, etc. from mapping
        # Otherwise, extract from layer_name as fallback
        if (!"pathway" %in% names(port_catalog)) {
          port_catalog <- port_catalog %>%
            mutate(
              components = strsplit(as.character(layer_name), "_"),
              pathway = sapply(components, function(x) if(length(x) > 0) x[1] else NA),
              container = sapply(components, function(x) if(length(x) > 1) x[2] else NA),
              product_pathway = sapply(components, function(x) if(length(x) > 2) x[3] else NA),
              product_subpathway = sapply(components, function(x) if(length(x) > 3) x[4] else NA)
            ) %>%
            select(-components)
        }
      } else {
        # No mapping file, extract components from layer names
        port_catalog <- port_catalog %>%
          mutate(
            components = strsplit(as.character(layer_name), "_"),
            pathway = sapply(components, function(x) if(length(x) > 0) x[1] else NA),
            container = sapply(components, function(x) if(length(x) > 1) x[2] else NA),
            product_pathway = sapply(components, function(x) if(length(x) > 2) x[3] else NA),
            product_subpathway = sapply(components, function(x) if(length(x) > 3) x[4] else NA)
          ) %>%
          select(-components)
      }
      
      return(port_catalog)    } else {
      # If summary doesn't exist, scan the directory
      port_files_dir <- here("output", "spatial layers", "risk at ports")
      all_port_files <- list.files(path = port_files_dir, pattern = ".*\\.gpkg$", full.names = FALSE)
      
      # Handle "all" scenario differently - need to extract scenario from each filename
      if (scenario == "all") {
        # Get all files and extract their scenarios
        # Pattern: layername_scenario.gpkg
        scenario_options <- c("current", "mex_can", "other", "EAN", "IBL")
        
        # Try to load mapping files for all scenarios
        all_mapping_data <- list()
        for (scen in scenario_options) {
          mapping_path <- here("output", "spatial layers", "risk at ports", 
                             paste0("filename_mapping_", scen, ".csv"))
          if (file.exists(mapping_path)) {
            mapping_df <- read.csv(mapping_path, stringsAsFactors = FALSE)
            mapping_df$scenario <- scen
            all_mapping_data[[scen]] <- mapping_df
          }
        }
        
        # If we have mapping data, use it
        if (length(all_mapping_data) > 0) {
          port_catalog <- bind_rows(all_mapping_data)
          # Rename short_name to layer_name for consistency
          if ("short_name" %in% names(port_catalog)) {
            port_catalog <- port_catalog %>%
              rename(layer_name = short_name)
          }
          # Add point_count and total_risk as NA if not present
          if (!"point_count" %in% names(port_catalog)) port_catalog$point_count <- NA
          if (!"total_risk" %in% names(port_catalog)) port_catalog$total_risk <- NA
          
          return(port_catalog)
        }
        
        # Fallback: Create catalog by extracting scenario from each file
        port_catalog_list <- lapply(all_port_files, function(file) {
          # Try to match each scenario pattern
          matched_scenario <- NA
          layer_name <- NA
          
          for (scen in scenario_options) {
            pattern <- paste0("_", scen, "\\.gpkg$")
            if (grepl(pattern, file)) {
              matched_scenario <- scen
              layer_name <- gsub(pattern, "", file)
              break
            }
          }
          
          # If no scenario matched, try without scenario suffix
          if (is.na(matched_scenario)) {
            layer_name <- gsub("\\.gpkg$", "", file)
            matched_scenario <- "unknown"
          }
          
          data.frame(
            filename = file,
            layer_name = layer_name,
            scenario = matched_scenario,
            point_count = NA,
            total_risk = NA,
            stringsAsFactors = FALSE
          )
        })
        
        port_catalog <- do.call(rbind, port_catalog_list)
          } else {
        # For specific scenarios, try to use mapping file
        mapping_path <- here("output", "spatial layers", "risk at ports", 
                           paste0("filename_mapping_", scenario, ".csv"))
        
        if (file.exists(mapping_path)) {
          # Use mapping file
          port_catalog <- read.csv(mapping_path, stringsAsFactors = FALSE)
          port_catalog$scenario <- scenario
          
          # Rename short_name to layer_name for consistency
          if ("short_name" %in% names(port_catalog)) {
            port_catalog <- port_catalog %>%
              rename(layer_name = short_name)
          }
          
          # Add point_count and total_risk as NA if not present
          if (!"point_count" %in% names(port_catalog)) port_catalog$point_count <- NA
          if (!"total_risk" %in% names(port_catalog)) port_catalog$total_risk <- NA
          
          return(port_catalog)
        }
          # Fallback: scan files if no mapping exists
        scenario_pattern <- paste0("_", scenario, "\\.gpkg$")
        port_files <- all_port_files[grepl(scenario_pattern, all_port_files)]
        
        # If no files match the scenario, return empty catalog
        if (length(port_files) == 0) {
          message("No files found for scenario: ", scenario)
          return(data.frame(
            filename = character(0),
            layer_name = character(0),
            scenario = character(0),
            point_count = numeric(0),
            total_risk = numeric(0),
            pathway = character(0),
            container = character(0),
            product_pathway = character(0),
            product_subpathway = character(0),
            stringsAsFactors = FALSE
          ))
        }
        
        # Create a catalog dataframe
        port_catalog <- data.frame(
          filename = port_files,
          layer_name = gsub(paste0("_", scenario, "\\.gpkg$"), "", port_files),
          scenario = scenario,
          point_count = NA,
          total_risk = NA,
          stringsAsFactors = FALSE
        )
      }
      
      # Extract components from layer names (fallback if no mapping data)
      if (!"pathway" %in% names(port_catalog)) {
        port_catalog <- port_catalog %>%
          mutate(
            components = strsplit(as.character(layer_name), "_"),
            pathway = sapply(components, function(x) if(length(x) > 0) x[1] else NA),
            container = sapply(components, function(x) if(length(x) > 1) x[2] else NA),
            product_pathway = sapply(components, function(x) if(length(x) > 2) x[3] else NA),
            product_subpathway = sapply(components, function(x) if(length(x) > 3) x[4] else NA)
          ) %>%
          select(-components)
      }
      
      return(port_catalog)
    }
  })
  
  # Helper function to create readable layer names for port risk
  create_port_readable_name <- function(pathway, container, product_pathway, product_subpathway) {
    parts <- c()
    
    # Add pathway if available
    if (!is.na(pathway) && pathway != "" && pathway != "NA") {
      parts <- c(parts, pathway)
    }
    
    # Add container if available
    if (!is.na(container) && container != "" && container != "NA") {
      parts <- c(parts, container)
    }
    
    # Add product pathway if available
    if (!is.na(product_pathway) && product_pathway != "" && product_pathway != "NA") {
      parts <- c(parts, product_pathway)
    }
    
    # Add product subpathway if available
    if (!is.na(product_subpathway) && product_subpathway != "" && product_subpathway != "NA") {
      parts <- c(parts, product_subpathway)
    }
    
    # If no parts, return "Unknown"
    if (length(parts) == 0) {
      return("Unknown")
    }
    
    # Join all parts with " | "
    return(paste(parts, collapse = " | "))
  }
  
  # Create dynamic port layer checkboxes
  output$port_layer_selector <- renderUI({
    port_catalog <- available_port_layers()
    scenario <- input$scenario_filter
    
    # Apply filters if selected
    if (!is.null(input$port_pathway_filter) && length(input$port_pathway_filter) > 0 && 
        !("All" %in% input$port_pathway_filter)) {
      port_catalog <- port_catalog %>% filter(pathway %in% input$port_pathway_filter)
    }
    if (!is.null(input$port_container_filter) && length(input$port_container_filter) > 0 && 
        !("All" %in% input$port_container_filter)) {
      port_catalog <- port_catalog %>% filter(container %in% input$port_container_filter)
    }
    
    # Add sorting options
    sort_options <- c("Pathway" = "pathway", 
                     "Container" = "container", 
                     "Product Pathway" = "product_pathway",
                     "Product Use" = "product_subpathway",
                     "Risk (High to Low)" = "risk_desc",
                     "Risk (Low to High)" = "risk_asc")
    
    # Apply sorting based on input (default to pathway)
    sort_by <- if (!is.null(input$port_sort_by)) input$port_sort_by else "pathway"
    
    if (sort_by == "risk_desc" && "total_risk" %in% names(port_catalog)) {
      port_catalog <- port_catalog %>% arrange(desc(total_risk))
    } else if (sort_by == "risk_asc" && "total_risk" %in% names(port_catalog)) {
      port_catalog <- port_catalog %>% arrange(total_risk)
    } else if (sort_by %in% names(port_catalog)) {
      port_catalog <- port_catalog %>% arrange(!!sym(sort_by))
    }
    
    # Create readable display names
    port_catalog <- port_catalog %>%
      rowwise() %>%
      mutate(
        display_name = create_port_readable_name(pathway, container, product_pathway, product_subpathway)
      ) %>%
      ungroup()
    
    # Add scenario to display name when "all" is selected
    if (tolower(scenario) == "all" && "scenario" %in% colnames(port_catalog)) {
      port_catalog <- port_catalog %>%
        mutate(display_name = paste0(display_name, " (", scenario, ")"))
    }
      # Check if catalog is empty
    if (nrow(port_catalog) == 0) {
      # Show message when no layers are found
      return(tagList(
        h4(paste0("Available Layers (", scenario, " scenario)")),
        div(
          style = "padding: 20px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 4px; color: #721c24;",
          icon("exclamation-triangle"),
          strong(" No files found for this scenario."),
          p(style = "margin-top: 10px; margin-bottom: 0;",
            "Please check that the data files exist in the 'output/spatial layers/risk at ports' directory ",
            "with the naming pattern: layername_", scenario, ".gpkg")
        ),
        hr(),
        actionButton("refresh_port_layers", "Refresh Layers List", 
                     icon = icon("refresh"), class = "btn-sm")
      ))
    }
    
    # Create choices list
    choices_list <- setNames(port_catalog$layer_name, port_catalog$display_name)
      # Show some info about the available layers
    tagList(
      h4(paste0("Available Layers (", scenario, " scenario)")),
      p(paste0("Found ", nrow(port_catalog), " layer(s) matching your criteria")),
      # Add sorting dropdown
      selectInput("port_sort_by", "Sort layers by:",
                 choices = sort_options,
                 selected = sort_by),
      checkboxGroupInput(
        "selected_port_layers",
        "Select Port Risk Layers:",
        choices = choices_list,
        selected = NULL
      ),
      actionButton("refresh_port_layers", "Refresh Layers List", 
                   icon = icon("refresh"), class = "btn-sm")
    )
  })
  
  # Refresh button observer
  observeEvent(input$refresh_port_layers, {
    # Force re-evaluation of port_layer_selector
    session$userData$portLayersLastUpdate <- Sys.time()
  })
  
  # Update port filter choices when data loads
  observe({
    port_catalog <- available_port_layers()
    pathways <- unique(port_catalog$pathway)
    pathways <- pathways[!is.na(pathways)]
    containers <- unique(port_catalog$container)
    containers <- containers[!is.na(containers)]
    
    updateSelectInput(session, "port_pathway_filter", choices = c("All", pathways))
    updateSelectInput(session, "port_container_filter", choices = c("All", containers))
  })  # Load the selected port layers
  port_layers_data <- reactive({
    # Only update when button is pressed (unless auto-update is enabled)
    if (!input$port_auto_update) {
      input$port_update_map
    }
    
    # Handle case when no layers are selected
    if(is.null(input$selected_port_layers) || length(input$selected_port_layers) == 0) {
      return(NULL)
    }
    
    scenario <- input$scenario_filter
    
    isolate({
      # Get the selected layer filenames
      selected <- input$selected_port_layers
      
      if (length(selected) == 0) {
        return(NULL)
      }
      
      # Create a progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Loading port risk data", value = 0)
      on.exit(progress$close())
      
      # Set up step size for progress bar
      step_size <- 1 / length(selected)      # Load the port risk layers
      port_layers <- lapply(seq_along(selected), function(i) {
        layer_name <- selected[i]
        progress$set(value = i * step_size, detail = paste("Loading", layer_name))
        
        # Handle the "All" scenario differently
        if (scenario == "all") {
          # Look for all possible scenario files for this layer
          scenario_options <- c("current", "mex_can", "other", "EAN", "IBL")
          all_scenario_layers <- list()
          
          # Try each possible scenario suffix and collect ALL matching files
          for (scenario_option in scenario_options) {
            scenario_file_path <- here("output", "spatial layers", "risk at ports", 
                                      paste0(layer_name, "_", scenario_option, ".gpkg"))
            if (file.exists(scenario_file_path)) {
              # Read this scenario's file
              layer <- tryCatch({
                layer <- st_read(scenario_file_path, quiet = TRUE)
                
                # Verify it has geometry
                if (nrow(layer) == 0 || !st_geometry_type(layer)[1] %in% c("POINT", "MULTIPOINT")) {
                  warning("Layer has no point geometries: ", layer_name, " for scenario ", scenario_option)
                  NULL
                } else {
                  # Transform to WGS84 for Leaflet compatibility
                  layer <- st_transform(layer, 4326)
                  
                  # Add layer name attribute for identification
                  layer$layer_name <- layer_name
                  layer$scenario <- scenario_option  # Note the actual scenario used
                  
                  layer
                }
              }, error = function(e) {
                warning("Error reading ", scenario_file_path, ": ", e$message)
                NULL
              })
              
              # Add to list if successfully loaded
              if (!is.null(layer)) {
                all_scenario_layers[[scenario_option]] <- layer
              }
            }
          }
          
          # Try without any scenario suffix as last resort
          alt_file_path <- here("output", "spatial layers", "risk at ports", 
                               paste0(layer_name, ".gpkg"))
          if (file.exists(alt_file_path)) {
            layer <- tryCatch({
              layer <- st_read(alt_file_path, quiet = TRUE)
              
              if (nrow(layer) > 0 && st_geometry_type(layer)[1] %in% c("POINT", "MULTIPOINT")) {
                layer <- st_transform(layer, 4326)
                layer$layer_name <- layer_name
                layer$scenario <- "unknown"
                layer
              } else {
                NULL
              }
            }, error = function(e) {
              NULL
            })
            
            if (!is.null(layer)) {
              all_scenario_layers[["unknown"]] <- layer
            }
          }
          
          # Return all layers found for this layer_name (will be combined later)
          if (length(all_scenario_layers) == 0) {
            warning("No files found for layer: ", layer_name)
            return(NULL)
          }
          
          # Return the list of all scenario layers for this layer_name
          return(all_scenario_layers)
          
        } else {
          # For specific scenarios, use the selected scenario
          file_path <- here("output", "spatial layers", "risk at ports", 
                           paste0(layer_name, "_", scenario, ".gpkg"))
          
          # Check if file exists
          if (!file.exists(file_path)) {
            # Try without scenario suffix as fallback
            alt_file_path <- here("output", "spatial layers", "risk at ports", 
                               paste0(layer_name, ".gpkg"))          # If still doesn't exist, give warning and return NULL
            if (!file.exists(alt_file_path)) {
              warning("Port risk file not found: ", file_path)
              return(NULL)
            } else {
              file_path <- alt_file_path
            }
          }
          
          # Handle regular scenario case - read the file we found
          tryCatch({
            layer <- st_read(file_path, quiet = TRUE)
            
            # Verify it has geometry
            if (nrow(layer) == 0 || !st_geometry_type(layer)[1] %in% c("POINT", "MULTIPOINT")) {
              warning("Layer has no point geometries: ", layer_name)
              return(NULL)
            }
            
            # Transform to WGS84 for Leaflet compatibility
            layer <- st_transform(layer, 4326)
            
            # Add layer name attribute for identification
            layer$layer_name <- layer_name
            layer$scenario <- scenario
            
            return(layer)
          }, error = function(e) {
            warning("Error reading ", file_path, ": ", e$message)
            return(NULL)
          })
        }
        
        # If we reach here with "all" scenario and no files found, return NULL
        return(NULL)
      })
        # Remove any NULL layers
      port_layers <- port_layers[!sapply(port_layers, is.null)]
      
      if (length(port_layers) == 0) {
        showNotification("No valid port risk layers found for the selected criteria.", 
                        type = "warning", duration = 5)
        return(NULL)
      }
      
      # Flatten the list structure: when scenario = "all", each element may be a list of layers
      # from different scenarios, so we need to flatten it to a simple list of layers
      flat_layers <- list()
      for (item in port_layers) {
        if (is.list(item) && !inherits(item, "sf")) {
          # This is a nested list (from "all" scenario), add all its elements
          flat_layers <- c(flat_layers, item)
        } else if (inherits(item, "sf")) {
          # This is a single sf object, add it directly
          flat_layers <- c(flat_layers, list(item))
        }
      }
      
      # Combine layers and summarize total_mean_kg for the same geometry
      combined_ports <- bind_rows(flat_layers)

      # Standardize LOCATION_ID to double type to handle mixed types from different scenarios
      # IBL uses CBP_CODE (double), while other scenarios may use numeric IDs
      if ("LOCATION_ID" %in% names(combined_ports)) {
        combined_ports <- combined_ports %>%
          mutate(LOCATION_ID = as.double(LOCATION_ID))
      }
      
      # Find risk value column - might be total_mean_kg or another column with "risk" in the name
      risk_col <- names(combined_ports)[grep("risk|total_mean_kg", names(combined_ports), ignore.case = TRUE)]
      if(length(risk_col) == 0) risk_col <- "total_mean_kg"
      
      # Ensure the risk column exists, if not create it with zeros
      if(!risk_col %in% names(combined_ports)) {
        combined_ports[[risk_col]] <- 0
        warning("Risk column not found, created empty column: ", risk_col)
      }
      
      # Summarize points with the same geometry by summing risk values
      progress$set(message = "Summarizing risk at locations", value = 0.9)
      
      # Convert to regular dataframe with coordinates for detailed grouping
      coords_df <- st_coordinates(combined_ports)
      combined_ports$lon <- coords_df[, "X"]
      combined_ports$lat <- coords_df[, "Y"]      # Group by coordinates (rounded to handle slight differences) and create detailed summaries
      # First create a lookup table for component details (drop geometry to avoid sf join error)
      component_lookup <- combined_ports %>%
        st_drop_geometry() %>%
        mutate(
          lon_rounded = round(lon, 6),
          lat_rounded = round(lat, 6),
          location_key = paste(lon_rounded, lat_rounded, sep = "_")
        ) %>%
        group_by(location_key) %>%
        summarise(
          component_details = list(data.frame(
            layer_name = layer_name,
            risk_value = get(risk_col),
            scenario = scenario,
            stringsAsFactors = FALSE
          )),
          .groups = 'drop'
        )
        # Now summarize the main data (drop geometry first, rebuild later)
      combined_ports_summarized <- combined_ports %>%
        st_drop_geometry() %>%
        mutate(
          lon_rounded = round(lon, 6),
          lat_rounded = round(lat, 6),
          location_key = paste(lon_rounded, lat_rounded, sep = "_")
        ) %>%
        group_by(lon_rounded, lat_rounded, location_key) %>%
        summarize(
          total_mean_kg = sum(!!sym(risk_col), na.rm = TRUE),
          LOCATION_NAME = first(LOCATION_NAME),
          LOCATION_STATE_CODE = first(LOCATION_STATE_CODE),
          layer_name = paste(unique(layer_name), collapse = ", "),
          scenario = first(scenario),
          layer_count = dplyr::n(),
          lon = first(lon),
          lat = first(lat),
          .groups = 'drop'
        ) %>%
        left_join(component_lookup, by = "location_key") %>%
        select(-location_key) %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      progress$set(value = 1, message = "Processing complete")
      
      return(combined_ports_summarized)
    })
  })
    # Render the port risk map
  output$port_risk_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -95, lat = 39, zoom = 4)
  })
    # Update port risk map when combined layer changes
  observe({
    port_data <- tryCatch({
      port_layers_data()
    }, error = function(e) {
      warning("Error loading port layers data: ", e$message)
      NULL
    })
    
    scenario <- input$scenario_filter
    
    if (is.null(port_data) || nrow(port_data) == 0) {
      leafletProxy("port_risk_map") %>%
        clearMarkers() %>%
        clearControls()
      return()
    }
    
    # Find risk value column - this might vary based on your data structure
    risk_col <- "total_mean_kg"  # We're now using the summarized column
    
    # Apply risk threshold filter if enabled
    if (input$enable_risk_threshold) {
      # Calculate the threshold value (input$risk_threshold is a percentage)
      max_risk <- max(port_data[[risk_col]], na.rm = TRUE)
      threshold_value <- max_risk * (input$risk_threshold / 100)
      
      # Filter out points above the threshold
      port_data <- port_data %>%
        filter(!!sym(risk_col) <= threshold_value)
      
      # Check if we filtered out all points
      if (nrow(port_data) == 0) {
        showNotification("All data points were filtered out by the risk threshold. Try a higher threshold.", 
                         type = "warning", duration = 5)
        leafletProxy("port_risk_map") %>%
          clearMarkers() %>%
          clearControls()
        return()
      }
    }
    
    # Create color palette based on risk values
    pal <- colorNumeric(palette = input$color_scheme, domain = port_data[[risk_col]])
    
    # Update point size from slider
    point_size <- input$point_size
      # Ensure we have columns for display
    if(!"LOCATION_NAME" %in% names(port_data)) port_data$LOCATION_NAME <- "Unknown Location"
    if(!"layer_name" %in% names(port_data)) port_data$layer_name <- "Unknown Layer"
    if(!"layer_count" %in% names(port_data)) port_data$layer_count <- 1

    # Add stable marker ids for click capture
    port_data$marker_id <- paste0("port_", seq_len(nrow(port_data)))# Scale point size based on risk level
    # Calculate relative sizes between min_size (point_size/2) and max_size (point_size*1.5)
    min_size <- max(point_size/2, 2)  # Don't go below 2px
    max_size <- point_size*1.5

    # Calculate transparency values - minimum alpha = 0.1, maximum = 0.9
    min_alpha <- 0.1
    max_alpha <- 0.9
    
    # Get risk range
    min_risk <- min(port_data[[risk_col]], na.rm = TRUE)
    max_risk <- max(port_data[[risk_col]], na.rm = TRUE)
    
    # If there's no variation in risk, use constant values
    if (max_risk == min_risk) {
      adjusted_size <- rep(point_size, nrow(port_data))
      adjusted_opacity <- rep(0.8, nrow(port_data))
    } else {
      # Scale sizes and opacity based on risk values
      risk_scale <- (port_data[[risk_col]] - min_risk) / (max_risk - min_risk)
      adjusted_size <- min_size + risk_scale * (max_size - min_size)
      adjusted_opacity <- min_alpha + risk_scale * (max_alpha - min_alpha)
    }    # Store the displayed data for click handling
    displayed_port_data(port_data)
    
    # Create custom cluster icon function that colors by total risk
    # Get the risk range for color scaling
    risk_range <- range(port_data[[risk_col]], na.rm = TRUE)
    
    # Generate JavaScript code for the icon creation function
    # This function will sum the risk values and color the cluster accordingly
    icon_create_js <- sprintf(
      "function(cluster) {
        var markers = cluster.getAllChildMarkers();
        var totalRisk = 0;
        for (var i = 0; i < markers.length; i++) {
          totalRisk += markers[i].options.risk_value || 0;
        }
        
        // Color scale based on risk (viridis approximation)
        var minRisk = %f;
        var maxRisk = %f;
        var riskScale = (totalRisk - minRisk) / (maxRisk - minRisk);
        riskScale = Math.max(0, Math.min(1, riskScale)); // Clamp between 0 and 1
        
        // Get color from the selected palette
        var color;
        var palette = '%s';
        
        // Approximate color palettes in JavaScript
        if (palette === 'magma') {
          if (riskScale < 0.25) color = '#000004';
          else if (riskScale < 0.5) color = '#7e03a8';
          else if (riskScale < 0.75) color = '#f1605d';
          else color = '#fcfdbf';
        } else if (palette === 'plasma') {
          if (riskScale < 0.25) color = '#0d0887';
          else if (riskScale < 0.5) color = '#cc4778';
          else if (riskScale < 0.75) color = '#f89540';
          else color = '#f0f921';
        } else if (palette === 'inferno') {
          if (riskScale < 0.25) color = '#000004';
          else if (riskScale < 0.5) color = '#9f2a63';
          else if (riskScale < 0.75) color = '#f98e09';
          else color = '#fcffa4';
        } else if (palette === 'cividis') {
          if (riskScale < 0.25) color = '#00224e';
          else if (riskScale < 0.5) color = '#575d6d';
          else if (riskScale < 0.75) color = '#a69c75';
          else color = '#fdea45';
        } else { // viridis default
          if (riskScale < 0.25) color = '#440154';
          else if (riskScale < 0.5) color = '#31688e';
          else if (riskScale < 0.75) color = '#35b779';
          else color = '#fde724';
        }
        
        // Size scales with risk
        var size = 30 + (riskScale * 20);
        
        return L.divIcon({
          html: '<div style=\"background-color:' + color + '; width: ' + size + 'px; height: ' + size + 'px; border-radius: 50%%; border: 2px solid white; display: flex; align-items: center; justify-content: center; font-weight: bold; color: white; text-shadow: 1px 1px 2px black;\"><span>' + totalRisk.toFixed(2) + '</span></div>',
          className: 'custom-cluster-icon',
          iconSize: L.point(size, size)
        });
      }",
      risk_range[1], risk_range[2], input$color_scheme
    )
      # Use clearGroup to properly clear clustered markers
    leafletProxy("port_risk_map") %>%
      clearGroup("port_markers") %>%
      clearControls() %>%
      addCircleMarkers(
        data = port_data,
        group = "port_markers",  # Add group identifier
        layerId = ~marker_id,
        radius = adjusted_size,  # Use scaled size based on risk
        fillColor = ~pal(total_mean_kg),
        color = "black",
        weight = 0,
        opacity = 1,
        fillOpacity = adjusted_opacity,  # Use scaled opacity based on risk
        # Add risk_value as a custom option for cluster calculation
        options = list(risk_value = port_data[[risk_col]]),
        popup = ~paste0(
          "<strong>", LOCATION_NAME, "</strong><br>",
          "Combined Risk: ", round(total_mean_kg, 4), "<br>",
          "Layers: ", layer_count
        ),
        clusterOptions = markerClusterOptions(
          showCoverageOnHover = TRUE,
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          removeOutsideVisibleBounds = TRUE,
          maxClusterRadius = 80,
          iconCreateFunction = JS(icon_create_js)
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = port_data[[risk_col]],
        title = paste0("Combined Risk Level (", scenario, ")\nColor & Size = Risk Value"),
        opacity = 0.8,
        layerId = "port_legend"  # Add layerId for proper legend replacement
      )
  })# Scenario information text
  output$scenario_info <- renderText({
    scenario <- input$scenario_filter
    
    # Format the scenario name nicely for display
    scenario_display <- switch(scenario,
                              "IBL" = "Illegal Boat Landings",
                              "current" = "Current Risk Assessment",
                              "mex_can" = "Mexico/Canada Scenario",
                              "other" = "Other International Scenario",
                              paste("Scenario:", scenario))
    
    # Count total available layers for this scenario
    port_catalog <- available_port_layers()
    total_layers <- nrow(port_catalog)
    
    return(paste0(scenario_display, " (", total_layers, " available layers)"))
  })  # Active port layers count
  output$active_port_layers_count <- renderText({
    port_data <- port_layers_data()
    
    if (is.null(port_data)) {
      return("No active layers selected")
    }
    
    # Find risk value column
    risk_col <- "total_mean_kg"
    
    # Apply risk threshold filter if enabled (only for display purposes)
    filtered_data <- port_data
    threshold_applied <- FALSE
    
    if (input$enable_risk_threshold) {
      # Calculate the threshold value (input$risk_threshold is a percentage)
      max_risk <- max(port_data[[risk_col]], na.rm = TRUE)
      threshold_value <- max_risk * (input$risk_threshold / 100)
      
      # Filter out points above the threshold
      filtered_data <- port_data %>%
        filter(!!sym(risk_col) <= threshold_value)
      
      threshold_applied <- TRUE
    }
    
    # Count unique locations and total selected layers
    total_locations <- nrow(filtered_data)
    original_locations <- nrow(port_data)
    
    # Get the total number of selected layers (from the UI selection)
    total_selected_layers <- length(input$selected_port_layers)
    
    # Count how many locations have multiple layers
    if("layer_count" %in% names(filtered_data)) {
      multi_layer_locations <- sum(filtered_data$layer_count > 1)
      max_layers_at_one_location <- max(filtered_data$layer_count)
    } else {
      multi_layer_locations <- 0
      max_layers_at_one_location <- 1
    }
    
# Check for various possible risk column names
possible_risk_cols <- c("total_mean_kg", "TOTAL_KG", "total_risk")
    
# Find the first matching column that exists in the data
risk_col <- NULL
for (col in possible_risk_cols) {
  if (col %in% names(filtered_data)) {
    risk_col <- col
    break
  }
}

# If no matching column found, use a default and warn
if (is.null(risk_col)) {
  warning("None of the expected risk columns found in data. Using first numeric column.")
  # Find the first numeric column as fallback
  numeric_cols <- sapply(filtered_data, is.numeric)
  if (any(numeric_cols)) {
    risk_col <- names(filtered_data)[which(numeric_cols)[1]]
  } else {
    # Last resort - create a dummy column with zeros
    filtered_data$dummy_risk <- 0
    risk_col <- "dummy_risk"
  }
}
    
# Calculate total risk using the identified column
total_risk <- sum(filtered_data[[risk_col]], na.rm = TRUE)
original_risk <- sum(port_data[[risk_col]], na.rm = TRUE)
    
# Create the summary text with threshold info if applied
if (threshold_applied) {
  # Calculate percentage of risk and locations removed
  pct_risk_remaining <- round(100 * total_risk / original_risk, 1)
  pct_locations_remaining <- round(100 * total_locations / original_locations, 1)
  
  return(paste0(
    total_selected_layers, " active layers at ", total_locations, " locations",
    " (showing ", pct_locations_remaining, "% of locations)\n",
    multi_layer_locations, " locations have multiple layers (max: ", max_layers_at_one_location, ")\n",
    "Total risk value: ", formatC(total_risk, format = "f", digits = 2),
    " (showing ", pct_risk_remaining, "% of total risk)\n",
    "Risk threshold filter: ", input$risk_threshold, "% of max value"
  ))
} else {
  # Original summary without threshold info
  return(paste0(
    total_selected_layers, " active layers at ", total_locations, " locations\n",
    multi_layer_locations, " locations have multiple layers (max: ", max_layers_at_one_location, ")\n",
    "Total risk value: ", formatC(total_risk, format = "f", digits = 2)
  ))
}
  })  # Create data table for port risk data
  output$port_data_table <- renderDT({
    port_data <- tryCatch({
      port_layers_data()
    }, error = function(e) {
      warning("Error getting port data for table: ", e$message)
      NULL
    })
    
    if (is.null(port_data) || nrow(port_data) == 0) {
      return(datatable(data.frame(Message = "No data available"),
                      options = list(dom = 't'),
                      rownames = FALSE))
    }
    
    # The risk column should now be total_mean_kg from our summarization
    risk_col <- "total_mean_kg"
    
    # Apply risk threshold filter if enabled
    if (input$enable_risk_threshold) {
      # Calculate the threshold value (input$risk_threshold is a percentage)
      max_risk <- max(port_data[[risk_col]], na.rm = TRUE)
      threshold_value <- max_risk * (input$risk_threshold / 100)
      
      # Filter out points above the threshold
      port_data <- port_data %>%
        filter(!!sym(risk_col) <= threshold_value)
      
      if (nrow(port_data) == 0) {
        return(NULL)
      }
    }
    
    # Ensure necessary columns exist
    if(!"layer_name" %in% names(port_data)) port_data$layer_name <- "Unknown"
    if(!"LOCATION_NAME" %in% names(port_data)) port_data$LOCATION_NAME <- "Unknown"
    if(!"LOCATION_STATE_CODE" %in% names(port_data)) port_data$LOCATION_STATE_CODE <- "NA"
    if(!"layer_count" %in% names(port_data)) port_data$layer_count <- 1
    
    # Drop geometry to create a regular dataframe
    port_table_data <- st_drop_geometry(port_data)
    
    # Create the table columns
    port_table_data$Location <- port_table_data$LOCATION_NAME
    port_table_data$State <- port_table_data$LOCATION_STATE_CODE
    port_table_data$Layers <- port_table_data$layer_count
    port_table_data$Risk <- port_table_data[[risk_col]]
    port_table_data$LayerNames <- port_table_data$layer_name
    
    # Select only the columns we want
    port_table_data <- port_table_data[, c("Location", "State", "Layers", "Risk", "LayerNames")]
    
    # Sort by risk
    port_table_data <- port_table_data[order(port_table_data$Risk, decreasing = TRUE), ]
      # Return a formatted datatable
    datatable(port_table_data, 
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                dom = 'ftip'
              ),
              rownames = FALSE) %>%
      formatRound(columns = 'Risk', digits = 4)
  })
    #====================================
  # PORT MAP CLICK FUNCTIONALITY
  #====================================
  
  # Reactive value to store clicked location data and the displayed data
  clicked_location <- reactiveVal(NULL)
  displayed_port_data <- reactiveVal(NULL)
  
  # Observer for map clicks
  observeEvent(input$port_risk_map_marker_click, {
    click <- input$port_risk_map_marker_click
    
    if (!is.null(click)) {
      # Get the marker_id from the click
      clicked_id <- click$id
      
      # Get the displayed port data (filtered version)
      port_data <- displayed_port_data()
      
      if (!is.null(port_data) && nrow(port_data) > 0) {
        # Find the row with matching marker_id
        matching_rows <- which(port_data$marker_id == clicked_id)
        
        if (length(matching_rows) > 0) {
          clicked_data <- port_data[matching_rows[1], ]
          clicked_location(clicked_data)
        }
      }
    }
  })
  
  # Clear selection when button is clicked
  observeEvent(input$clear_selection, {
    clicked_location(NULL)
  })
  
  # Output to control visibility of location details panel
  output$location_clicked <- reactive({
    !is.null(clicked_location())
  })
  outputOptions(output, "location_clicked", suspendWhenHidden = FALSE)
  
  # Output for clicked location information
  output$clicked_location_info <- renderText({
    data <- clicked_location()
    
    if (is.null(data)) {
      return("")
    }
    
    # Extract coordinates
    coords <- st_coordinates(data)
    
    paste0(
      "Location: ", data$LOCATION_NAME, "\n",
      "State: ", data$LOCATION_STATE_CODE, "\n",
      "Coordinates: ", round(coords[1], 4), ", ", round(coords[2], 4), "\n",
      "Total Risk Value: ", round(data$total_mean_kg, 4), "\n",
      "Number of Components: ", data$layer_count, "\n",
      "Scenario: ", data$scenario
    )
  })
  
  # Output for clicked location component breakdown
  output$clicked_location_components <- renderDT({
    data <- clicked_location()
    
    if (is.null(data) || is.null(data$component_details)) {
      return(datatable(data.frame(Message = "No component data available"),
                      options = list(dom = 't'),
                      rownames = FALSE))
    }
    
    # Extract component details
    components_df <- data$component_details[[1]]
    
    if (is.null(components_df) || nrow(components_df) == 0) {
      return(datatable(data.frame(Message = "No component data available"),
                      options = list(dom = 't'),
                      rownames = FALSE))
    }
    
    # Clean up and format the component data
    components_df <- components_df %>%
      arrange(desc(risk_value)) %>%
      mutate(
        risk_value = round(risk_value, 4),
        percentage = round(100 * risk_value / sum(risk_value), 1)
      ) %>%
      rename(
        "Layer Name" = layer_name,
        "Risk Value" = risk_value,
        "% of Total" = percentage,
        "Scenario" = scenario
      )
    
    # Return formatted datatable
    datatable(components_df,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'ftip',
                order = list(list(1, 'desc'))  # Sort by Risk Value descending
              ),
              rownames = FALSE) %>%
      formatRound(columns = 'Risk Value', digits = 4)
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
