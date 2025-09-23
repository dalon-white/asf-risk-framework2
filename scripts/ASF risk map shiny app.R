# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(here)
library(viridis)
library(DT)
library(rlang)  # For sym() function

# Suppress R CMD check NOTEs about non-standard evaluation
globalVariables(c(
  "pathway", "container", "geom", "risk_value", "short_name", "layer_name", 
  "total_risk", "risk_percentage", "components", "LOCATION_NAME", 
  "LOCATION_STATE_CODE", "Risk", "Layer", "Location", "State"
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
          # Dynamic checkboxes for layer selection
          uiOutput("layer_selector"),
          hr(),
          # Category filters
          selectInput("pathway_filter", "Filter by Pathway:", choices = NULL, multiple = TRUE),
          selectInput("container_filter", "Filter by Container:", choices = NULL, multiple = TRUE),
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
                                  "EANs" = "EAN"),
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
          actionButton("port_update_map", "Update Map"),
          hr(),          # Point size control
          sliderInput("point_size", "Point Size:", min = 3, max = 15, value = 8),
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
                     animate = animationOptions(interval = 300))
        ),        mainPanel(
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
  
  # Load all available layers
  available_layers <- reactive({
    # Read the layer catalog
    layer_catalog <- read.csv(here("output", "aqim total distributed risk", "distributed_risk_summary.csv"))
    return(layer_catalog)
  })
  
  # Create dynamic layer checkboxes based on available layers
  output$layer_selector <- renderUI({
    catalog <- available_layers()
    
    # Apply filters if selected
    if (!is.null(input$pathway_filter) && length(input$pathway_filter) > 0 && 
        !("All" %in% input$pathway_filter)) {
      catalog <- catalog %>% filter(pathway %in% input$pathway_filter)
    }
    if (!is.null(input$container_filter) && length(input$container_filter) > 0 && 
        !("All" %in% input$container_filter)) {
      catalog <- catalog %>% filter(container %in% input$container_filter)
    }
    
    # Create checkboxes
    checkboxGroupInput(
      "selected_layers",
      "Select Layers:",
      choices = setNames(catalog$short_name, catalog$layer_name),
      selected = NULL
    )
  })
  
  # Update filter choices when data loads
  observe({
    catalog <- available_layers()
    updateSelectInput(session, "pathway_filter", choices = c("All", unique(catalog$pathway)))
    updateSelectInput(session, "container_filter", choices = c("All", unique(catalog$container)))
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
        file_path <- here("output", "spatial layers", "risk at final destinations", 
                          paste0("risk_", layer_name, ".gpkg"))
        
        # Check if file exists
        if (!file.exists(file_path)) {
          warning("File not found: ", file_path)
          return(NULL)
        }
        
        # Read the layer and transform to WGS84
        layer <- st_read(file_path, quiet = TRUE)
        
        # Debug info - uncomment if needed
        # print(paste("Loaded layer:", layer_name, "with", nrow(layer), "features"))
        # print(paste("Original CRS:", st_crs(layer)$input))
        
        # Transform to WGS84 for Leaflet compatibility
        layer <- st_transform(layer, 4326)
        
        return(layer)
      })
      
      # Remove any NULL layers (files that couldn't be loaded)
      all_layers <- all_layers[!sapply(all_layers, is.null)]
      
      if (length(all_layers) == 0) {
        return(NULL)
      }
      
      # Combine layers by summing risk values at each location
      combined <- bind_rows(all_layers) %>%
        group_by(geom) %>%
        summarize(combined_risk = sum(risk_value, na.rm = TRUE))
      
      # Ensure final data is in WGS84
      combined <- st_transform(combined, 4326)
      
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
    
    # Handle "all" scenario specially
    if (scenario == "all") {
      # Scan the directory for all files
      port_files_dir <- here("output", "spatial layers", "risk at ports")
      all_port_files <- list.files(path = port_files_dir, pattern = ".*\\.gpkg$", full.names = FALSE)
      
      # Get all unique layer names by removing scenario suffixes
      all_layers <- character()
      
      # Define all possible scenarios to look for
      all_scenarios <- c("current", "mex_can", "other", "EAN")
      
      # Collect all unique layers across all scenarios
      port_catalog_list <- list()
      
      for (s in all_scenarios) {
        # First try the summary file
        port_summary_path <- here("output", "spatial layers", "risk at ports", paste0("layer_summary_", s, ".csv"))
        
        if (file.exists(port_summary_path)) {
          # Read from summary file if it exists
          this_catalog <- read.csv(port_summary_path, stringsAsFactors = FALSE)
          this_catalog$scenario <- s
          port_catalog_list[[length(port_catalog_list) + 1]] <- this_catalog
        } else {
          # Filter files by this scenario
          scenario_pattern <- paste0("_", s, "\\.gpkg$")
          scenario_files <- all_port_files[grepl(scenario_pattern, all_port_files)]
          
          if (length(scenario_files) > 0) {
            # Create a catalog dataframe for this scenario
            this_catalog <- data.frame(
              filename = scenario_files,
              layer_name = gsub(paste0("_", s, "\\.gpkg$"), "", scenario_files),
              scenario = s,
              point_count = NA,
              total_risk = NA,
              stringsAsFactors = FALSE
            )
            port_catalog_list[[length(port_catalog_list) + 1]] <- this_catalog
          }
        }
      }
      
      # Combine all scenario catalogs
      if (length(port_catalog_list) > 0) {
        port_catalog <- bind_rows(port_catalog_list)
      } else {
        # Fallback if no files found for any scenario
        port_catalog <- data.frame(
          filename = character(),
          layer_name = character(),
          scenario = character(),
          point_count = numeric(),
          total_risk = numeric(),
          stringsAsFactors = FALSE
        )
      }
      
      # Add components for filtering
      port_catalog <- port_catalog %>%
        mutate(
          components = strsplit(as.character(layer_name), "_"),
          pathway = sapply(components, function(x) if(length(x) > 0) x[1] else NA),
          container = sapply(components, function(x) if(length(x) > 1) x[2] else NA),
          product_pathway = sapply(components, function(x) if(length(x) > 2) x[3] else NA),
          product_subpathway = sapply(components, function(x) if(length(x) > 3) x[4] else NA)
        ) %>%
        select(-components)
      
      return(port_catalog)
    }
    
    # For a specific scenario (not "all")
    # First try to read the summary file
    port_summary_path <- here("output", "spatial layers", "risk at ports", paste0("layer_summary_", scenario, ".csv"))
    
    if (file.exists(port_summary_path)) {
      # Read from summary file if it exists
      port_catalog <- read.csv(port_summary_path, stringsAsFactors = FALSE)
      
      # Add components for filtering
      port_catalog <- port_catalog %>%
        mutate(
          components = strsplit(as.character(layer_name), "_"),
          pathway = sapply(components, function(x) if(length(x) > 0) x[1] else NA),
          container = sapply(components, function(x) if(length(x) > 1) x[2] else NA),
          product_pathway = sapply(components, function(x) if(length(x) > 2) x[3] else NA),
          product_subpathway = sapply(components, function(x) if(length(x) > 3) x[4] else NA)
        ) %>%
        select(-components)
      
      return(port_catalog)
    } else {
      # If summary doesn't exist, scan the directory
      port_files_dir <- here("output", "spatial layers", "risk at ports")
      all_port_files <- list.files(path = port_files_dir, pattern = ".*\\.gpkg$", full.names = FALSE)
      
      # Filter files by scenario
      scenario_pattern <- paste0("_", scenario, "\\.gpkg$")
      port_files <- all_port_files[grepl(scenario_pattern, all_port_files)]
      
      # If no files match the scenario, fall back to all files
      if (length(port_files) == 0) {
        message("No files found for scenario: ", scenario, ". Using all files.")
        port_files <- all_port_files
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
      
      # Extract components from layer names
      port_catalog <- port_catalog %>%
        mutate(
          components = strsplit(as.character(layer_name), "_"),
          pathway = sapply(components, function(x) if(length(x) > 0) x[1] else NA),
          container = sapply(components, function(x) if(length(x) > 1) x[2] else NA),
          product_pathway = sapply(components, function(x) if(length(x) > 2) x[3] else NA),
          product_subpathway = sapply(components, function(x) if(length(x) > 3) x[4] else NA)
        ) %>%
        select(-components)
      
      return(port_catalog)
    }
  })
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
    
    # Show some info about the available layers
    tagList(
      h4(paste0("Available Layers (", scenario, " scenario)")),
      p(paste0("Found ", nrow(port_catalog), " layer(s) matching your criteria")),
      checkboxGroupInput(
        "selected_port_layers",
        "Select Port Risk Layers:",
        choices = setNames(port_catalog$layer_name, port_catalog$layer_name),
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
    
    req(input$selected_port_layers)
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
      step_size <- 1 / length(selected)
        # Load the port risk layers
      port_layers <- lapply(seq_along(selected), function(i) {
        layer_name <- selected[i]
        progress$set(value = i * step_size, detail = paste("Loading", layer_name))
        
        # For "all" scenario, try to load the layer from each possible scenario
        if (scenario == "all") {
          # Try each possible scenario suffix
          all_scenarios <- c("current", "mex_can", "other", "EAN")
          
          # Store all layers found for this layer name across scenarios
          scenario_layers <- list()
          
          for (s in all_scenarios) {
            # Construct filename with this scenario
            file_path <- here("output", "spatial layers", "risk at ports", 
                             paste0(layer_name, "_", s, ".gpkg"))
            
            # If this scenario-specific file exists, load it
            if (file.exists(file_path)) {
              tryCatch({
                layer <- st_read(file_path, quiet = TRUE)
                
                # Verify it has geometry
                if (nrow(layer) > 0 && st_geometry_type(layer)[1] %in% c("POINT", "MULTIPOINT")) {
                  # Transform to WGS84 for Leaflet compatibility
                  layer <- st_transform(layer, 4326)
                  
                  # Add layer name and scenario attributes for identification
                  layer$layer_name <- layer_name
                  layer$scenario <- s
                  
                  # Add to our collection
                  scenario_layers[[length(scenario_layers) + 1]] <- layer
                }
              }, error = function(e) {
                warning("Error reading ", file_path, ": ", e$message)
              })
            }
          }
          
          # Also try without scenario suffix as fallback
          alt_file_path <- here("output", "spatial layers", "risk at ports", 
                              paste0(layer_name, ".gpkg"))
          
          if (file.exists(alt_file_path)) {
            tryCatch({
              layer <- st_read(alt_file_path, quiet = TRUE)
              
              # Verify it has geometry
              if (nrow(layer) > 0 && st_geometry_type(layer)[1] %in% c("POINT", "MULTIPOINT")) {
                # Transform to WGS84 for Leaflet compatibility
                layer <- st_transform(layer, 4326)
                
                # Add layer name and scenario attributes for identification
                layer$layer_name <- layer_name
                layer$scenario <- "unspecified"
                
                # Add to our collection
                scenario_layers[[length(scenario_layers) + 1]] <- layer
              }
            }, error = function(e) {
              warning("Error reading ", alt_file_path, ": ", e$message)
            })
          }
          
          # Combine all layers found for this layer name across scenarios
          if (length(scenario_layers) > 0) {
            combined_layer <- bind_rows(scenario_layers)
            return(combined_layer)
          } else {
            warning("No valid files found for layer: ", layer_name)
            return(NULL)
          }
        } else {
          # For a specific scenario, just try to load that one file
          # Construct filename with scenario
          file_path <- here("output", "spatial layers", "risk at ports", 
                           paste0(layer_name, "_", scenario, ".gpkg"))
          
          # Check if file exists
          if (!file.exists(file_path)) {
            # Try without scenario suffix as fallback
            alt_file_path <- here("output", "spatial layers", "risk at ports", 
                               paste0(layer_name, ".gpkg"))
            
            # If still doesn't exist, give warning and return NULL
            if (!file.exists(alt_file_path)) {
              warning("Port risk file not found: ", file_path)
              return(NULL)
            } else {
              file_path <- alt_file_path
            }
          }
          
          # Read the layer and transform to WGS84
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
      })
      
      # Remove any NULL layers
      port_layers <- port_layers[!sapply(port_layers, is.null)]
      
      if (length(port_layers) == 0) {
        showNotification("No valid port risk layers found for the selected criteria.", 
                        type = "warning", duration = 5)
        return(NULL)
      }
      
      # Combine layers and summarize total_mean_kg for the same geometry
      combined_ports <- bind_rows(port_layers)
      
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
      
      # Use the sf geometry to identify points at the same location
      combined_ports_summarized <- combined_ports %>%
        group_by(geom) %>%
        summarize(
          # Sum the risk values
          total_mean_kg = sum(!!sym(risk_col), na.rm = TRUE),
          # Keep other important attributes
          LOCATION_NAME = first(LOCATION_NAME),
          LOCATION_STATE_CODE = first(LOCATION_STATE_CODE),
          # Create combined layer name for identification
          layer_name = paste(unique(layer_name), collapse = ", "),
          scenario = first(scenario),
          # Count number of layers combined at this point
          layer_count = n()
        )
        progress$set(value = 1, message = "Processing complete")
      
      combined_ports_summarized <- combined_ports_summarized %>%
        mutate(original_total_mean_kg = total_mean_kg)  # Keep the original value for reference
      
      return(combined_ports_summarized)
    })
  })
  
  # Apply threshold filter to port layer data
  filtered_port_layers_data <- reactive({
    port_data <- port_layers_data()
    
    if (is.null(port_data) || !input$enable_risk_threshold) {
      return(port_data)  # Return unfiltered data if no filter enabled
    }
    
    # Find the risk column
    possible_risk_cols <- c("total_mean_kg", "TOTAL_KG", "total_risk")
    risk_col <- NULL
    for (col in possible_risk_cols) {
      if (col %in% names(port_data)) {
        risk_col <- col
        break
      }
    }
    
    if (is.null(risk_col)) {
      warning("No risk column found for filtering")
      return(port_data)
    }
    
    # Calculate threshold based on percentile of the risk values
    threshold_percentile <- input$risk_threshold / 100
    max_risk_value <- quantile(port_data[[risk_col]], probs = threshold_percentile, na.rm = TRUE)
    
    # Filter data to show only points below the threshold
    filtered_data <- port_data %>%
      filter(!!sym(risk_col) <= max_risk_value)
    
    # Add metadata about filtering
    attr(filtered_data, "threshold_value") <- max_risk_value
    attr(filtered_data, "original_count") <- nrow(port_data)
    attr(filtered_data, "filtered_count") <- nrow(filtered_data)
    attr(filtered_data, "percentile") <- threshold_percentile
    
    return(filtered_data)
  })
  
  # Update risk threshold slider based on the data
  observe({
    port_data <- port_layers_data()
    
    if (is.null(port_data)) {
      return()
    }
    
    # Find risk column
    possible_risk_cols <- c("total_mean_kg", "TOTAL_KG", "total_risk")
    risk_col <- NULL
    for (col in possible_risk_cols) {
      if (col %in% names(port_data)) {
        risk_col <- col
        break
      }
    }
    
    if (is.null(risk_col)) return()
    
    # Get min and max of the risk values
    max_val <- max(port_data[[risk_col]], na.rm = TRUE)
    min_val <- min(port_data[[risk_col]], na.rm = TRUE)
    
    # Only update if we have valid data
    if (!is.infinite(max_val) && !is.infinite(min_val) && max_val > min_val) {
      # Calculate a reasonable default value (showing 95% of the data)
      default_val <- 95
      
      # Update slider
      updateSliderInput(session, "risk_threshold", 
                        min = 1, max = 100, value = default_val, 
                        step = 1)
    }
  })
  
  # Render the port risk map
  output$port_risk_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -95, lat = 39, zoom = 4)
  })  # Update port risk map when combined layer changes
  observe({
    port_data <- filtered_port_layers_data()
    scenario <- input$scenario_filter
    
    if (is.null(port_data)) {
      leafletProxy("port_risk_map") %>%
        clearMarkers() %>%
        clearControls()
      return()
    }
    
    # Find risk value column - this might vary based on your data structure
    risk_col <- "total_mean_kg"  # We're now using the summarized column
    
    # Create color palette based on risk values
    pal <- colorNumeric(palette = input$color_scheme, domain = port_data[[risk_col]])
    
    # Update point size from slider
    point_size <- input$point_size
    
    # Ensure we have columns for the popup
    if(!"LOCATION_NAME" %in% names(port_data)) port_data$LOCATION_NAME <- "Unknown Location"
    if(!"layer_name" %in% names(port_data)) port_data$layer_name <- "Unknown Layer"
    if(!"layer_count" %in% names(port_data)) port_data$layer_count <- 1
    
    # Create the popup content
    popup_content <- mapply(function(loc_name, risk_value, layer_name, layer_count) {
      paste0(
        "<strong>Location:</strong> ", loc_name, "<br>",
        "<strong>Combined Risk Value:</strong> ", round(risk_value, 4), "<br>",
        "<strong>Layers (", layer_count, "):</strong> ", layer_name, "<br>",
        "<strong>Scenario:</strong> ", scenario
      )
    }, port_data$LOCATION_NAME, port_data[[risk_col]], port_data$layer_name, port_data$layer_count, SIMPLIFY = FALSE)
    
    # Get filter info for legend title
    filter_info <- ""
    if (input$enable_risk_threshold && !is.null(attr(port_data, "threshold_value"))) {
      max_value <- attr(port_data, "threshold_value")
      percentile <- attr(port_data, "percentile") * 100
      filter_info <- paste0(" (Filtered to ", percentile, "th percentile, max: ", round(max_value, 2), ")")
    }
    
    leafletProxy("port_risk_map") %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        data = port_data,
        radius = point_size,
        fillColor = ~pal(port_data[[risk_col]]),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.8,
        popup = popup_content
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = port_data[[risk_col]],
        title = paste0("Combined Risk Level", filter_info),
        opacity = 0.8
      )
  })  # Scenario information text
  output$scenario_info <- renderText({
    scenario <- input$scenario_filter
    
    # Format the scenario name nicely for display
    scenario_display <- switch(scenario,
                              "all" = "All Scenarios Combined",
                              "current" = "Current Risk Assessment",
                              "mex_can" = "Mexico/Canada Scenario",
                              "other" = "Other International Scenario",
                              "EAN" = "Emergency Action Notifications",
                              paste("Scenario:", scenario))
    
    # Count total available layers for this scenario
    port_catalog <- available_port_layers()
    total_layers <- nrow(port_catalog)
    
    # For "all" scenario, count unique layer names (ignoring scenario)
    if (scenario == "all") {
      unique_layers <- length(unique(port_catalog$layer_name))
      return(paste0(scenario_display, " (", unique_layers, " unique layers across ", 
                    length(unique(port_catalog$scenario)), " scenarios)"))
    } else {
      return(paste0(scenario_display, " (", total_layers, " available layers)"))
    }
  })# Active port layers count
  output$active_port_layers_count <- renderText({
    raw_data <- port_layers_data()
    port_data <- filtered_port_layers_data()
    
    if (is.null(port_data)) {
      return("No active layers selected")
    }
    
    # Count unique locations and total selected layers
    total_locations <- nrow(port_data)
    
    # Get the total number of selected layers (from the UI selection)
    total_selected_layers <- length(input$selected_port_layers)
    
    # Count how many locations have multiple layers
    if("layer_count" %in% names(port_data)) {
      multi_layer_locations <- sum(port_data$layer_count > 1)
      max_layers_at_one_location <- max(port_data$layer_count)
    } else {
      multi_layer_locations <- 0
      max_layers_at_one_location <- 1
    }
    
    # Check for various possible risk column names
    possible_risk_cols <- c("total_mean_kg", "TOTAL_KG", "total_risk")
    
    # Find the first matching column that exists in the data
    risk_col <- NULL
    for (col in possible_risk_cols) {
      if (col %in% names(port_data)) {
        risk_col <- col
        break
      }
    }
    
    # If no matching column found, use a default and warn
    if (is.null(risk_col)) {
      warning("None of the expected risk columns found in data. Using first numeric column.")
      # Find the first numeric column as fallback
      numeric_cols <- sapply(port_data, is.numeric)
      if (any(numeric_cols)) {
        risk_col <- names(port_data)[which(numeric_cols)[1]]
      } else {
        # Last resort - create a dummy column with zeros
        port_data$dummy_risk <- 0
        risk_col <- "dummy_risk"
      }
    }
    
    # Calculate total risk using the identified column
    total_risk <- sum(port_data[[risk_col]], na.rm = TRUE)
    
    # Add filtering information if enabled
    filter_info <- ""
    if (input$enable_risk_threshold && !is.null(attr(port_data, "threshold_value"))) {
      original_count <- attr(port_data, "original_count")
      filtered_count <- attr(port_data, "filtered_count")
      filtered_out <- original_count - filtered_count
      threshold_value <- attr(port_data, "threshold_value")
      percentile <- attr(port_data, "percentile") * 100
      
      filter_info <- paste0(
        "\nFiltering applied: Showing ", filtered_count, " of ", original_count, " locations",
        " (", filtered_out, " filtered out)",
        "\nThreshold: ", round(threshold_value, 2), " (", percentile, "th percentile)"
      )
    }
    
    # Create the summary text
    return(paste0(
      total_selected_layers, " active layers at ", total_locations, " locations\n",
      multi_layer_locations, " locations have multiple layers (max: ", max_layers_at_one_location, ")\n",
      "Total risk value: ", formatC(total_risk, format = "f", digits = 2),
      filter_info
    ))
  })  # Create data table for port risk data
  output$port_data_table <- renderDT({
    port_data <- filtered_port_layers_data()
    
    if (is.null(port_data)) {
      return(NULL)
    }
    
    # The risk column should now be total_mean_kg from our summarization
    risk_col <- "total_mean_kg"
    
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
              rownames = FALSE,
              caption = if(input$enable_risk_threshold && !is.null(attr(port_data, "threshold_value"))) {
                paste("Filtered to", attr(port_data, "percentile")*100, "percentile,", 
                      attr(port_data, "filtered_count"), "of", attr(port_data, "original_count"), "locations shown")
              } else NULL) %>%
      formatRound(columns = 'Risk', digits = 4)
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
