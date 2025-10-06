# ASF Risk Framework - Improved Hierarchical Chord Diagram with Flow Labels
# This script creates an enhanced version of the hierarchical chord diagram
# with removed directional arrows and added data flow labels

library(circlize)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(RColorBrewer)
library(viridis)
library(grid)
library(gridExtra)
library(here)

# Load the summarized data
# If you're running this directly, make sure to execute the data preparation code first
# This assumes summ_1 is available in the environment

# Prepare the data for hierarchical chord diagram
chord_data <- summ_1 %>%
  dplyr::select(PoE, Conveyance.type.pathway, Product.pathway, Product.subpathway, current) %>%
  filter(!is.na(current)) %>%
  # Create hierarchical groups
  mutate(
    # Create source group: PoE + Conveyance.type.pathway
    source_group = paste0(PoE, ":", Conveyance.type.pathway),
    # Create target group: Product.pathway + Product.subpathway
    target_group = paste0(Product.pathway, ":", Product.subpathway)
  ) %>%
  # Group by the new hierarchical categories and sum the values
  group_by(source_group, target_group) %>%
  summarise(value = sum(current, na.rm = TRUE), .groups = "drop")

# Sort the data by value to improve visualization
chord_data <- chord_data %>% arrange(desc(value))

# Create a matrix for the chord diagram
chord_matrix <- chord_data %>%
  pivot_wider(
    names_from = target_group,
    values_from = value,
    values_fill = 0
  ) %>%
  column_to_rownames("source_group") %>%
  as.matrix()

# Define custom grid colors based on the parent categories
source_parent_categories <- str_extract(rownames(chord_matrix), "^[^:]+")
target_parent_categories <- str_extract(colnames(chord_matrix), "^[^:]+")

# Get unique parent categories
all_parent_categories <- unique(c(source_parent_categories, target_parent_categories))

# Create a better color palette - using viridis for distinct colors
n_categories <- length(all_parent_categories)
viridis_colors <- viridis(min(9, n_categories), option = "D")
brewer_colors <- brewer.pal(min(9, n_categories), "Set1")
spectral_colors <- brewer.pal(min(11, n_categories), "Spectral")

# Combine color palettes if we have many categories
if (n_categories > 11) {
  parent_colors <- setNames(
    colorRampPalette(c(viridis_colors, brewer_colors, spectral_colors))(n_categories),
    all_parent_categories
  )
} else {
  # Use a mix of palettes for better distinction
  mixed_palette <- c(viridis_colors[1:min(5, n_categories)], 
                     brewer_colors[1:min(4, max(0, n_categories-5))],
                     spectral_colors[1:min(2, max(0, n_categories-9))])
  parent_colors <- setNames(mixed_palette[1:n_categories], all_parent_categories)
}

# Get colors for source and target groups
source_colors <- parent_colors[source_parent_categories]
target_colors <- parent_colors[target_parent_categories]

# Combine source and target colors
grid_colors <- c(source_colors, target_colors)

# Reset the circos environment
circos.clear()

# Parameters for better visualization
gap_degree <- 2  # Slightly reduced gap to use space more efficiently
transparency <- 0.7  # Transparency for better visibility
link_border <- "white"  # Border color for links
bg_color <- "#FCFCFC"  # Light background color

# Set up the plotting parameters - using equal margins to center the plot
par(mar = c(2, 2, 4, 2), bg = bg_color, cex = 1.1)

# Create the chord diagram with improved parameters for readability
circos.par(
  gap.degree = gap_degree,
  track.margin = c(0.03, 0.03), 
  start.degree = 90,
  clock.wise = FALSE,
  canvas.xlim = c(-0.85, 0.85),
  canvas.ylim = c(-0.85, 0.85)
)

# Create the chord diagram without directional arrows
chordDiagram(
  chord_matrix,
  grid.col = grid_colors,
  transparency = transparency,
  link.border = link_border,
  link.lwd = 0.6,
  link.sort = TRUE,
  link.largest.ontop = TRUE,
  annotationTrack = c("grid", "axis"),
  preAllocateTracks = list(track.height = 0.12),
  directional = 0,  # REMOVED DIRECTIONAL ARROWS
  annotationTrackHeight = c(0.05, 0.12),
  diffHeight = 0.05,
  reduce = 0.01
)

# Add flow value labels directly to the chord diagram
for(i in 1:nrow(chord_data)) {
  # Get source, target and value
  source_sector <- chord_data$source_group[i]
  target_sector <- chord_data$target_group[i]
  value <- chord_data$value[i]
  
  # Only show labels for significant flows (adjust threshold as needed)
  # Currently showing flows greater than 3% of maximum flow
  if(value > max(chord_data$value) * 0.03) {
    # Format value for display
    value_text <- format(round(value, 1), nsmall=1)
    
    # Add text label at the middle of the link
    circos.link(
      source_sector, get.cell.meta.data("xlim", sector.index=source_sector)[1] + 0.5, 
      target_sector, get.cell.meta.data("xlim", sector.index=target_sector)[1] + 0.5,
      h = 0.7, # Height of the curve
      col = "transparent", # Transparent link just for positioning
      border = FALSE
    )
    
    # Get the calculated position from the link
    link_coords <- circlize:::get.link.position(
      source_sector, get.cell.meta.data("xlim", sector.index=source_sector)[1] + 0.5,
      target_sector, get.cell.meta.data("xlim", sector.index=target_sector)[1] + 0.5,
      h = 0.7
    )
    
    # Place the text at the midpoint of the link
    x_mid <- mean(c(link_coords$x1, link_coords$x2))
    y_mid <- mean(c(link_coords$y1, link_coords$y2))
    
    # Add a small white background to make text more readable
    rect_width <- strwidth(value_text, cex=0.75) * 1.2
    rect_height <- strheight(value_text, cex=0.75) * 1.5
    rect(
      x_mid - rect_width/2, y_mid - rect_height/2,
      x_mid + rect_width/2, y_mid + rect_height/2,
      col = alpha("white", 0.7),
      border = NA
    )
    
    # Add the text
    text(
      x_mid, y_mid,
      value_text,
      cex = 0.75,
      col = "#333333",
      font = 2
    )
  }
}

# Add custom labels with improved parent/child hierarchy formatting AND value labels
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.name <- get.cell.meta.data("sector.index")
    # Extract parent and child parts
    parts <- strsplit(sector.name, ":")[[1]]
    parent <- parts[1]
    child <- ifelse(length(parts) > 1, parts[2], "")
    
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    # Get sector index to determine position in the circle
    sector.index = get.cell.meta.data("sector.numeric.index")
    total.sectors = get.cell.meta.data("num.sectors")
    
    # Calculate angle to determine the best text orientation
    sector_angle <- (sector.index / total.sectors) * 360
    
    # Choose text orientation based on position in the circle
    if (sector_angle > 90 && sector_angle < 270) {
      facing_direction <- "outside"
      adj_value <- c(0.5, 1)  # Adjust text position
      y_offset <- mm_y(-1)  # Move text away from circle
    } else {
      facing_direction <- "outside"
      adj_value <- c(0.5, 0)
      y_offset <- mm_y(2)  # Default position
    }
    
    # Adjust text size based on sector size and data value
    sector_size <- xlim[2] - xlim[1]
    sector_value <- sum(chord_matrix[sector.name,], na.rm=TRUE) + sum(chord_matrix[,sector.name], na.rm=TRUE)
    rel_importance <- sector_value / max(rowSums(chord_matrix) + colSums(chord_matrix), na.rm=TRUE)
    
    # Adjust text size based on sector size and importance
    text_cex <- min(1.0, max(0.65, sector_size/25 * (0.7 + 0.3*rel_importance)))
    
    # Position for the labels
    text_pos <- mean(xlim)
    
    # First line: Parent category (bold)
    circos.text(
      text_pos, ylim[1] + mm_y(3) + y_offset, 
      parent,
      cex = text_cex, adj = adj_value,
      facing = facing_direction, 
      niceFacing = TRUE,
      font = 2  # Bold font
    )
    
    if (child != "") {
      # Second line: Child category with indentation
      circos.text(
        text_pos, ylim[1] + mm_y(7) + y_offset, 
        paste0(" ", child),  # Add space for indentation
        cex = text_cex * 0.9, adj = adj_value,
        facing = facing_direction, 
        niceFacing = TRUE,
        col = "#404040"  # Dark gray for better visibility
      )
    }
    
    # Add value label - NEW ADDITION
    value_text <- format(round(sector_value, 1), nsmall=1)
    circos.text(
      text_pos, ylim[1] + mm_y(11) + y_offset, 
      paste0("(", value_text, ")"),
      cex = text_cex * 0.7, adj = adj_value,
      facing = facing_direction, 
      niceFacing = TRUE,
      col = "#666666"  # Medium gray for subtle display
    )
  }
)

# Add a more informative and attractive title
title(
  main = "ASF Risk Flow: Hierarchical Pathway Analysis",
  sub = "Source: Port of Entry + Conveyance Type → Target: Product Pathway + Subpathway",
  cex.main = 1.2,
  font.main = 2,
  col.main = "#333333",
  cex.sub = 0.8,
  col.sub = "#555555",
  line = 0.5
)

# Add a caption with data information
mtext(
  text = paste0("Data shows relative risk flows between", 
                " hierarchical categories (n=", nrow(chord_data), ")"),
  side = 1, 
  line = 0.5,
  cex = 0.8,
  col = "#666666"
)

# Define output directories
output_dir <- here::here("output", "html_reports")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Create filename with timestamp for uniqueness
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
png_filename <- file.path(output_dir, paste0("asf_improved_chord_diagram_", timestamp, ".png"))
pdf_filename <- file.path(output_dir, paste0("asf_improved_chord_diagram_", timestamp, ".pdf"))

# Save as high-resolution PNG file
png(png_filename, width = 2400, height = 2400, res = 300)

# Recreate the chord diagram for PNG output
circos.clear()
circos.par(
  gap.degree = gap_degree,
  track.margin = c(0.03, 0.03),
  start.degree = 90,
  clock.wise = FALSE,
  canvas.xlim = c(-0.85, 0.85),
  canvas.ylim = c(-0.85, 0.85)
)

# Create the chord diagram without directional arrows
chordDiagram(
  chord_matrix,
  grid.col = grid_colors,
  transparency = transparency,
  link.border = link_border,
  link.lwd = 0.6,
  link.sort = TRUE,
  link.largest.ontop = TRUE,
  annotationTrack = c("grid", "axis"),
  preAllocateTracks = list(track.height = 0.12),
  directional = 0,  # REMOVED DIRECTIONAL ARROWS
  annotationTrackHeight = c(0.05, 0.12),
  diffHeight = 0.05,
  reduce = 0.01
)

# Add flow value labels to PNG output
for(i in 1:nrow(chord_data)) {
  source_sector <- chord_data$source_group[i]
  target_sector <- chord_data$target_group[i]
  value <- chord_data$value[i]
  
  # Only show labels for significant flows
  if(value > max(chord_data$value) * 0.03) {
    value_text <- format(round(value, 1), nsmall=1)
    
    circos.link(
      source_sector, get.cell.meta.data("xlim", sector.index=source_sector)[1] + 0.5, 
      target_sector, get.cell.meta.data("xlim", sector.index=target_sector)[1] + 0.5,
      h = 0.7,
      col = "transparent",
      border = FALSE
    )
    
    link_coords <- circlize:::get.link.position(
      source_sector, get.cell.meta.data("xlim", sector.index=source_sector)[1] + 0.5,
      target_sector, get.cell.meta.data("xlim", sector.index=target_sector)[1] + 0.5,
      h = 0.7
    )
    
    x_mid <- mean(c(link_coords$x1, link_coords$x2))
    y_mid <- mean(c(link_coords$y1, link_coords$y2))
    
    rect_width <- strwidth(value_text, cex=0.75) * 1.2
    rect_height <- strheight(value_text, cex=0.75) * 1.5
    rect(
      x_mid - rect_width/2, y_mid - rect_height/2,
      x_mid + rect_width/2, y_mid + rect_height/2,
      col = alpha("white", 0.7),
      border = NA
    )
    
    text(
      x_mid, y_mid,
      value_text,
      cex = 0.75,
      col = "#333333",
      font = 2
    )
  }
}

# Add custom labels with values for PNG output
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.name <- get.cell.meta.data("sector.index")
    parts <- strsplit(sector.name, ":")[[1]]
    parent <- parts[1]
    child <- ifelse(length(parts) > 1, parts[2], "")
    
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.index = get.cell.meta.data("sector.numeric.index")
    total.sectors = get.cell.meta.data("num.sectors")
    sector_angle <- (sector.index / total.sectors) * 360
    
    if (sector_angle > 90 && sector_angle < 270) {
      facing_direction <- "outside"
      adj_value <- c(0.5, 1)
      y_offset <- mm_y(-1)
    } else {
      facing_direction <- "outside"
      adj_value <- c(0.5, 0)
      y_offset <- mm_y(2)
    }
    
    sector_size <- xlim[2] - xlim[1]
    sector_value <- sum(chord_matrix[sector.name,], na.rm=TRUE) + sum(chord_matrix[,sector.name], na.rm=TRUE)
    rel_importance <- sector_value / max(rowSums(chord_matrix) + colSums(chord_matrix), na.rm=TRUE)
    text_cex <- min(1.0, max(0.65, sector_size/25 * (0.7 + 0.3*rel_importance)))
    text_pos <- mean(xlim)
    
    circos.text(
      text_pos, ylim[1] + mm_y(3) + y_offset, 
      parent,
      cex = text_cex, adj = adj_value,
      facing = facing_direction, 
      niceFacing = TRUE,
      font = 2
    )
    
    if (child != "") {
      circos.text(
        text_pos, ylim[1] + mm_y(7) + y_offset, 
        paste0(" ", child),
        cex = text_cex * 0.9, adj = adj_value,
        facing = facing_direction, 
        niceFacing = TRUE,
        col = "#404040"
      )
    }
    
    # Add value label
    value_text <- format(round(sector_value, 1), nsmall=1)
    circos.text(
      text_pos, ylim[1] + mm_y(11) + y_offset, 
      paste0("(", value_text, ")"),
      cex = text_cex * 0.75, adj = adj_value,
      facing = facing_direction, 
      niceFacing = TRUE,
      col = "#666666"
    )
  }
)

# Add title for PNG
title(
  main = "ASF Risk Flow: Hierarchical Pathway Analysis",
  sub = "Source: Port of Entry + Conveyance Type → Target: Product Pathway + Subpathway",
  cex.main = 1.2,
  font.main = 2,
  col.main = "#333333",
  cex.sub = 0.8,
  col.sub = "#555555"
)

# Add caption for PNG
mtext(
  text = paste0("Data shows relative risk flows between hierarchical categories (n=", nrow(chord_data), ")"),
  side = 1,
  line = 0.5,
  cex = 0.8,
  col = "#666666"
)

# Close the PNG device
dev.off()

# Also save as PDF for high-quality vector graphics
pdf(pdf_filename, width = 10, height = 10)

# Recreate the chart for PDF output
circos.clear()
circos.par(
  gap.degree = gap_degree,
  track.margin = c(0.03, 0.03),
  start.degree = 90,
  clock.wise = FALSE,
  canvas.xlim = c(-0.85, 0.85),
  canvas.ylim = c(-0.85, 0.85)
)

# Create the chord diagram without directional arrows for PDF
chordDiagram(
  chord_matrix,
  grid.col = grid_colors,
  transparency = transparency,
  link.border = link_border,
  link.lwd = 0.6,
  link.sort = TRUE,
  link.largest.ontop = TRUE,
  annotationTrack = c("grid", "axis"),
  preAllocateTracks = list(track.height = 0.12),
  directional = 0,  # REMOVED DIRECTIONAL ARROWS
  annotationTrackHeight = c(0.05, 0.12),
  diffHeight = 0.05,
  reduce = 0.01
)

# Add flow value labels to PDF output
for(i in 1:nrow(chord_data)) {
  source_sector <- chord_data$source_group[i]
  target_sector <- chord_data$target_group[i]
  value <- chord_data$value[i]
  
  # Only show labels for significant flows
  if(value > max(chord_data$value) * 0.03) {
    value_text <- format(round(value, 1), nsmall=1)
    
    circos.link(
      source_sector, get.cell.meta.data("xlim", sector.index=source_sector)[1] + 0.5, 
      target_sector, get.cell.meta.data("xlim", sector.index=target_sector)[1] + 0.5,
      h = 0.7,
      col = "transparent",
      border = FALSE
    )
    
    link_coords <- circlize:::get.link.position(
      source_sector, get.cell.meta.data("xlim", sector.index=source_sector)[1] + 0.5,
      target_sector, get.cell.meta.data("xlim", sector.index=target_sector)[1] + 0.5,
      h = 0.7
    )
    
    x_mid <- mean(c(link_coords$x1, link_coords$x2))
    y_mid <- mean(c(link_coords$y1, link_coords$y2))
    
    rect_width <- strwidth(value_text, cex=0.75) * 1.2
    rect_height <- strheight(value_text, cex=0.75) * 1.5
    rect(
      x_mid - rect_width/2, y_mid - rect_height/2,
      x_mid + rect_width/2, y_mid + rect_height/2,
      col = alpha("white", 0.7),
      border = NA
    )
    
    text(
      x_mid, y_mid,
      value_text,
      cex = 0.75,
      col = "#333333",
      font = 2
    )
  }
}

# Add custom labels with values for PDF
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.name <- get.cell.meta.data("sector.index")
    parts <- strsplit(sector.name, ":")[[1]]
    parent <- parts[1]
    child <- ifelse(length(parts) > 1, parts[2], "")
    
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.index = get.cell.meta.data("sector.numeric.index")
    total.sectors = get.cell.meta.data("num.sectors")
    sector_angle <- (sector.index / total.sectors) * 360
    
    if (sector_angle > 90 && sector_angle < 270) {
      facing_direction <- "outside"
      adj_value <- c(0.5, 1)
      y_offset <- mm_y(-1)
    } else {
      facing_direction <- "outside"
      adj_value <- c(0.5, 0)
      y_offset <- mm_y(2)
    }
    
    sector_size <- xlim[2] - xlim[1]
    sector_value <- sum(chord_matrix[sector.name,], na.rm=TRUE) + sum(chord_matrix[,sector.name], na.rm=TRUE)
    rel_importance <- sector_value / max(rowSums(chord_matrix) + colSums(chord_matrix), na.rm=TRUE)
    text_cex <- min(1.0, max(0.65, sector_size/25 * (0.7 + 0.3*rel_importance)))
    text_pos <- mean(xlim)
    
    circos.text(
      text_pos, ylim[1] + mm_y(3) + y_offset, 
      parent,
      cex = text_cex, adj = adj_value,
      facing = facing_direction, 
      niceFacing = TRUE,
      font = 2
    )
    
    if (child != "") {
      circos.text(
        text_pos, ylim[1] + mm_y(7) + y_offset, 
        paste0(" ", child),
        cex = text_cex * 0.9, adj = adj_value,
        facing = facing_direction, 
        niceFacing = TRUE,
        col = "#404040"
      )
    }
    
    # Add value label
    value_text <- format(round(sector_value, 1), nsmall=1)
    circos.text(
      text_pos, ylim[1] + mm_y(11) + y_offset, 
      paste0("(", value_text, ")"),
      cex = text_cex * 0.75, adj = adj_value,
      facing = facing_direction, 
      niceFacing = TRUE,
      col = "#666666"
    )
  }
)

# Add title for PDF
title(
  main = "ASF Risk Flow: Hierarchical Pathway Analysis",
  sub = "Source: Port of Entry + Conveyance Type → Target: Product Pathway + Subpathway",
  cex.main = 1.2,
  font.main = 2,
  col.main = "#333333",
  cex.sub = 0.8,
  col.sub = "#555555"
)

# Add caption for PDF
mtext(
  text = paste0("Data shows relative risk flows between hierarchical categories (n=", nrow(chord_data), ")"),
  side = 1,
  line = 0.5,
  cex = 0.8,
  col = "#666666"
)

# Close the PDF device
dev.off()

# Print file locations
cat("Improved hierarchical chord diagram saved as:\n")
cat("1. PNG: ", png_filename, "\n")
cat("2. PDF: ", pdf_filename, "\n")
