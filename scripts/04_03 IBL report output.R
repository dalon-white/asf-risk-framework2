# filepath: c:\Users\Dalon.White\OneDrive - USDA\Desktop\Projects\asf-risk-framework2\asf-risk-framework2\scripts\04_03 IBL report output.R

# Load necessary libraries
library(tidyverse)
library(cowplot)  # For combining plots
library(ggthemes)  # For nice themes
library(scales)    # For better scale formatting
library(here)      # For file path handling
library(lubridate) # For date handling

# Set up directories
output_dir <- file.path(here::here(), "output", "IBLs")
report_dir <- file.path(here::here(), "output", "report files")

# Create report directory if it doesn't exist
if (!dir.exists(report_dir)) {
  dir.create(report_dir, recursive = TRUE)
}

# Load parameters for the years (similar to what's used in 04_02)
source(file.path(here::here(), "scripts", "initialize_params.R"))

# Load the IBL data
ibl_file <- file.path(output_dir, paste0(min(params$years), "_to_", max(params$years),"_ibl_data.csv"))
ibl_data <- read.csv(ibl_file)

# Convert date to proper format
ibl_data$start_date <- as.Date(ibl_data$start_date)

# Filter for Florida and Puerto Rico data
fl_pr_data <- ibl_data %>%
  filter(owner %in% c("Florida", "Puerto Rico")) %>%
  mutate(
    # Format the owner for display
    owner = factor(owner, levels = c("Florida", "Puerto Rico")),
    # Create year-month for time series
    year_month = floor_date(start_date, "month")
  )

# Check if we have data for both regions
regions_present <- unique(fl_pr_data$owner)
cat("Regions present in the data:", paste(regions_present, collapse = ", "), "\n")

# ===== PLOT 1: Number of IBL Events by Region =====
event_counts <- fl_pr_data %>%
  count(owner) %>%
  rename(Events = n)

plot_events <- ggplot(event_counts, aes(x = owner, y = Events, fill = owner)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Events), position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Florida" = "#1f78b4", "Puerto Rico" = "#33a02c")) +
  labs(
    title = paste0("Number of Illegal Boat Landing Events (", min(params$years), "-", max(params$years), ")"),
    x = NULL,
    y = "Number of Events"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12)
  )

# ===== PLOT 2: Animal KG Distribution by Region =====
# Remove rows with NA Animal.kg for the violin plot
animal_kg_data <- fl_pr_data

plot_kg <- ggplot(animal_kg_data, aes(x = Animal.kg, y = owner, fill = owner)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.1, alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("Florida" = "#1f78b4", "Puerto Rico" = "#33a02c")) +
  labs(
    title = paste0("Distribution of Seized Animal Products (kg) (", min(params$years), "-", max(params$years), ")"),
    x = "Animal Products (kg)",
    y = NULL
  ) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12)
  )

# ===== COMBINE PLOTS =====
combined_plot <- plot_grid(
  plot_events, 
  plot_kg, 
  labels = c("A", "B"),
  ncol = 1,
  align = "v",
  rel_heights = c(1, 1.2)
)

# Add an overall title
title <- ggdraw() + 
  draw_label(
    paste0("Illegal Boat Landing: FL and PR (", min(params$years), "-", max(params$years), ")"),
    fontface = "bold",
    size = 16,
    x = 0.5,
    hjust = 0.5
  ) +
  theme(plot.margin = margin(0, 0, 10, 0))

final_plot <- plot_grid(
  title, 
  combined_plot, 
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# Save the combined plot
timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
plot_file <- file.path(report_dir, paste0("IBL_Florida_vs_PuertoRico_", timestamp, ".png"))
ggsave(plot_file, final_plot, width = 6, height = 8, dpi = 300)

# Also display the plot
print(final_plot)

# Print summary statistics for reporting
cat("\n===== SUMMARY STATISTICS =====\n")
cat("Total IBL Events:", nrow(fl_pr_data), "\n")

summary_stats <- fl_pr_data %>%
  group_by(owner) %>%
  summarize(
    Events = n(),
    `Animal Products (kg)` = sum(Animal.kg, na.rm = TRUE),
    `Min Animal (kg)` = min(Animal.kg, na.rm = TRUE),
    `Max Animal (kg)` = max(Animal.kg, na.rm = TRUE),
    `Median Animal (kg)` = median(Animal.kg, na.rm = TRUE),
    `Mean Animal (kg)` = mean(Animal.kg, na.rm = TRUE),
    # Count valid animal seizures (not NA, not 0, not NaN, not Inf)
    `Events with Valid Animal Seizures` = sum(!is.na(Animal.kg) & Animal.kg > 0 & is.finite(Animal.kg)),
    # Count valid plant seizures (not NA, not 0, not NaN, not Inf)
    `Events with Valid Plant Seizures` = sum(!is.na(Plant.kg) & Plant.kg > 0 & is.finite(Plant.kg)),
    # Count problematic plant seizures (0, NaN, or Inf, but not NA)
  )

print(summary_stats)

cat("\nVisualization saved to:", plot_file, "\n")

# save summary stats to csv in the report files
# save summary statistics
summary_file <- file.path(report_dir, paste0("IBL_Florida_vs_PuertoRico_", timestamp, "_summary.csv"))
write.csv(summary_stats, summary_file, row.names = FALSE)   
