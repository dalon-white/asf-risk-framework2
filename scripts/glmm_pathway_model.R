# Split-and-Model GLMM Approach for ASF Risk Prediction
# This script splits data by PATHWAY and fits separate GLMMs for each

library(tidyverse)
library(lme4)
library(here)
library(scales)    # For pretty percentage formatting
library(ggplot2)

# Function to fit GLMM for a single pathway
fit_pathway_glmm <- function(pathway_data, pathway_name) {
  # Print info about this pathway
  cat("\n--------------------------------------------------------\n")
  cat("Processing pathway:", pathway_name, "\n")
  cat("Number of observations:", nrow(pathway_data), "\n")
  cat("Number of locations:", length(unique(pathway_data$INSPECTION_LOCATION_NAME)), "\n")
  cat("Total positive events:", sum(pathway_data$n_positive), "\n")
  cat("Average positive rate:", 
      round(100 * sum(pathway_data$n_positive) / sum(pathway_data$total_obs), 4), "%\n")
  
  # Skip pathways with insufficient data
  if (nrow(pathway_data) < 5) {
    cat("WARNING: Too few observations (< 5) for pathway", pathway_name, "- skipping\n")
    return(NULL)
  }
  
  if (sum(pathway_data$n_positive) == 0) {
    cat("WARNING: No positive events for pathway", pathway_name, "- skipping\n")
    return(NULL)
  }
  
  # Try to fit the model with error handling
  tryCatch({
    # Fit the GLMM model
    model <- glmer(
      cbind(n_positive, total_obs - n_positive) ~ 1 + 
        (1 | INSPECTION_LOCATION_NAME) + 
        (1 | container) + 
        (1 | product_pathway) +
        (1 | product_pathway:product_subpathway),
      data = pathway_data,
      family = binomial("logit"),
      control = glmerControl(optimizer = "bobyqa")
    )
    
    # Get fixed effect (global intercept)
    fixed_effect <- fixef(model)[1]
    cat("Fixed effect (intercept):", round(fixed_effect, 4), "\n")
    cat("Overall baseline probability:", round(plogis(fixed_effect), 4), "\n")
    
    # Get predicted probabilities for each observation
    # Method 1: Simple fitted values
    fitted_values <- predict(model, newdata = pathway_data, type = "link")
    fitted_probs <- plogis(fitted_values)
    
    # Calculate standard errors using the delta method
    se_link <- sqrt(fitted_probs * (1 - fitted_probs) / pathway_data$total_obs)
    
    # Calculate confidence intervals
    lower_probs <- plogis(fitted_values - 1.96 * se_link)
    upper_probs <- plogis(fitted_values + 1.96 * se_link)
    
    # Create results data frame
    results <- pathway_data %>%
      mutate(
        pathway = pathway_name,
        pred_prob = fitted_probs,
        lower_prob = lower_probs,
        upper_prob = upper_probs,
        observed_rate = n_positive / total_obs
      )
    
    cat("Model successfully fit for pathway:", pathway_name, "\n")
    return(list(model = model, results = results))
    
  }, error = function(e) {
    cat("ERROR fitting model for pathway", pathway_name, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Main function to process all pathways
process_all_pathways <- function(bayes_data) {
  # Split data by PATHWAY
  pathways <- unique(bayes_data$PATHWAY)
  cat("Found", length(pathways), "unique pathways to process\n")
  
  # Initialize lists to store results
  all_models <- list()
  all_results <- list()
  
  # Process each pathway
  for (pathway in pathways) {
    # Extract data for this pathway
    pathway_data <- bayes_data %>% filter(PATHWAY == pathway)
    
    # Fit model for this pathway
    result <- fit_pathway_glmm(pathway_data, pathway)
    
    # Store results if successful
    if (!is.null(result)) {
      all_models[[pathway]] <- result$model
      all_results[[pathway]] <- result$results
    }
  }
  
  # Combine all results
  combined_results <- bind_rows(all_results)
  
  # Create output directory if it doesn't exist
  output_dir <- here::here("output", "aqim pathway approach rates")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save the combined results
  results_file <- file.path(output_dir, "glmm_results_aqim_fraction_at_risk.csv")
  write.csv(combined_results, results_file, row.names = FALSE)
  saveRDS(combined_results, file.path(output_dir, "glmm_results_aqim_fraction_at_risk.rds"))
  
  # Save the models
  models_file <- file.path(output_dir, "glmm_models_by_pathway.rds")
  saveRDS(all_models, models_file)
  
  cat("\nResults saved to:", results_file, "\n")
  cat("Models saved to:", models_file, "\n")
  
  return(list(
    models = all_models,
    results = combined_results
  ))
}

# Function to create visualization of results
visualize_results <- function(results) {
  # Create output directory if it doesn't exist
  output_dir <- here::here("output", "aqim pathway approach rates", "glmm_validation")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Observed vs. Predicted Plot
  obs_vs_pred_plot <- ggplot(results, aes(x = observed_rate, y = pred_prob, size = total_obs, color = pathway)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    scale_x_continuous(labels = percent_format()) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Observed vs. Predicted Rates by Pathway",
      subtitle = "Point size represents number of observations",
      x = "Observed Rate (n_positive / total_obs)",
      y = "Predicted Rate",
      size = "Sample Size"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(output_dir, "observed_vs_predicted_by_pathway.png"), obs_vs_pred_plot, width = 10, height = 8)
  
  # 2. Pathway-level summary
  pathway_summary <- results %>%
    group_by(pathway) %>%
    summarize(
      total_obs = sum(total_obs),
      n_positive = sum(n_positive),
      observed_rate = n_positive / total_obs,
      pred_prob = mean(pred_prob),
      lower_prob = mean(lower_prob),
      upper_prob = mean(upper_prob),
      .groups = "drop"
    ) %>%
    arrange(desc(pred_prob))
  
  # Pathway predictions plot
  pathway_plot <- ggplot(pathway_summary, 
                       aes(x = reorder(pathway, pred_prob), y = pred_prob)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2) +
    geom_point(aes(y = observed_rate), shape = 4, size = 2) +  # Add observed rates
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Pathway Risk Probabilities",
      subtitle = "Error bars represent 95% confidence intervals. X marks show observed rates.",
      x = "Pathway",
      y = "Estimated Probability"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "pathway_risk_probabilities.png"), pathway_plot, width = 12, height = 10)
  
  # 3. Location-level summary
  location_summary <- results %>%
    group_by(INSPECTION_LOCATION_NAME) %>%
    summarize(
      total_obs = sum(total_obs),
      n_positive = sum(n_positive),
      observed_rate = n_positive / total_obs,
      pred_prob = mean(pred_prob),
      lower_prob = mean(lower_prob),
      upper_prob = mean(upper_prob),
      .groups = "drop"
    ) %>%
    arrange(desc(pred_prob))
  
  # Location predictions plot (top 30)
  location_plot <- ggplot(head(location_summary, 30), 
                        aes(x = reorder(INSPECTION_LOCATION_NAME, pred_prob), y = pred_prob)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2) +
    geom_point(aes(y = observed_rate), shape = 4, size = 2) +  # Add observed rates
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Top 30 Locations by Risk Probability",
      subtitle = "Error bars represent 95% confidence intervals. X marks show observed rates.",
      x = "Location",
      y = "Estimated Probability"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "location_risk_probabilities.png"), location_plot, width = 12, height = 10)
  
  cat("\nVisualization plots saved to:", output_dir, "\n")
  
  return(list(
    pathway_summary = pathway_summary,
    location_summary = location_summary
  ))
}

# Execute the entire workflow if this script is run directly
if (!exists("bayes_data")) {
  # Load the data if not already loaded
  if (file.exists(here::here("output", "intermediate files", "aqim_events_summary.rds"))) {
    aqim_events_summary <- readRDS(here::here("output", "intermediate files", "aqim_events_summary.rds"))
    
    # Prepare the data
    bayes_data <- aqim_events_summary %>%
      filter(!is.na(n_positive), !is.na(total_obs), total_obs > 0) %>%
      mutate(n_positive = pmin(n_positive, total_obs)) %>%
      mutate(obs_id = row_number())
  } else {
    stop("Could not find aqim_events_summary.rds. Run the data preparation steps first.")
  }
}

# Run the analysis
results <- process_all_pathways(bayes_data)

# Create visualizations
summaries <- visualize_results(results$results)

# Print a summary of results
cat("\n==========================================================\n")
cat("Analysis completed successfully\n")
cat("Processed", length(results$models), "pathways\n")
cat("Total observations in results:", nrow(results$results), "\n")
cat("==========================================================\n")
