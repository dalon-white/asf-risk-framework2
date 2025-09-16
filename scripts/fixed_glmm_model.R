# Modified GLMM Validation Script - Fix for predictInterval error
library(tidyverse)
library(lme4)
library(here)
library(merTools) # For prediction intervals from mixed models
library(scales)    # For pretty percentage formatting
library(ggplot2)

# Function to load the model and data
load_and_validate_glmm <- function() {
  # Load the bayes_data (either from a saved file or recreate it)
  message("Loading data...")
  
  # Try to load saved data if available
  data_path <- here::here("output", "intermediate files", "bayes_data.rds")
  if (file.exists(data_path)) {
    bayes_data <- readRDS(data_path)
    message("Loaded bayes_data from saved file")
  } else {
    # If data not saved, recreate it using the code from the Rmd file
    message("Recreating bayes_data from scratch...")
    
    # Load the aqim_events_summary data
    if (file.exists(here::here("output", "intermediate files", "aqim_events_summary.rds"))) {
      aqim_events_summary <- readRDS(here::here("output", "intermediate files", "aqim_events_summary.rds"))
    } else {
      stop("Could not find aqim_events_summary data. Please run the data preparation steps first.")
    }
    
    # Prepare the data using the same process as in the Rmd file
    bayes_data <- aqim_events_summary %>%
      filter(!is.na(n_positive), !is.na(total_obs), total_obs > 0) %>%
      mutate(n_positive = pmin(n_positive, total_obs)) %>%
      mutate(obs_id = row_number())
    
    # Save the data for future use
    saveRDS(bayes_data, data_path)
  }
  
  # Fit the GLMM model
  message("Fitting GLMM model...")
  glmm_model <- glmer(
    cbind(n_positive, total_obs - n_positive) ~ 1 + 
      (1 | INSPECTION_LOCATION_NAME) + 
      (1 | INSPECTION_LOCATION_NAME:PATHWAY) +
      (1 | PATHWAY:container) + 
      (1 | PATHWAY:product_pathway) +
      (1 | PATHWAY:product_pathway:product_subpathway),
    data = bayes_data,
    family = binomial("logit"),
    control = glmerControl(optimizer = "bobyqa")
  )
  
  # Summarize the model
  message("Model Summary:")
  print(summary(glmm_model))
  
  # Get fixed effect (global intercept)
  fixed_effect <- fixef(glmm_model)[1]
  message("Fixed effect (intercept): ", round(fixed_effect, 4))
  message("Overall baseline probability: ", round(plogis(fixed_effect), 4))
  
  # Extract random effects for each level
  ranef_location <- ranef(glmm_model)$INSPECTION_LOCATION_NAME
  ranef_location_pathway <- ranef(glmm_model)$`INSPECTION_LOCATION_NAME:PATHWAY`
  ranef_pathway_container <- ranef(glmm_model)$`PATHWAY:container`
  ranef_pathway_product <- ranef(glmm_model)$`PATHWAY:product_pathway`
  ranef_pathway_product_sub <- ranef(glmm_model)$`PATHWAY:product_pathway:product_subpathway`
  
  # ===== FIX #1: ALTERNATIVE APPROACH TO CONFIDENCE INTERVALS =====
  # Instead of using predictInterval, which has bugs in some situations,
  # we'll manually calculate predictions with CIs using a different approach
  
  # Method 1: Direct prediction with standard errors
  # This is more reliable but doesn't fully account for random effects uncertainty
  message("Generating predictions with confidence intervals (Method 1)...")
  
  # Get fitted values on the link scale (log-odds)
  fitted_values <- predict(glmm_model, newdata = bayes_data, type = "link")
  
  # Transform to probability scale
  fitted_probs <- plogis(fitted_values)
  
  # Calculate standard errors using the delta method for binomial data
  # This is an approximation but works well for most purposes
  se_link <- sqrt(fitted_probs * (1 - fitted_probs) / bayes_data$total_obs)
  
  # Calculate confidence intervals on probability scale
  lower_probs <- plogis(fitted_values - 1.96 * se_link)
  upper_probs <- plogis(fitted_values + 1.96 * se_link)
  
  # Create results data frame
  results <- bind_cols(
    bayes_data,
    tibble(
      pred_prob = fitted_probs,
      lower_prob = lower_probs,
      upper_prob = upper_probs,
      observed_rate = bayes_data$n_positive / bayes_data$total_obs,
      residual = observed_rate - pred_prob,
      within_CI = observed_rate >= lower_prob & observed_rate <= upper_prob
    )
  )
  
  # ===== FIX #2: ALTERNATIVE APPROACH USING BOOTSTRAP =====
  # If you want to try a more robust approach with bootstrap instead
  message("Generating bootstrap-based predictions (Method 2)...")
  
  # Function to run bootstrap simulation for each observation
  run_bootstrap <- function(model, newdata, n_boot = 1000) {
    # Extract fixed and random effects
    fe <- fixef(model)
    re_vars <- names(ranef(model))
    
    # Initialize results matrix
    boot_results <- matrix(NA, nrow = nrow(newdata), ncol = n_boot)
    
    # Run bootstrap iterations
    for (i in 1:n_boot) {
      # Simulate new fixed effect
      new_fe <- rnorm(1, fe, sqrt(vcov(model)))
      
      # Simulate new random effects for each level
      sim_ranef <- list()
      for (re_var in re_vars) {
        re_vcov <- attr(ranef(model, condVar = TRUE)[[re_var]], "postVar")
        re_means <- ranef(model)[[re_var]][,1]
        re_names <- rownames(ranef(model)[[re_var]])
        
        # Simulate from multivariate normal
        sim_values <- rnorm(length(re_means), re_means, sqrt(diag(re_vcov[1,,])))
        sim_ranef[[re_var]] <- setNames(sim_values, re_names)
      }
      
      # Compute predictions for each observation
      for (j in 1:nrow(newdata)) {
        # Start with fixed effect
        pred_j <- new_fe
        
        # Add random effects if available for this observation
        for (re_var in re_vars) {
          # Parse the random effect variable
          if (re_var == "INSPECTION_LOCATION_NAME") {
            re_val <- newdata$INSPECTION_LOCATION_NAME[j]
            if (re_val %in% names(sim_ranef[[re_var]])) {
              pred_j <- pred_j + sim_ranef[[re_var]][re_val]
            }
          } else if (re_var == "INSPECTION_LOCATION_NAME:PATHWAY") {
            re_val <- paste(newdata$INSPECTION_LOCATION_NAME[j], newdata$PATHWAY[j], sep = ":")
            if (re_val %in% names(sim_ranef[[re_var]])) {
              pred_j <- pred_j + sim_ranef[[re_var]][re_val]
            }
          } else if (re_var == "PATHWAY:container") {
            re_val <- paste(newdata$PATHWAY[j], newdata$container[j], sep = ":")
            if (re_val %in% names(sim_ranef[[re_var]])) {
              pred_j <- pred_j + sim_ranef[[re_var]][re_val]
            }
          } else if (re_var == "PATHWAY:product_pathway") {
            re_val <- paste(newdata$PATHWAY[j], newdata$product_pathway[j], sep = ":")
            if (re_val %in% names(sim_ranef[[re_var]])) {
              pred_j <- pred_j + sim_ranef[[re_var]][re_val]
            }
          } else if (re_var == "PATHWAY:product_pathway:product_subpathway") {
            re_val <- paste(newdata$PATHWAY[j], newdata$product_pathway[j], 
                           newdata$product_subpathway[j], sep = ":")
            if (re_val %in% names(sim_ranef[[re_var]])) {
              pred_j <- pred_j + sim_ranef[[re_var]][re_val]
            }
          }
        }
        
        # Store prediction on probability scale
        boot_results[j, i] <- plogis(pred_j)
      }
    }
    
    # Calculate summary statistics
    pred_mean <- rowMeans(boot_results)
    pred_lower <- apply(boot_results, 1, quantile, probs = 0.025)
    pred_upper <- apply(boot_results, 1, quantile, probs = 0.975)
    
    return(tibble(
      boot_pred = pred_mean,
      boot_lower = pred_lower, 
      boot_upper = pred_upper
    ))
  }
  
  # Run bootstrap for a smaller subset if data is large
  # This can be time-consuming for large datasets
  if (nrow(bayes_data) > 1000) {
    message("Data is large, running bootstrap on a random subset of 1000 observations...")
    set.seed(123)
    boot_subset_idx <- sample(1:nrow(bayes_data), 1000)
    boot_subset <- bayes_data[boot_subset_idx, ]
    boot_results <- run_bootstrap(glmm_model, boot_subset, n_boot = 500)
    
    # Save bootstrap results for the subset
    results$boot_pred[boot_subset_idx] <- boot_results$boot_pred
    results$boot_lower[boot_subset_idx] <- boot_results$boot_lower
    results$boot_upper[boot_subset_idx] <- boot_results$boot_upper
  } else {
    # Run bootstrap for all observations
    boot_results <- run_bootstrap(glmm_model, bayes_data, n_boot = 500)
    
    # Add bootstrap results to main results
    results <- bind_cols(results, boot_results)
  }
  
  # Calculate bootstrap coverage
  results <- results %>%
    mutate(
      boot_within_CI = observed_rate >= boot_lower & observed_rate <= boot_upper
    )
  
  # Calculate coverage percentages
  method1_coverage_pct <- mean(results$within_CI, na.rm = TRUE) * 100
  method2_coverage_pct <- mean(results$boot_within_CI, na.rm = TRUE) * 100
  
  message("Method 1 (delta method) 95% CI coverage rate: ", round(method1_coverage_pct, 1), "%")
  if ("boot_within_CI" %in% names(results)) {
    message("Method 2 (bootstrap) 95% CI coverage rate: ", round(method2_coverage_pct, 1), "%")
  }
  
  # Save the results
  results_path <- here::here("output", "intermediate files", "glmm_validation_results.rds")
  saveRDS(results, results_path)
  
  # Also save as CSV for easier examination
  write.csv(results, here::here("output", "intermediate files", "glmm_validation_results.csv"), row.names = FALSE)
  
  message("Results saved to: ", results_path)
  
  return(list(
    model = glmm_model,
    data = bayes_data,
    results = results
  ))
}

# Function to create validation visualizations
create_validation_plots <- function(validation_results) {
  results <- validation_results$results
  model <- validation_results$model
  
  # Create output directory if it doesn't exist
  output_dir <- here::here("output", "aqim pathway approach rates", "glmm_validation")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Observed vs. Predicted Plot
  obs_vs_pred_plot <- ggplot(results, aes(x = observed_rate, y = pred_prob, size = total_obs)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    scale_x_continuous(labels = scales::percent_format()) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Observed vs. Predicted Rates",
      subtitle = "Point size represents number of observations",
      x = "Observed Rate (n_positive / total_obs)",
      y = "Predicted Rate",
      size = "Sample Size"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "observed_vs_predicted.png"), obs_vs_pred_plot, width = 10, height = 8)
  
  # 2. Residual Plot
  residual_plot <- ggplot(results, aes(x = pred_prob, y = residual, size = total_obs)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
      title = "Residuals vs. Predicted Values",
      subtitle = "Residual = Observed - Predicted",
      x = "Predicted Rate",
      y = "Residual",
      size = "Sample Size"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "residuals.png"), residual_plot, width = 10, height = 8)
  
  # 3. Confidence Interval Plot
  # Select a subset of rows for visualization (to avoid overcrowding)
  set.seed(123) # For reproducibility
  plot_rows <- results %>%
    arrange(desc(total_obs)) %>%
    slice(1:50) # Top 50 rows by sample size
  
  method1_coverage_pct <- mean(results$within_CI, na.rm = TRUE) * 100
  
  ci_plot <- ggplot(plot_rows, aes(x = factor(obs_id), y = pred_prob, color = within_CI)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2) +
    geom_point(aes(y = observed_rate), shape = 4, size = 2) +
    scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = paste0("95% Confidence Intervals and Observed Rates (Coverage: ", 
                     round(method1_coverage_pct, 1), "%)"),
      subtitle = "Top 50 observations by sample size. X marks show observed rates.",
      x = "Observation ID",
      y = "Probability",
      color = "Within CI"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  ggsave(file.path(output_dir, "confidence_intervals.png"), ci_plot, width = 12, height = 8)
  
  # 4. Bootstrap CI Plot (if available)
  if ("boot_within_CI" %in% names(results)) {
    method2_coverage_pct <- mean(results$boot_within_CI, na.rm = TRUE) * 100
    
    boot_ci_plot <- ggplot(plot_rows, aes(x = factor(obs_id), y = boot_pred, color = boot_within_CI)) +
      geom_point() +
      geom_errorbar(aes(ymin = boot_lower, ymax = boot_upper), width = 0.2) +
      geom_point(aes(y = observed_rate), shape = 4, size = 2) +
      scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = paste0("Bootstrap 95% Confidence Intervals (Coverage: ", 
                       round(method2_coverage_pct, 1), "%)"),
        subtitle = "Top 50 observations by sample size. X marks show observed rates.",
        x = "Observation ID",
        y = "Probability",
        color = "Within CI"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_blank())
    
    ggsave(file.path(output_dir, "bootstrap_confidence_intervals.png"), boot_ci_plot, width = 12, height = 8)
  }
  
  # 5. Location-level effects
  # Extract fixed effect (global intercept)
  fixed_effect <- fixef(model)[1]
  
  # Extract random effects for locations
  location_effects <- ranef(model)$INSPECTION_LOCATION_NAME %>%
    as.data.frame() %>%
    rename(random_effect = `(Intercept)`) %>%
    rownames_to_column("INSPECTION_LOCATION_NAME") %>%
    mutate(
      total_effect = fixed_effect + random_effect,
      effect_prob = plogis(total_effect),
      # Get standard errors
      stderr = attr(ranef(model, condVar=TRUE)$INSPECTION_LOCATION_NAME, "postVar")[1,,] %>% sqrt(),
      # Calculate approximate confidence intervals on log-odds scale
      lower_logodds = total_effect - 1.96 * stderr,
      upper_logodds = total_effect + 1.96 * stderr,
      # Transform to probability scale
      lower_prob = plogis(lower_logodds),
      upper_prob = plogis(upper_logodds)
    ) %>%
    arrange(desc(effect_prob))
  
  # Location effects plot (top 20)
  location_plot <- ggplot(location_effects %>% head(20), 
                          aes(x = reorder(INSPECTION_LOCATION_NAME, effect_prob), y = effect_prob)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Top 20 Locations by Estimated Risk Probability",
      subtitle = "Error bars represent 95% confidence intervals",
      x = "Location",
      y = "Estimated Probability"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "location_effects.png"), location_plot, width = 12, height = 10)
  
  # Save location effects as CSV
  write.csv(location_effects, file.path(output_dir, "location_effects.csv"), row.names = FALSE)
  
  # 6. Pathway-level effects
  # Aggregate results by pathway
  pathway_effects <- results %>%
    group_by(PATHWAY) %>%
    summarize(
      avg_pred = mean(pred_prob),
      avg_lower = mean(lower_prob),
      avg_upper = mean(upper_prob),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(avg_pred))
  
  # Pathway effects plot
  pathway_plot <- ggplot(pathway_effects %>% head(20), 
                         aes(x = reorder(PATHWAY, avg_pred), y = avg_pred)) +
    geom_point() +
    geom_errorbar(aes(ymin = avg_lower, ymax = avg_upper), width = 0.2) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Top 20 Pathways by Estimated Risk Probability",
      subtitle = "Error bars represent 95% confidence intervals",
      x = "Pathway",
      y = "Estimated Probability"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "pathway_effects.png"), pathway_plot, width = 12, height = 10)
  
  # Save pathway effects as CSV
  write.csv(pathway_effects, file.path(output_dir, "pathway_effects.csv"), row.names = FALSE)
  
  message("Validation plots saved to: ", output_dir)
  
  return(list(
    location_effects = location_effects,
    pathway_effects = pathway_effects
  ))
}

# Generate the final output files in the format expected by the ASF Risk Framework
generate_output_files <- function(validation_results) {
  results <- validation_results$results
  model <- validation_results$model
  
  # Create output directory if it doesn't exist
  output_dir <- here::here("output", "aqim pathway approach rates")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 1. Prepare location-level results
  location_results <- results %>%
    group_by(INSPECTION_LOCATION_NAME) %>%
    summarize(
      total_obs = sum(total_obs),
      n_positive = sum(n_positive),
      pred_prob = mean(pred_prob),
      lower_prob = mean(lower_prob),
      upper_prob = mean(upper_prob),
      .groups = "drop"
    ) %>%
    arrange(desc(pred_prob))
  
  # 2. Prepare pathway-level results
  pathway_results <- results %>%
    group_by(PATHWAY) %>%
    summarize(
      total_obs = sum(total_obs),
      n_positive = sum(n_positive),
      pred_prob = mean(pred_prob),
      lower_prob = mean(lower_prob),
      upper_prob = mean(upper_prob),
      .groups = "drop"
    ) %>%
    arrange(desc(pred_prob))
  
  # 3. Prepare container-level results
  container_results <- results %>%
    group_by(PATHWAY, container) %>%
    summarize(
      total_obs = sum(total_obs),
      n_positive = sum(n_positive),
      pred_prob = mean(pred_prob),
      lower_prob = mean(lower_prob),
      upper_prob = mean(upper_prob),
      .groups = "drop"
    ) %>%
    arrange(PATHWAY, desc(pred_prob))
  
  # 4. Prepare product-pathway-level results
  product_pathway_results <- results %>%
    group_by(PATHWAY, product_pathway) %>%
    summarize(
      total_obs = sum(total_obs),
      n_positive = sum(n_positive),
      pred_prob = mean(pred_prob),
      lower_prob = mean(lower_prob),
      upper_prob = mean(upper_prob),
      .groups = "drop"
    ) %>%
    arrange(PATHWAY, desc(pred_prob))
  
  # 5. Prepare product-subpathway-level results
  product_subpathway_results <- results %>%
    group_by(PATHWAY, product_pathway, product_subpathway) %>%
    summarize(
      total_obs = sum(total_obs),
      n_positive = sum(n_positive),
      pred_prob = mean(pred_prob),
      lower_prob = mean(lower_prob),
      upper_prob = mean(upper_prob),
      .groups = "drop"
    ) %>%
    arrange(PATHWAY, product_pathway, desc(pred_prob))
  
  # Save results in the format expected by the ASF Risk Framework
  # Main GLMM results file
  glmm_results <- bind_rows(
    location_results %>% mutate(level = "location"),
    pathway_results %>% mutate(level = "pathway"),
    container_results %>% mutate(level = "container"),
    product_pathway_results %>% mutate(level = "product_pathway"),
    product_subpathway_results %>% mutate(level = "product_subpathway")
  )
  
  # Save results
  write.csv(glmm_results, file.path(output_dir, "glmm_results_aqim_fraction_at_risk.csv"), row.names = FALSE)
  saveRDS(glmm_results, file.path(output_dir, "glmm_results_aqim_fraction_at_risk.rds"))
  
  message("Output files generated and saved to: ", output_dir)
  
  return(glmm_results)
}

# Main execution function
run_glmm_validation <- function() {
  # Step 1: Load data and fit GLMM model
  validation_results <- load_and_validate_glmm()
  
  # Step 2: Create validation plots
  effects <- create_validation_plots(validation_results)
  
  # Step 3: Generate output files
  output_results <- generate_output_files(validation_results)
  
  return(list(
    validation_results = validation_results,
    effects = effects,
    output_results = output_results
  ))
}

# Run the validation if this script is executed directly
if (!interactive()) {
  run_glmm_validation()
}
