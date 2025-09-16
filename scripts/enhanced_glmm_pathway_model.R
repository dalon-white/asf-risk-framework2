# Enhanced Split-and-Model GLMM Approach for ASF Risk Prediction
# This script splits data by PATHWAY and fits separate GLMMs for each
# with improved diagnostics and multiple probability calculation methods

library(tidyverse)
library(lme4)
library(here)
library(scales)    # For pretty percentage formatting
library(ggplot2)
library(broom.mixed) # For model diagnostics
library(DHARMa)    # For residual diagnostics
library(car)       # For model diagnostics
library(boot)      # For bootstrapping

# Function to fit GLMM for a single pathway
fit_pathway_glmm <- function(pathway_data, pathway_name, 
                            method = c("simple", "bootstrap", "profile"),
                            bootstrap_reps = 500,
                            fallback = TRUE) {
  # Default to simple method if not specified
  method <- match.arg(method)
  
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
  model_result <- tryCatch({
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
    
    # Check for convergence warnings
    conv_warnings <- warnings()
    if (length(conv_warnings) > 0) {
      warning_text <- paste(as.character(conv_warnings), collapse = "; ")
      if (grepl("convergence|singular|scaled gradient", warning_text)) {
        cat("CONVERGENCE WARNING:", warning_text, "\n")
        
        if (fallback) {
          cat("Trying simplified model...\n")
          # Try a simpler model with fewer random effects
          model <- glmer(
            cbind(n_positive, total_obs - n_positive) ~ 1 + 
              (1 | INSPECTION_LOCATION_NAME),
            data = pathway_data,
            family = binomial("logit"),
            control = glmerControl(optimizer = "bobyqa")
          )
          cat("Simplified model fitted successfully\n")
        }
      }
    }
    
    # Get fixed effect (global intercept)
    fixed_effect <- fixef(model)[1]
    cat("Fixed effect (intercept):", round(fixed_effect, 4), "\n")
    cat("Overall baseline probability:", round(plogis(fixed_effect), 4), "\n")
    
    # Calculate predicted probabilities based on the selected method
    if (method == "simple") {
      # Method 1: Simple fitted values
      fitted_values <- predict(model, newdata = pathway_data, type = "link")
      fitted_probs <- plogis(fitted_values)
      
      # Calculate standard errors using the delta method
      se_link <- sqrt(fitted_probs * (1 - fitted_probs) / pathway_data$total_obs)
      
      # Calculate confidence intervals
      lower_probs <- plogis(fitted_values - 1.96 * se_link)
      upper_probs <- plogis(fitted_values + 1.96 * se_link)
      
    } else if (method == "bootstrap") {
      # Method 2: Parametric bootstrap
      cat("Running parametric bootstrap with", bootstrap_reps, "replications...\n")
      
      # Function to simulate from the fitted model
      boot_func <- function(model, newdata) {
        # Get fixed effects and variance components
        fixef_val <- fixef(model)
        ranef_vals <- ranef(model)
        
        # Simulate random effects
        new_ranef <- list()
        for (re in names(ranef_vals)) {
          # Get variance-covariance matrix for this random effect
          vc <- VarCorr(model)[[re]]
          sd_val <- attr(vc, "stddev")
          
          # Get levels for this random effect
          levels <- rownames(ranef_vals[[re]])
          
          # Simulate new random effects
          new_ranef[[re]] <- rnorm(length(levels), 0, sd_val)
          names(new_ranef[[re]]) <- levels
        }
        
        # Calculate linear predictor for each observation
        lp <- fixef_val[1]  # Intercept
        
        # Add random effects
        for (i in 1:nrow(newdata)) {
          for (re in names(ranef_vals)) {
            # Extract the level of this random effect for this observation
            level <- as.character(newdata[[re]][i])
            if (level %in% names(new_ranef[[re]])) {
              lp[i] <- lp[i] + new_ranef[[re]][level]
            }
          }
        }
        
        # Convert to probability
        probs <- plogis(lp)
        
        return(probs)
      }
      
      # Run bootstrap
      boot_results <- replicate(bootstrap_reps, boot_func(model, pathway_data))
      
      # Calculate point estimates and confidence intervals
      fitted_probs <- rowMeans(boot_results)
      lower_probs <- apply(boot_results, 1, quantile, probs = 0.025)
      upper_probs <- apply(boot_results, 1, quantile, probs = 0.975)
      
    } else if (method == "profile") {
      # Method 3: Profile likelihood (most accurate but slowest)
      cat("Using profile likelihood for confidence intervals...\n")
      
      # Get predictions on link scale
      fitted_values <- predict(model, newdata = pathway_data, type = "link", re.form = NULL)
      fitted_probs <- plogis(fitted_values)
      
      # We need to calculate profile CIs for each observation, but this is very slow
      # So we'll just use the fixed effect's profile CI as a proxy and adjust
      
      # Get profile CI for fixed effect
      prof <- profile(model, which = "beta_")
      ci_fixed <- confint(prof, level = 0.95)
      
      # Calculate width of CI on link scale
      ci_width <- ci_fixed[1, 2] - ci_fixed[1, 1]
      
      # Apply same width to all predictions (simplified approach)
      lower_probs <- plogis(fitted_values - ci_width/2)
      upper_probs <- plogis(fitted_values + ci_width/2)
    }
    
    # Calculate model diagnostics
    model_summary <- summary(model)
    AIC_val <- AIC(model)
    BIC_val <- BIC(model)
    
    # Get random effects variances
    var_comps <- as.data.frame(VarCorr(model))
    var_summary <- var_comps %>%
      select(grp, vcov) %>%
      rename(Random_Effect = grp, Variance = vcov)
    
    # Create diagnostics dataframe
    diagnostics <- data.frame(
      pathway = pathway_name,
      AIC = AIC_val,
      BIC = BIC_val,
      n_obs = nrow(pathway_data),
      n_positive = sum(pathway_data$n_positive),
      positive_rate = sum(pathway_data$n_positive) / sum(pathway_data$total_obs),
      intercept = fixed_effect,
      baseline_prob = plogis(fixed_effect),
      method = method
    )
    
    # Create results data frame
    results <- pathway_data %>%
      mutate(
        pathway = pathway_name,
        pred_prob = fitted_probs,
        lower_prob = lower_probs,
        upper_prob = upper_probs,
        observed_rate = n_positive / total_obs,
        method = method
      )
    
    cat("Model successfully fit for pathway:", pathway_name, "\n")
    return(list(
      model = model, 
      results = results, 
      diagnostics = diagnostics,
      var_components = var_summary
    ))
    
  }, error = function(e) {
    cat("ERROR fitting model for pathway", pathway_name, ":", conditionMessage(e), "\n")
    
    if (fallback) {
      cat("Trying simplified binomial model without random effects...\n")
      
      # Try a simple binomial GLM as fallback
      tryCatch({
        # Fit a simple binomial model
        simple_model <- glm(
          cbind(n_positive, total_obs - n_positive) ~ 1,
          data = pathway_data,
          family = binomial("logit")
        )
        
        # Get predictions
        fitted_probs <- predict(simple_model, type = "response")
        ci <- confint(simple_model)
        lower_probs <- plogis(qlogis(fitted_probs) + ci[1] - coef(simple_model)[1])
        upper_probs <- plogis(qlogis(fitted_probs) + ci[2] - coef(simple_model)[1])
        
        # Create results data frame
        results <- pathway_data %>%
          mutate(
            pathway = pathway_name,
            pred_prob = fitted_probs,
            lower_prob = lower_probs,
            upper_prob = upper_probs,
            observed_rate = n_positive / total_obs,
            method = "fallback_glm"
          )
        
        # Create diagnostics dataframe
        diagnostics <- data.frame(
          pathway = pathway_name,
          AIC = AIC(simple_model),
          BIC = BIC(simple_model),
          n_obs = nrow(pathway_data),
          n_positive = sum(pathway_data$n_positive),
          positive_rate = sum(pathway_data$n_positive) / sum(pathway_data$total_obs),
          intercept = coef(simple_model)[1],
          baseline_prob = plogis(coef(simple_model)[1]),
          method = "fallback_glm"
        )
        
        cat("Fallback model successfully fit for pathway:", pathway_name, "\n")
        return(list(
          model = simple_model, 
          results = results, 
          diagnostics = diagnostics,
          var_components = data.frame(Random_Effect = "None", Variance = 0)
        ))
        
      }, error = function(e2) {
        cat("ERROR fitting fallback model:", conditionMessage(e2), "\n")
        return(NULL)
      })
    } else {
      return(NULL)
    }
  })
  
  return(model_result)
}

# Main function to process all pathways
process_all_pathways <- function(bayes_data, 
                                method = c("simple", "bootstrap", "profile"),
                                bootstrap_reps = 500,
                                fallback = TRUE) {
  # Default to simple method if not specified
  method <- match.arg(method)
  
  # Split data by PATHWAY
  pathways <- unique(bayes_data$PATHWAY)
  cat("Found", length(pathways), "unique pathways to process\n")
  
  # Initialize lists to store results
  all_models <- list()
  all_results <- list()
  all_diagnostics <- list()
  all_var_components <- list()
  
  # Process each pathway
  for (pathway in pathways) {
    # Extract data for this pathway
    pathway_data <- bayes_data %>% filter(PATHWAY == pathway)
    
    # Fit model for this pathway
    result <- fit_pathway_glmm(
      pathway_data = pathway_data, 
      pathway_name = pathway, 
      method = method,
      bootstrap_reps = bootstrap_reps,
      fallback = fallback
    )
    
    # Store results if successful
    if (!is.null(result)) {
      all_models[[pathway]] <- result$model
      all_results[[pathway]] <- result$results
      all_diagnostics[[pathway]] <- result$diagnostics
      all_var_components[[pathway]] <- result$var_components
    }
  }
  
  # Combine all results
  combined_results <- bind_rows(all_results)
  combined_diagnostics <- bind_rows(all_diagnostics)
  combined_var_components <- bind_rows(
    lapply(names(all_var_components), function(p) {
      cbind(pathway = p, all_var_components[[p]])
    })
  )
  
  # Create output directory if it doesn't exist
  output_dir <- here::here("output", "aqim pathway approach rates")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create method-specific suffix
  method_suffix <- switch(method,
                          simple = "",
                          bootstrap = "_bootstrap",
                          profile = "_profile")
  
  # Save the combined results
  results_file <- file.path(output_dir, paste0("glmm_results_aqim_fraction_at_risk", method_suffix, ".csv"))
  write.csv(combined_results, results_file, row.names = FALSE)
  saveRDS(combined_results, file.path(output_dir, paste0("glmm_results_aqim_fraction_at_risk", method_suffix, ".rds")))
  
  # Save the diagnostics
  diag_file <- file.path(output_dir, paste0("glmm_diagnostics_by_pathway", method_suffix, ".csv"))
  write.csv(combined_diagnostics, diag_file, row.names = FALSE)
  
  # Save the variance components
  var_file <- file.path(output_dir, paste0("glmm_variance_components", method_suffix, ".csv"))
  write.csv(combined_var_components, var_file, row.names = FALSE)
  
  # Save the models
  models_file <- file.path(output_dir, paste0("glmm_models_by_pathway", method_suffix, ".rds"))
  saveRDS(all_models, models_file)
  
  cat("\nResults saved to:", results_file, "\n")
  cat("Diagnostics saved to:", diag_file, "\n")
  cat("Variance components saved to:", var_file, "\n")
  cat("Models saved to:", models_file, "\n")
  
  return(list(
    models = all_models,
    results = combined_results,
    diagnostics = combined_diagnostics,
    var_components = combined_var_components
  ))
}

# Function to create visualization of results
visualize_results <- function(results, diagnostics = NULL) {
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
  
  # 4. Model diagnostics plot (if available)
  if (!is.null(diagnostics)) {
    # Plot model statistics by pathway
    diag_plot <- ggplot(diagnostics, aes(x = reorder(pathway, baseline_prob), y = baseline_prob)) +
      geom_point(aes(size = n_obs, color = method)) +
      coord_flip() +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = "Model Diagnostics by Pathway",
        subtitle = "Baseline probability with sample size and method used",
        x = "Pathway",
        y = "Baseline Probability",
        size = "Sample Size",
        color = "Method"
      ) +
      theme_minimal()
    
    ggsave(file.path(output_dir, "model_diagnostics_by_pathway.png"), diag_plot, width = 12, height = 10)
  }
  
  cat("\nVisualization plots saved to:", output_dir, "\n")
  
  return(list(
    pathway_summary = pathway_summary,
    location_summary = location_summary
  ))
}

# Function to compare GLMM results with the Bayesian model results
compare_with_bayesian <- function(glmm_results, bayesian_file) {
  # Check if Bayesian results exist
  if (!file.exists(bayesian_file)) {
    cat("Bayesian results file not found:", bayesian_file, "\n")
    return(NULL)
  }
  
  # Load Bayesian results
  bayes_results <- readRDS(bayesian_file)
  
  # Merge the results
  if ("pathway" %in% names(glmm_results) && 
      "pathway" %in% names(bayes_results) && 
      "pred_prob" %in% names(glmm_results) && 
      "pred_prob" %in% names(bayes_results)) {
    
    # Prepare Bayesian results
    bayes_summary <- bayes_results %>%
      group_by(pathway) %>%
      summarize(
        bayes_prob = mean(pred_prob, na.rm = TRUE),
        bayes_lower = mean(lower_prob, na.rm = TRUE),
        bayes_upper = mean(upper_prob, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Prepare GLMM results
    glmm_summary <- glmm_results %>%
      group_by(pathway) %>%
      summarize(
        glmm_prob = mean(pred_prob, na.rm = TRUE),
        glmm_lower = mean(lower_prob, na.rm = TRUE),
        glmm_upper = mean(upper_prob, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Merge the results
    comparison <- full_join(glmm_summary, bayes_summary, by = "pathway") %>%
      mutate(
        diff = glmm_prob - bayes_prob,
        relative_diff = diff / bayes_prob,
        overlap = pmin(glmm_upper, bayes_upper) - pmax(glmm_lower, bayes_lower),
        has_overlap = overlap > 0
      )
    
    # Create output directory if it doesn't exist
    output_dir <- here::here("output", "aqim pathway approach rates", "model_comparison")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Save comparison
    comparison_file <- file.path(output_dir, "glmm_vs_bayesian_comparison.csv")
    write.csv(comparison, comparison_file, row.names = FALSE)
    
    # Create comparison plot
    comparison_plot <- ggplot(comparison, 
                             aes(x = bayes_prob, y = glmm_prob, size = abs(diff))) +
      geom_point(aes(color = has_overlap)) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      scale_x_continuous(labels = percent_format()) +
      scale_y_continuous(labels = percent_format()) +
      scale_color_manual(values = c("red", "blue")) +
      labs(
        title = "GLMM vs. Bayesian Probability Estimates",
        subtitle = "Point size represents absolute difference, color indicates CI overlap",
        x = "Bayesian Estimate",
        y = "GLMM Estimate",
        size = "Absolute Difference",
        color = "CIs Overlap"
      ) +
      theme_minimal()
    
    ggsave(file.path(output_dir, "glmm_vs_bayesian_comparison.png"), comparison_plot, width = 10, height = 8)
    
    cat("\nComparison saved to:", comparison_file, "\n")
    return(comparison)
  } else {
    cat("Error: Results data frames don't have the expected structure\n")
    return(NULL)
  }
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

# Run the analysis (you can change the method as needed)
results <- process_all_pathways(bayes_data, method = "simple", fallback = TRUE)

# Create visualizations
summaries <- visualize_results(results$results, results$diagnostics)

# Compare with Bayesian results if available
bayesian_file <- here::here("output", "aqim pathway approach rates", "bootstrap_results_aqim_fraction_at_risk.rds")
if (file.exists(bayesian_file)) {
  comparison <- compare_with_bayesian(results$results, bayesian_file)
}

# Print a summary of results
cat("\n==========================================================\n")
cat("Analysis completed successfully\n")
cat("Processed", length(results$models), "pathways\n")
cat("Total observations in results:", nrow(results$results), "\n")
cat("==========================================================\n")
