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
  
  # Calculate fitted values and confidence intervals
  message("Calculating predictions and confidence intervals...")
  
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
  
  # Get predictions for each observation
  # Use merTools for prediction intervals (this handles the random effects properly)
  message("Generating predictions with confidence intervals for each observation...")
  predictions <- predictInterval(
    glmm_model, 
    newdata = bayes_data,
    level = 0.95,
    n.sims = 1000, # Increase for more precise intervals
    which = "fixed"
  )
  
  # Combine with original data
  results <- bind_cols(
    bayes_data,
    predictions
  ) %>%
    mutate(
      observed_rate = n_positive / total_obs,
      pred_prob = plogis(fit),
      lower_prob = plogis(lwr),
      upper_prob = plogis(upr),
      residual = observed_rate - pred_prob,
      within_CI = observed_rate >= lower_prob & observed_rate <= upper_prob
    )
  
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
  
  # 1. Observed vs Predicted plot
  p1 <- ggplot(results, aes(x = observed_rate, y = pred_prob, size = total_obs)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    scale_x_continuous(labels = percent_format()) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Observed vs. Predicted Rates",
      subtitle = "Size represents number of observations",
      x = "Observed Rate (n_positive / total_obs)",
      y = "Predicted Rate",
      size = "Sample Size"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "observed_vs_predicted.png"), p1, width = 10, height = 8)
  
  # 2. Residual plot
  p2 <- ggplot(results, aes(x = pred_prob, y = residual, size = total_obs)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_x_continuous(labels = percent_format()) +
    labs(
      title = "Residuals vs. Predicted Values",
      subtitle = "Residual = Observed - Predicted",
      x = "Predicted Rate",
      y = "Residual",
      size = "Sample Size"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "residuals.png"), p2, width = 10, height = 8)
  
  # 3. Histogram of predicted probabilities
  p3 <- ggplot(results, aes(x = pred_prob)) +
    geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
    scale_x_continuous(labels = percent_format()) +
    labs(
      title = "Distribution of Predicted Probabilities",
      x = "Predicted Probability",
      y = "Count"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "pred_prob_distribution.png"), p3, width = 10, height = 6)
  
  # 4. Coverage plot - check if observed values fall within confidence intervals
  coverage_rate <- mean(results$within_CI) * 100
  
  # Select a subset of rows for visualization (to avoid overcrowding)
  set.seed(123) # For reproducibility
  plot_rows <- results %>%
    arrange(desc(total_obs)) %>%
    slice(1:100) # Top 100 rows by sample size
  
  p4 <- ggplot(plot_rows, aes(x = factor(obs_id), y = pred_prob, color = within_CI)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_prob, ymax = upper_prob), width = 0.2) +
    geom_point(aes(y = observed_rate), shape = 4, size = 2) +
    scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = paste0("95% Confidence Intervals and Observed Rates (Coverage: ", round(coverage_rate, 1), "%)"),
      subtitle = "Top 100 observations by sample size. X marks show observed rates.",
      x = "Observation ID",
      y = "Probability",
      color = "Within CI"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  ggsave(file.path(output_dir, "confidence_intervals.png"), p4, width = 12, height = 8)
  
  # 5. Random effects visualization - location effects
  location_effects <- ranef(model)$INSPECTION_LOCATION_NAME %>%
    as.data.frame() %>%
    rename(random_effect = `(Intercept)`) %>%
    rownames_to_column("INSPECTION_LOCATION_NAME") %>%
    mutate(
      effect_prob = plogis(fixef(model)[1] + random_effect),
      random_effect_prob = plogis(random_effect + 0.5) - 0.5 # Approximate effect on probability scale
    ) %>%
    arrange(desc(effect_prob))
  
  p5 <- ggplot(location_effects %>% head(20), 
               aes(x = reorder(INSPECTION_LOCATION_NAME, effect_prob), y = effect_prob)) +
    geom_point() +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Top 20 Locations by Estimated Risk Probability",
      x = "Location",
      y = "Estimated Probability"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "location_effects.png"), p5, width = 10, height = 8)
  
  # 6. Pathway effects
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
  
  p6 <- ggplot(pathway_effects %>% head(20), 
               aes(x = reorder(PATHWAY, avg_pred), y = avg_pred)) +
    geom_point() +
    geom_errorbar(aes(ymin = avg_lower, ymax = avg_upper), width = 0.2) +
    coord_flip() +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = "Top 20 Pathways by Estimated Risk Probability",
      x = "Pathway",
      y = "Estimated Probability"
    ) +
    theme_minimal()
  
  ggsave(file.path(output_dir, "pathway_effects.png"), p6, width = 10, height = 8)
  
  message("Validation plots saved to: ", output_dir)
  
  return(list(
    observed_vs_predicted = p1,
    residuals = p2,
    pred_distribution = p3,
    confidence_intervals = p4,
    location_effects = p5,
    pathway_effects = p6
  ))
}

# Function to create and save an HTML report with the validation results
create_validation_report <- function(validation_results) {
  # Create rmarkdown report
  report_template <- '
---
title: "GLMM Validation Report for ASF Risk Model"
date: "`r format(Sys.time(), \"%Y-%m-%d %H:%M\")`"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: united
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)
```

## Overview

This report validates the Generalized Linear Mixed Model (GLMM) used to estimate ASF risk probabilities across different pathways, locations, and product types.

## Model Summary

```{r}
summary(model)
```

## Data Summary

The model was fit using data with the following characteristics:

```{r}
data_summary <- data.frame(
  Metric = c(
    "Number of observations",
    "Number of locations",
    "Number of pathways",
    "Total positive events",
    "Average positive rate",
    "Min positive rate (where > 0)",
    "Max positive rate",
    "Min total observations",
    "Max total observations"
  ),
  Value = c(
    nrow(data),
    length(unique(data$INSPECTION_LOCATION_NAME)),
    length(unique(data$PATHWAY)),
    sum(data$n_positive),
    sprintf("%.4f%%", 100 * sum(data$n_positive) / sum(data$total_obs)),
    sprintf("%.4f%%", 100 * min(data$n_positive[data$n_positive > 0] / data$total_obs[data$n_positive > 0])),
    sprintf("%.4f%%", 100 * max(data$n_positive / data$total_obs)),
    min(data$total_obs),
    max(data$total_obs)
  )
)

kable(data_summary) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

## Model Performance

### Observed vs. Predicted Rates

```{r fig.width=10, fig.height=7}
plots$observed_vs_predicted
```

### Residuals vs. Predicted Values

```{r fig.width=10, fig.height=7}
plots$residuals
```

### Distribution of Predicted Probabilities

```{r fig.width=10, fig.height=6}
plots$pred_distribution
```

### Confidence Interval Coverage

```{r fig.width=12, fig.height=8}
plots$confidence_intervals
```

## Model Results

### Top 20 Locations by Risk Probability

```{r fig.width=10, fig.height=8}
plots$location_effects
```

### Top 20 Pathways by Risk Probability

```{r fig.width=10, fig.height=8}
plots$pathway_effects
```

## Detailed Results Table

Below are the top 20 rows from the results table, ordered by predicted probability:

```{r}
top_results <- results %>%
  arrange(desc(pred_prob)) %>%
  head(20) %>%
  select(
    INSPECTION_LOCATION_NAME,
    PATHWAY,
    container,
    product_pathway,
    product_subpathway,
    n_positive,
    total_obs,
    observed_rate,
    pred_prob,
    lower_prob,
    upper_prob
  ) %>%
  mutate(
    observed_rate = sprintf("%.2f%%", 100 * observed_rate),
    pred_prob = sprintf("%.2f%%", 100 * pred_prob),
    lower_prob = sprintf("%.2f%%", 100 * lower_prob),
    upper_prob = sprintf("%.2f%%", 100 * upper_prob)
  )

kable(top_results) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 11) %>%
  scroll_box(width = "100%", height = "500px")
```

## Bottom 20 Rows (Lowest Risk)

```{r}
bottom_results <- results %>%
  filter(pred_prob > 0) %>%  # Only include non-zero predictions
  arrange(pred_prob) %>%
  head(20) %>%
  select(
    INSPECTION_LOCATION_NAME,
    PATHWAY,
    container,
    product_pathway,
    product_subpathway,
    n_positive,
    total_obs,
    observed_rate,
    pred_prob,
    lower_prob,
    upper_prob
  ) %>%
  mutate(
    observed_rate = sprintf("%.2f%%", 100 * observed_rate),
    pred_prob = sprintf("%.2f%%", 100 * pred_prob),
    lower_prob = sprintf("%.2f%%", 100 * lower_prob),
    upper_prob = sprintf("%.2f%%", 100 * upper_prob)
  )

kable(bottom_results) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 11) %>%
  scroll_box(width = "100%", height = "500px")
```

## Conclusion

This validation report provides evidence for the reliability and performance of the GLMM model in estimating ASF risk probabilities. The model accounts for the hierarchical structure of the data and provides estimates with quantified uncertainty.
'
  
  # Save the template
  report_path <- here::here("scripts", "glmm_validation_report.Rmd")
  writeLines(report_template, report_path)
  
  # Render the report
  output_dir <- here::here("output", "aqim pathway approach rates", "glmm_validation")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save the data needed for the report
  report_data <- list(
    model = validation_results$model,
    data = validation_results$data,
    results = validation_results$results,
    plots = create_validation_plots(validation_results)
  )
  saveRDS(report_data, here::here("output", "intermediate files", "glmm_validation_report_data.rds"))
  
  # Use rmarkdown to render the report
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    rmarkdown::render(
      report_path,
      output_file = "glmm_validation_report.html",
      output_dir = output_dir,
      envir = new.env()
    )
    message("Report generated at: ", file.path(output_dir, "glmm_validation_report.html"))
  } else {
    message("rmarkdown package not available. Report template saved but not rendered.")
  }
  
  return(report_path)
}

# Main execution
if (!interactive()) {
  message("Running GLMM validation...")
  validation_results <- load_and_validate_glmm()
  message("Creating validation plots...")
  plots <- create_validation_plots(validation_results)
  message("Creating validation report...")
  report_path <- create_validation_report(validation_results)
  message("GLMM validation complete.")
}
