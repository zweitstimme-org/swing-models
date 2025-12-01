
# ----------------------------------------------------------
# District-Level Swing Model Project
# Authors: Lukas Stoetzer & Cornelius Erfort
# ----------------------------------------------------------

# ========== Load Required Libraries ==========
# Suppress package startup messages for cleaner output
suppressPackageStartupMessages({
  library(tidyverse)   # Data manipulation and visualization
  library(lubridate)   # Date and time handling
  library(dlm)         # Dynamic linear models
  library(xml2)        # XML parsing
  library(xtable)      # Export tables to LaTeX
  library(ggplot2)     # Advanced plotting
  # ... add others as needed
})

# ========== Load Data ==========
# Load candidate data for all federal elections 1983-2025
load("data/btw_candidates_1983-2025.RData")

# ========== Data Cleaning ==========
# Ensure binary variables are strictly 0 or 1
btw_candidates_1983_2025$formercand[btw_candidates_1983_2025$formercand > 0 & btw_candidates_1983_2025$formercand < 1] <- 0
btw_candidates_1983_2025$female[btw_candidates_1983_2025$female > 0 & btw_candidates_1983_2025$female < 1] <- 0
btw_candidates_1983_2025$akad[btw_candidates_1983_2025$akad > 0 & btw_candidates_1983_2025$akad < 1] <- 0

# ========== Define Model Formulas ==========
# Each formula represents a different model specification for predicting election outcomes
# resp_E = candidate vote share (first vote/Erststimme)
# resp_Z = party vote share (second vote/Zweitstimme) 
# l1 = lag (previous election results)

# First, create the swing variables with single coefficients
btw_candidates_1983_2025 <- btw_candidates_1983_2025 %>%
  mutate(
    # Proportional swing with single coefficient
    proportional_swing = res_l1_Z + res_l1_Z * proportional,
    # Uniform swing with single coefficient  
    uniform_swing = res_l1_Z + uniform,
    # Piecewise swing using interaction approach
    # This creates interactions between swing direction, district strength, and swing magnitude
    # The interaction coefficients will show how swing differs in strong vs. weak districts for positive vs. negative swings
    party_avg_vote = mean(res_l1_Z, na.rm = TRUE), .by = c(election, partei),
    above_avg_district = ifelse(res_l1_Z > party_avg_vote, 1, 0),
    positive_swing = ifelse(uniform >= 0, 1, 0),
    negative_swing = ifelse(uniform < 0, 1, 0),
    # Interaction: positive swing in above-average districts
    positive_swing_above_avg = positive_swing * above_avg_district,
    # Interaction: negative swing in above-average districts  
    negative_swing_above_avg = negative_swing * above_avg_district,
    # Piecewise proportional interactions
    # Interaction: positive proportional swing in above-average districts
    positive_proportional_above_avg = positive_swing_above_avg * proportional_swing,
    # Interaction: negative proportional swing in above-average districts
    negative_proportional_above_avg = negative_swing_above_avg * proportional_swing,
    positive_uniform_above_avg = positive_swing_above_avg * uniform_swing,
    negative_uniform_above_avg = negative_swing_above_avg * uniform_swing,
    uniform_above_avg = uniform * above_avg_district,
    proportional_above_avg = proportional * above_avg_district
    
  )

formulas <- data.frame(
  formula = c(
    # 1. Proportional adjustment with single coefficient
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + proportional_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 2. Uniform swing adjustment with single coefficient
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + uniform_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 4. Proportional and uniform together
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + proportional_swing + uniform_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 5. Piecewise swing model (interaction approach)
    # This model allows swing to differ between strong and weak districts for positive vs. negative swings
    # uniform = baseline swing effect, positive_swing_above_avg = effect for positive swings in strong districts
    # negative_swing_above_avg = effect for negative swings in strong districts
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + uniform_swing + positive_uniform_above_avg + negative_uniform_above_avg + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 6. Piecewise proportional model (interaction approach)
    # This model combines piecewise logic with proportional swing behavior
    # proportional = baseline proportional swing, positive_proportional_above_avg = effect for positive proportional swings in strong districts
    # negative_proportional_above_avg = effect for negative proportional swings in strong districts
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + proportional_swing + positive_proportional_above_avg + negative_proportional_above_avg + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 7. Proportional with interaction
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 10. Uniform with interaction
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*uniform_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 11. No adjustment (baseline)
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1"
  ),
  name = c("proportional",
           "uniform",
           "mixed",
           "uniform_piecewise",
           "proportional_piecewise",
           "proportional_interaction",
           "uniform_interaction",
           "no_adjustment"
  )
)

# ========== Initialize Results Data Frame ==========
# This will store evaluation results for each model and election

eval_results <- data.frame(
  formula = character(),
  name = character(),
  election = integer(),
  correct_predictions = integer(),
  total_predictions = integer(),
  accuracy = numeric()
)

# ========== Model Training and Evaluation Loop ==========
# For each model formula, train on all but one election and test on the held-out election (leave-one-election-out cross-validation)
for (formula in formulas$formula) {
  print(formula)
  
  # Convert formula string to R formula object
  model_formula <- as.formula(formula)
  
  # Loop through each election year for out-of-sample testing
  for (this_election in c(1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)) {
    print(this_election)
    election_l1 <- this_election - 4
    
    # Split data: train on all years except the current test year, exclude 'AND' party
    train <- btw_candidates_1983_2025 %>%
      filter(
        (election != this_election),
        partei != "AND"
      )
    
    # Test set: only current election year
    test <- dplyr::filter(btw_candidates_1983_2025, election == this_election)
    
    ### Train Linear Model ----------------------------------
    # Fit linear regression model on training data
    reg <- lm(model_formula, data = train)
    
    summary(reg)
    
    # Predict on test data
    test$predicted <- predict(reg, newdata = test)
    # For each district (wkr), set winner_pred = 1 for candidate with highest predicted value
    test <- test %>% group_by(wkr) %>%  mutate(winner_pred = ifelse(predicted == max(predicted, na.rm = TRUE), 1, 0))
    
    # Print confusion tables for debugging
    table(test$winner == 1)
    test$winner_pred %>% table
    
    # Calculate comprehensive evaluation metrics for ALL observations
    # Out-of-bounds predictions (0-100 range is acceptable)
    out_of_bounds <- sum(test$predicted < 0 | test$predicted > 100, na.rm = TRUE)
    out_of_bounds_pct <- out_of_bounds / nrow(test)
    
    # Mean absolute error for all predictions (not just winners)
    mae_all <- mean(abs(test$predicted - test$resp_E), na.rm = TRUE)
    
    # Root mean square error for all predictions
    rmse_all <- sqrt(mean((test$predicted - test$resp_E)^2, na.rm = TRUE))
    
    # Mean error (bias) for all predictions
    bias_all <- mean(test$predicted - test$resp_E, na.rm = TRUE)
    
    # Store evaluation results
    eval_results <- rbind(
      eval_results,
      data.frame(
        formula = formula,
        name = formulas$name[formulas$formula == formula],
        election = this_election,
        correct_predictions = sum(test$winner == test$winner_pred, na.rm = TRUE),
        total_predictions = nrow(test),
        accuracy = sum(test$winner == test$winner_pred, na.rm = TRUE) / nrow(test),
        correct_winner = sum(test$winner[test$winner == 1] == test$winner_pred[test$winner == 1], na.rm = TRUE),
        accuracy_winner = sum(test$winner[test$winner == 1] == test$winner_pred[test$winner == 1], na.rm = TRUE) / sum(test$winner == 1, na.rm = TRUE),
        # New comprehensive metrics for all observations
        out_of_bounds = out_of_bounds,
        out_of_bounds_pct = out_of_bounds_pct,
        mae_all = mae_all,
        rmse_all = rmse_all,
        bias_all = bias_all
      )
    )  
  }
  
}

# ========== Inspect Results ==========
eval_results %>% head

eval_results$election %>% unique

# ========== Aggregate Results Across Elections ==========
# Summarize model performance by averaging across all elections

eval_aggregate <- eval_results %>%
  group_by(name, formula) %>%
  summarise(
    avg_accuracy = mean(accuracy, na.rm = TRUE),
    avg_correct_predictions = mean(correct_predictions, na.rm = TRUE),
    avg_total_predictions = mean(total_predictions, na.rm = TRUE),
    avg_correct_winner = mean(correct_winner, na.rm = TRUE),
    avg_accuracy_winner = mean(accuracy_winner, na.rm = TRUE),
    max_accuracy_winner = max(accuracy_winner, na.rm = TRUE),
    min_accuracy_winner = min(accuracy_winner, na.rm = TRUE),
    # New comprehensive metrics for all observations
    avg_out_of_bounds_pct = mean(out_of_bounds_pct, na.rm = TRUE),
    total_out_of_bounds = sum(out_of_bounds, na.rm = TRUE),
    avg_mae_all = mean(mae_all, na.rm = TRUE),
    avg_rmse_all = mean(rmse_all, na.rm = TRUE),
    avg_bias_all = mean(bias_all, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_accuracy))
eval_aggregate %>% head

# ========== Save Results to CSV ==========
# Save detailed and aggregate results for further analysis or reporting

eval_results %>% 
  write.csv("data/out/eval_results.csv", row.names = FALSE)

eval_aggregate %>%
  write.csv("data/out/eval_aggregate.csv", row.names = FALSE)

# ========== Create LaTeX Table of Model Performance ==========
# Summarize average, min, and max accuracy for each model across all elections

eval_aggregate %>% dplyr::select(name, avg_accuracy_winner, min_accuracy_winner, max_accuracy_winner) %>% 
  arrange(desc(avg_accuracy_winner)) %>%
  # Create LaTeX table with custom caption and formatting
  xtable(caption = "Average, Maximum and Minimum Accuracy of Candidate Vote Share Prediction Models for the Elections 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025.",
         label = "tab:eval_aggregate",
         digits = c(0, 0, 3, 3, 3)) %>%
  print(include.rownames = FALSE,
        caption.placement = "top",
        table.placement = "H",
        comment = FALSE,
        sanitize.text.function = identity,
        add.to.row = list(pos = list(-1), command = "\\hline \\hline \n"),
        floating = TRUE,
        size = "\\footnotesize",
        file = "tables/eval_aggregate_table.tex")

# Create comprehensive LaTeX table with all metrics
eval_aggregate %>% 
  dplyr::select(name, avg_accuracy_winner, avg_out_of_bounds_pct, avg_mae_all, avg_rmse_all, avg_bias_all) %>%
  mutate(
    avg_out_of_bounds_pct = avg_out_of_bounds_pct * 100,  # Convert to percentage
    avg_mae_all = round(avg_mae_all, 2),
    avg_mae_all = round(avg_mae_all, 2),
    avg_rmse_all = round(avg_rmse_all, 2),
    avg_bias_all = round(avg_bias_all, 2)
  ) %>%
  arrange(desc(avg_accuracy_winner)) %>%
  xtable(caption = "Comprehensive Model Performance Metrics for All Observations (Elections 1998-2025).",
         label = "tab:eval_comprehensive",
         digits = c(0, 0, 3, 2, 2, 2, 2)) %>%
  print(include.rownames = FALSE,
        caption.placement = "top",
        table.placement = "H",
        comment = FALSE,
        sanitize.text.function = identity,
        add.to.row = list(pos = list(-1), command = "\\hline \\hline \n"),
        floating = TRUE,
        size = "\\footnotesize",
        file = "tables/eval_comprehensive_table.tex") 

# ========== Plot Model Performance ==========
# Visualize average, min, and max accuracy for each model

# Create plot with individual election results and overall average
# First, create a factor with proper ordering based on average accuracy
model_order <- eval_aggregate$name[order(eval_aggregate$avg_accuracy_winner, decreasing = TRUE)]
eval_results$name_ordered <- factor(eval_results$name, levels = model_order)
eval_aggregate$name_ordered <- factor(eval_aggregate$name, levels = model_order)

ggplot() +
  # Individual election results as dots with labels
  geom_point(data = eval_results, aes(x = name_ordered, y = accuracy_winner, group = name), 
             position = position_jitter(width = 0.2, seed = 123), 
             alpha = 0.6, size = 2, color = "steelblue") +
  # Add text labels for individual elections
  geom_text(data = eval_results, aes(x = name_ordered, y = accuracy_winner, label = election), 
            position = position_jitter(width = 0.2, seed = 123), 
            size = 2.5, hjust = -0.3, vjust = 0.5, color = "darkblue") +
  # Overall average as a different symbol (diamond)
  geom_point(data = eval_aggregate, aes(x = name_ordered, y = avg_accuracy_winner), 
             shape = 23, size = 4, fill = "red", color = "darkred") +
  # Add average line
  geom_hline(data = eval_aggregate, aes(yintercept = avg_accuracy_winner, group = name_ordered), 
             linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Out-of-sample Performance of Models",
       subtitle = "Individual elections (dots) and overall average (red diamonds)",
       x = "Model",
       y = "Accuracy (Winner Prediction)") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "none")
# Save the plot to pdf
ggsave("figures/accuracy.pdf", width = 10, height = 8)

# ========== Comprehensive Analysis of ALL Observations ==========
# Create plots and analysis for comprehensive evaluation metrics

# 1. Out-of-bounds predictions analysis (sorted by avg_out_of_bounds_pct ascending - lower is better)
model_order_out_of_bounds <- eval_aggregate$name[order(eval_aggregate$avg_out_of_bounds_pct)]
eval_results$name_ordered_out_of_bounds <- factor(eval_results$name, levels = model_order_out_of_bounds)
eval_aggregate$name_ordered_out_of_bounds <- factor(eval_aggregate$name, levels = model_order_out_of_bounds)

ggplot() +
  geom_point(data = eval_results, aes(x = name_ordered_out_of_bounds, y = out_of_bounds_pct * 100, color = factor(election)), 
             position = position_jitter(width = 0.2, seed = 123), size = 3, alpha = 0.7) +
  geom_point(data = eval_aggregate, aes(x = name_ordered_out_of_bounds, y = avg_out_of_bounds_pct * 100), 
             shape = 23, size = 4, fill = "red", color = "darkred") +
  labs(title = "Out-of-Bounds Predictions (0-100% range)",
       subtitle = "Individual elections (colored dots) and overall average (red diamonds) - Sorted by performance (lower is better)",
       x = "Model",
       y = "Percentage of Out-of-Bounds Predictions (%)",
       color = "Election Year") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom")
ggsave("figures/out_of_bounds_analysis.pdf", width = 10, height = 8)

# 2. Mean Absolute Error for all observations (sorted by avg_mae_all ascending - lower is better)
model_order_mae <- eval_aggregate$name[order(eval_aggregate$avg_mae_all)]
eval_results$name_ordered_mae <- factor(eval_results$name, levels = model_order_mae)
eval_aggregate$name_ordered_mae <- factor(eval_aggregate$name, levels = model_order_mae)

ggplot() +
  geom_point(data = eval_results, aes(x = name_ordered_mae, y = mae_all, color = factor(election)), 
             position = position_jitter(width = 0.2, seed = 123), size = 3, alpha = 0.7) +
  geom_point(data = eval_aggregate, aes(x = name_ordered_mae, y = avg_mae_all), 
             shape = 23, size = 4, fill = "red", color = "darkred") +
  labs(title = "Mean Absolute Error for All Observations",
       subtitle = "Individual elections (colored dots) and overall average (red diamonds) - Sorted by performance (lower is better)",
       x = "Model",
       y = "Mean Absolute Error (percentage points)",
       color = "Election Year") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom")
ggsave("figures/mae_all_analysis.pdf", width = 10, height = 8)

# 3. Root Mean Square Error for all observations (sorted by avg_rmse_all ascending - lower is better)
model_order_rmse <- eval_aggregate$name[order(eval_aggregate$avg_rmse_all)]
eval_results$name_ordered_rmse <- factor(eval_results$name, levels = model_order_rmse)
eval_aggregate$name_ordered_rmse <- factor(eval_aggregate$name, levels = model_order_rmse)

ggplot() +
  geom_point(data = eval_results, aes(x = name_ordered_rmse, y = rmse_all, color = factor(election)), 
             position = position_jitter(width = 0.2, seed = 123), size = 3, alpha = 0.7) +
  geom_point(data = eval_aggregate, aes(x = name_ordered_rmse, y = avg_rmse_all), 
             shape = 23, size = 4, fill = "red", color = "darkred") +
  labs(title = "Root Mean Square Error for All Observations",
       subtitle = "Individual elections (colored dots) and overall average (red diamonds) - Sorted by performance (lower is better)",
       x = "Model",
       y = "RMSE (percentage points)",
       color = "Election Year") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom")
ggsave("figures/rmse_all_analysis.pdf", width = 10, height = 8)

# 4. Bias analysis for all observations (sorted by absolute avg_bias_all ascending - lower is better)
model_order_bias <- eval_aggregate$name[order(abs(eval_aggregate$avg_bias_all))]
eval_results$name_ordered_bias <- factor(eval_results$name, levels = model_order_bias)
eval_aggregate$name_ordered_bias <- factor(eval_aggregate$name, levels = model_order_bias)

ggplot() +
  geom_point(data = eval_results, aes(x = name_ordered_bias, y = bias_all, color = factor(election)), 
             position = position_jitter(width = 0.2, seed = 123), size = 3, alpha = 0.7) +
  geom_point(data = eval_aggregate, aes(x = name_ordered_bias, y = avg_bias_all), 
             shape = 23, size = 4, fill = "red", color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  labs(title = "Prediction Bias for All Observations",
       subtitle = "Individual elections (colored dots) and overall average (red diamonds) - Sorted by performance (lower absolute bias is better)",
       x = "Model",
       y = "Mean Bias (percentage points)",
       color = "Election Year") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size = 12),
        legend.position = "bottom")
ggsave("figures/bias_all_analysis.pdf", width = 10, height = 8)


# 5. Model performance comparison heatmap for all metrics
# Prepare data for heatmap with standardized metrics (all higher = better)
# First calculate the min/max values for each metric
accuracy_range <- range(eval_aggregate$avg_accuracy_winner)
out_of_bounds_range <- range(eval_aggregate$avg_out_of_bounds_pct)
mae_range <- range(eval_aggregate$avg_mae_all)
rmse_range <- range(eval_aggregate$avg_rmse_all)
bias_range <- range(abs(eval_aggregate$avg_bias_all))

heatmap_data <- eval_aggregate %>%
  dplyr::select(name, avg_accuracy_winner, avg_out_of_bounds_pct, avg_mae_all, avg_rmse_all, avg_bias_all) %>%
  mutate(
    # Standardize all metrics to 0-1 scale where higher is always better
    # Winner Accuracy: already higher is better, normalize to 0-1
    accuracy_std = (avg_accuracy_winner - accuracy_range[1]) / (accuracy_range[2] - accuracy_range[1]),
    # Out-of-bounds: lower is better, so invert and normalize to 0-1
    out_of_bounds_std = 1 - (avg_out_of_bounds_pct - out_of_bounds_range[1]) / (out_of_bounds_range[2] - out_of_bounds_range[1]),
    # MAE: lower is better, so invert and normalize to 0-1
    mae_std = 1 - (avg_mae_all - mae_range[1]) / (mae_range[2] - mae_range[1]),
    # RMSE: lower is better, so invert and normalize to 0-1
    rmse_std = 1 - (avg_rmse_all - rmse_range[1]) / (rmse_range[2] - rmse_range[1]),
    # Bias: lower absolute value is better, so normalize absolute bias and invert to 0-1
    bias_std = 1 - (abs(avg_bias_all) - bias_range[1]) / (bias_range[2] - bias_range[1])
  ) %>%
  # Create clean data structure with one row per model-metric combination
  mutate(
    # Format raw values for display
    accuracy_text = sprintf("%.3f", avg_accuracy_winner),
    out_of_bounds_text = sprintf("%.2f%%", avg_out_of_bounds_pct * 100),
    mae_text = sprintf("%.4f", avg_mae_all),
    rmse_text = sprintf("%.4f", avg_rmse_all),
    bias_text = sprintf("%.4f", abs(avg_bias_all))
  ) %>%
  # Reshape to long format for plotting
  pivot_longer(
    cols = c(accuracy_std, out_of_bounds_std, mae_std, rmse_std, bias_std),
    names_to = "metric_std",
    values_to = "std_value"
  ) %>%
  mutate(
    metric = case_when(
      metric_std == "accuracy_std" ~ "Winner Accuracy",
      metric_std == "out_of_bounds_std" ~ "Out-of-Bounds Predictions",
      metric_std == "mae_std" ~ "Error: MAE",
      metric_std == "rmse_std" ~ "Error: RMSE",
      metric_std == "bias_std" ~ "Bias"
    ),
    raw_text = case_when(
      metric_std == "accuracy_std" ~ accuracy_text,
      metric_std == "out_of_bounds_std" ~ out_of_bounds_text,
      metric_std == "mae_std" ~ mae_text,
      metric_std == "rmse_std" ~ rmse_text,
      metric_std == "bias_std" ~ bias_text
    )
  ) %>%
  dplyr::select(name, metric, std_value, raw_text)

# Sort models by descending accuracy for the heatmap
model_order_accuracy <- eval_aggregate %>%
  arrange(desc(avg_accuracy_winner)) %>%
  pull(name)

heatmap_data$name_ordered <- factor(heatmap_data$name, levels = rev(model_order_accuracy))

# Create comprehensive heatmap showing performance distribution across models
ggplot(heatmap_data, aes(x = metric, y = name_ordered, fill = std_value)) +
  geom_tile() +
  # Add text labels with raw values
  geom_text(aes(label = raw_text), size = 3, color = "black", fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027", mid = "#fdb863", high = "#1a9850",  # Natural red-yellow-green
    midpoint = 0.5,
    name = "Performance\n(0=Poor, 1=Good)",
    limits = c(0, 1)
  ) +
  labs(title = "Comprehensive Model Performance Heatmap",
       subtitle = "Models sorted by descending accuracy",
       x = "Performance Metric",
       y = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 12))
ggsave("figures/comprehensive_performance_heatmap.pdf", width = 12, height = 8)



# ========== Print Summary Statistics ==========
cat("\n=== COMPREHENSIVE EVALUATION SUMMARY ===\n")
cat("Analysis includes ALL observations, not just winners\n\n")

# Out-of-bounds predictions summary
cat("OUT-OF-BOUNDS PREDICTIONS (0-100% range is acceptable):\n")
cat("Models with highest out-of-bounds predictions:\n")
eval_aggregate %>% 
  arrange(desc(avg_out_of_bounds_pct)) %>%
  dplyr::select(name, avg_out_of_bounds_pct) %>%
  mutate(avg_out_of_bounds_pct = round(avg_out_of_bounds_pct * 100, 2)) %>%
  head(3) %>%
  print()

cat("\nModels with lowest out-of-bounds predictions:\n")
eval_aggregate %>% 
  arrange(avg_out_of_bounds_pct) %>%
  dplyr::select(name, avg_out_of_bounds_pct) %>%
  mutate(avg_out_of_bounds_pct = round(avg_out_of_bounds_pct * 100, 2)) %>%
  head(3) %>%
  print()

# Overall prediction accuracy for all observations
cat("\nOVERALL PREDICTION ACCURACY (All Observations):\n")
cat("Models with lowest MAE:\n")
eval_aggregate %>% 
  arrange(avg_mae_all) %>%
  dplyr::select(name, avg_mae_all) %>%
  head(3) %>%
  print()

cat("\nModels with lowest RMSE:\n")
eval_aggregate %>% 
  arrange(avg_rmse_all) %>%
  dplyr::select(name, avg_rmse_all) %>%
  head(3) %>%
  print()

cat("\nModels with lowest bias:\n")
eval_aggregate %>% 
  arrange(abs(avg_bias_all)) %>%
  dplyr::select(name, avg_bias_all) %>%
  head(3) %>%
  print()

cat("\n=== END COMPREHENSIVE EVALUATION ===\n")

# ========== Evaluate Models with Different Lead Types ==========
# Repeat evaluation using different definitions of the 'proportional' variable (polls_days, polls_weeks, polls_months)
lead_types <- c("polls_days.x", "polls_weeks.x", "polls_months.x")
eval_results_leads <- data.frame(
  formula = character(),
  name = character(),
  lead_type = character(),
  election = integer(),
  correct_predictions = integer(),
  total_predictions = integer(),
  accuracy = numeric()
)

for (lead_type in lead_types) {
  for (formula in formulas$formula) {
    print(paste("Formula:", formula, "Lead:", lead_type))
    # Update 'proportional' variable for this lead type (matching Lukas's approach)
    # Check if the lead type column exists and calculate proportional swing
    if (lead_type %in% names(btw_candidates_1983_2025)) {
      btw_candidates_1983_2025 <- btw_candidates_1983_2025 %>%
        mutate(
          proportional = case_when(
            !is.na(.data[[lead_type]]) & !is.na(vote_share_l1.x) & vote_share_l1.x > 0 ~ (.data[[lead_type]] - vote_share_l1.x) / vote_share_l1.x,
            TRUE ~ NA_real_
          )
        )
    } else {
      print(paste("Warning: Column", lead_type, "not found in data. Available columns:", paste(names(btw_candidates_1983_2025), collapse=", ")))
      # Use the existing proportional variable
    }
    # Loop through elections for out-of-sample testing
    for (this_election in c(1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)) {
      train <- btw_candidates_1983_2025 %>%
        filter((election != this_election), partei != "AND")
      test <- dplyr::filter(btw_candidates_1983_2025, election == this_election)
      model_formula <- as.formula(formula)
      reg <- lm(model_formula, data = train)
      test$predicted <- predict(reg, newdata = test)
      test <- test %>% group_by(wkr) %>% mutate(winner_pred = ifelse(predicted == max(predicted, na.rm = TRUE), 1, 0))
      eval_results_leads <- rbind(
        eval_results_leads,
        data.frame(
          formula = formula,
          name = formulas$name[formulas$formula == formula],
          lead_type = lead_type,
          election = this_election,
          correct_predictions = sum(test$winner == test$winner_pred, na.rm = TRUE),
          total_predictions = nrow(test),
          accuracy = sum(test$winner == test$winner_pred, na.rm = TRUE) / nrow(test),
          winner_accuracy = sum(test$winner[test$winner == 1] == test$winner_pred[test$winner == 1], na.rm = TRUE) / sum(test$winner == 1, na.rm = TRUE)
        )
      )
    }
  }
}

# ========== Aggregate and Save Lead-Type Results ==========
agg_leads <- eval_results_leads %>%
  group_by(name, formula, lead_type) %>%
  summarise(
    avg_accuracy = mean(accuracy, na.rm = TRUE),
    avg_winner_accuracy = mean(winner_accuracy, na.rm = TRUE),
    avg_correct_predictions = mean(correct_predictions, na.rm = TRUE),
    avg_total_predictions = mean(total_predictions, na.rm = TRUE)
  ) %>%
  arrange(lead_type, desc(avg_winner_accuracy))

write.csv(eval_results_leads, "data/out/eval_results_leads.csv", row.names = FALSE)
write.csv(agg_leads, "data/out/eval_aggregate_leads.csv", row.names = FALSE)


