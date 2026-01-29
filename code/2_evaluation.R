
# ----------------------------------------------------------
# District-Level Swing Model Project
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
  library(gt)
  library(scales)
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
    # 3. Proportional and uniform together
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + proportional_swing + uniform_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 4. Piecewise swing model (interaction approach)
    # This model allows swing to differ between strong and weak districts for positive vs. negative swings
    # uniform = baseline swing effect, positive_swing_above_avg = effect for positive swings in strong districts
    # negative_swing_above_avg = effect for negative swings in strong districts
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + uniform_swing + positive_uniform_above_avg + negative_uniform_above_avg + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 5. Piecewise proportional model (interaction approach)
    # This model combines piecewise logic with proportional swing behavior
    # proportional = baseline proportional swing, positive_proportional_above_avg = effect for positive proportional swings in strong districts
    # negative_proportional_above_avg = effect for negative proportional swings in strong districts
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + proportional_swing + positive_proportional_above_avg + negative_proportional_above_avg + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 6. Proportional with interaction
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 7. Uniform with interaction
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*uniform_swing + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
    # 8. No adjustment (baseline)
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


# gt table for accuarcy across elections

# Reshape eval_results
table_data <- eval_results %>%
  select(name, election, accuracy_winner) %>%
  pivot_wider(names_from = election, values_from = accuracy_winner) %>%
  left_join(eval_aggregate %>% select(name, avg_accuracy_winner), by = "name") %>%
  rename(Average = avg_accuracy_winner) %>%
  arrange(desc(Average))  # sort descending by Average

# Identify all accuracy columns (including Average)
accuracy_cols <- setdiff(names(table_data), "name")

# Determine global min/max across all accuracy values for gradient
acc_range <- range(table_data %>% select(all_of(accuracy_cols)), na.rm = TRUE)

# Create gt table
gt_table <- table_data %>%
  mutate(name = str_to_title(name) %>% str_replace("_", " ")) %>%
  gt(rowname_col = "name") %>%
  # Apply linear color gradient to all accuracy columns (red → green)
  data_color(
    columns = all_of(accuracy_cols),
    colors = scales::col_numeric(
      palette = c("#d73027", "#1a9850"),
      domain = acc_range
    ),
    apply_to = "fill"
  ) %>%
  fmt_number(
    columns = all_of(accuracy_cols),
    decimals = 3
  ) %>%
  fmt_percent(decimals = 1) %>% 
  # Bold Average column
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = vars(Average))
  ) %>%
  # Vertical border before Average
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(columns = vars(Average))
  )

# Export
gtsave(gt_table, "figures/model_accuracy_table.png")

# ========== Comprehensive Analysis of ALL Observations ==========
# Create plots and analysis for comprehensive evaluation metrics

# Model performance comparison heatmap for all metrics
# Prepare data for heatmap with standardized metrics (all higher = better)
# First calculate the min/max values for each metric
accuracy_range <- range(eval_aggregate$avg_accuracy_winner)
out_of_bounds_range <- range(eval_aggregate$avg_out_of_bounds_pct)
mae_range <- range(eval_aggregate$avg_mae_all)
rmse_range <- range(eval_aggregate$avg_rmse_all)
bias_range <- range(abs(eval_aggregate$avg_bias_all))

heatmap_data <- eval_aggregate %>%
  dplyr::select(name, avg_accuracy_winner, avg_out_of_bounds_pct, avg_mae_all, avg_rmse_all, avg_bias_all)


# 1. Prepare clean table with correct column order, descending accuracy
heatmap_data_clean <- heatmap_data %>%
  transmute(
    name,
    `Winner accuracy` = avg_accuracy_winner,
    `MAE` = avg_mae_all,
    `RMSE` = avg_rmse_all,
    `Out-of-bounds (share)` = avg_out_of_bounds_pct
  ) %>%
  arrange(desc(`Winner accuracy`))

# Metric columns (everything except model name)
metric_cols <- setdiff(names(heatmap_data_clean), "name")

# 2. Build GT table
gt_table <- heatmap_data_clean %>%
  mutate(name = str_to_title(name) %>% str_replace("_", " ")) %>%
  as.data.frame() %>%
  gt(rowname_col = "name") %>%
  fmt_number(
    columns = all_of(metric_cols),
    decimals = 4
  ) %>%
  cols_width(
    name ~ px(200),
    everything() ~ px(100)
  ) %>% 
  fmt_percent(decimals = 1)

# 3. Apply coloring per column
for (col in metric_cols) {
  if (col == "Winner accuracy") {
    # Higher = better
    gt_table <- gt_table %>%
      data_color(
        columns = all_of(col),
        colors = col_numeric(
          palette = c("#d73027", "#fdb863", "#1a9850"),  # red → yellow → green
          domain = range(heatmap_data_clean[[col]], na.rm = TRUE)
        )
      )
  } else {
    # Lower = better (MAE, RMSE, Out-of-bounds)
    gt_table <- gt_table %>%
      data_color(
        columns = all_of(col),
        colors = col_numeric(
          palette = c("#1a9850", "#fdb863", "#d73027"),  # green → yellow → red
          domain = range(heatmap_data_clean[[col]], na.rm = TRUE)
        )
      )
  }
}

# 5. Save as PNG
gtsave(gt_table, "figures/comprehensive_performance_heatmap.png")
cat("GT heatmap table saved as PNG!\n")



