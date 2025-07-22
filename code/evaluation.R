
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
formulas <- data.frame(
  formula = c(
    # 1. Proportional adjustment using second vote
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1", # 28 wrong in 2021
    # 2. Uniform swing adjustment
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z + uniform + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1", # 73 wrong in 2025
    # 3. Proportional squared and uniform swing
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional + res_l1_Z*I(proportional^2)  + uniform + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1", # 31 wrong in 2025
    # 4. Proportional adjustment using first vote
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E*proportional + res_l1_Z + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1", # 34 wrong in 2021
    # 5. Proportional adjustment for both votes
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E*proportional + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1", # 28 wrong in 2021
    # 6. No adjustment (baseline)
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1" # 28 wrong in 2021
  ),
  name = c("proportional second vote",
           "uniform",
           "proportional squared + uniform",
           "proportional first vote",
           "proportional both votes",
           "no adjustment"
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
        accuracy_winner = sum(test$winner[test$winner == 1] == test$winner_pred[test$winner == 1], na.rm = TRUE) / sum(test$winner == 1, na.rm = TRUE)
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
    min_accuracy_winner = min(accuracy_winner, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_accuracy))
eval_aggregate %>% head

# ========== Save Results to CSV ==========
# Save detailed and aggregate results for further analysis or reporting

eval_results %>% 
  write.csv("data/out/eval_results.csv", row.names = FALSE)

eval_aggregate %>%
  write.csv("data/out/eval_aggregate.csv", row.names = FALSE)

# ========== Evaluate Models with Different Lead Types ==========
# Repeat evaluation using different definitions of the 'proportional' variable (polls_days, polls_weeks, polls_months)
lead_types <- c("polls_days", "polls_weeks", "polls_months")
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
    # Update 'proportional' variable for this lead type
    btw_candidates_1983_2025$proportional <- btw_candidates_1983_2025[[lead_type]] / btw_candidates_1983_2025$vote_share_l1
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

# ========== Create LaTeX Table of Model Performance ==========
# Summarize average, min, and max accuracy for each model across all elections

eval_aggregate %>% select(name, avg_accuracy_winner, min_accuracy_winner, max_accuracy_winner) %>% 
  arrange(desc(avg_accuracy_winner)) %>%
  # Create LaTeX table with custom caption and formatting
  xtable(caption = "Average, Maximum and Minimum Accuracy of the Models for the Elections 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025.",
         label = "tab:eval_aggregate",
         digits = c(0, 0, 2, 2, 2)) %>%
  print(include.rownames = FALSE,
        caption.placement = "top",
        table.placement = "H",
        comment = FALSE,
        sanitize.text.function = identity,
        add.to.row = list(pos = list(-1), command = "\\hline \\hline \n"),
        floating = TRUE,
        size = "\\footnotesize") 

# ========== Plot Model Performance ==========
# Visualize average, min, and max accuracy for each model

ggplot(eval_aggregate, aes(x = reorder(name, avg_accuracy_winner), y = avg_accuracy_winner)) +
  geom_point(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = min_accuracy_winner, ymax = max_accuracy_winner), width = 0.2) +
  labs(title = "Out-of-sample Performance of Models",
       x = "Model",
       y = "Average Accuracy (Winner)") +
  theme_minimal() +
  coord_flip() +
  theme(text = element_text(size = 12))
# Save the plot to pdf
ggsave("data/out/accuracy.pdf", width = 8, height = 6)


