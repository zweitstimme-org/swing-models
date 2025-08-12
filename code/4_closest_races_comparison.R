# ========== Libraries ==========
library(tidyverse)
library(ggplot2)
library(dplyr)

# ========== Source Utility Functions ==========
source("code/0_utils.R")

# ========== Load Data ==========
# Please ensure btw_candidates_1983_2025 is loaded in your environment
# If not, load it here (uncomment and adjust as needed):
# btw_candidates_1983_2025 <- read_csv("data/btw_candidates_1983-2025_full.csv")

load("data/btw_candidates_1983-2025.RData")
btw_candidates_1983_2025 <- btw_candidates_1983_2025 %>% 
  mutate(formercand = ifelse(formercand == 1, 1, 0),
         female = ifelse(female == 1, 1, 0),
         akad = ifelse(akad == 1, 1, 0)
         )

# ========== Define Models ==========
# resp_E = candidate vote share (first vote/Erststimme)
# resp_Z = party vote share (second vote/Zweitstimme)
# l1 = lag (previous election results)
model_formulas <- list(
  proportional_both_votes = as.formula(
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E*proportional + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1"
  ),
  uniform_swing = as.formula(
    "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z + uniform + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1"
  )
)

years <- c(1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)
results_list <- list()



for (year in years) {
  test <- btw_candidates_1983_2025 %>% filter(election == year)
  train <- btw_candidates_1983_2025 %>% filter(election != year, partei != "AND")

  model_preds <- list()
  test_preds <- test
  
  for (model_name in names(model_formulas)) {
    print(paste("Processing model:", model_name, "for year:", year))
    reg <- lm(model_formulas[[model_name]], data = train)
    test_preds[[paste0("pred_", model_name)]] <- predict(reg, newdata = test)

    # For each constituency, get the predicted winner
    test_model <- test_preds %>% mutate(predicted = .data[[paste0("pred_", model_name)]])

    model_winner <- test_model %>%
      group_by(wkr) %>%
      filter(predicted == max(predicted, na.rm = TRUE)) %>%
      slice(1) %>% # In case of ties, take the first
      ungroup() %>%
      dplyr::select(wkr, !!paste0("winner_", model_name) := party)

    model_preds[[model_name]] <- model_winner
  }

  # Join all model predictions by wkr
  all_preds <- reduce(model_preds, left_join, by = "wkr")

  # Get actual winner and margin (based on resp_E)
  actual_top2 <- test %>%
    group_by(wkr) %>%
    arrange(desc(resp_E)) %>%
    slice(1:2) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  actual_margins <- actual_top2 %>%
    group_by(wkr) %>%
    summarise(
      actual_margin = abs(diff(resp_E)),
      actual_winner = party[rank == 1],
      actual_runnerup = party[rank == 2]
    ) %>%
    ungroup()

  all_preds <- left_join(all_preds, actual_margins, by = "wkr")

  # Join land and resp_E for all candidates (not just actual winner)
  # This will keep land and resp_E for each wkr, as in the test set
  land_respE_all <- test %>%
    group_by(wkr) %>%
    slice(1) %>% # land and resp_E are the same for all candidates in a wkr, so just take the first
    ungroup() %>%
    dplyr::select(wkr, land)

  all_preds <- left_join(all_preds, land_respE_all, by = "wkr")

  # Optionally, keep resp_E for the actual winner as a separate column
  actual_winner_respE <- test %>%
    group_by(wkr) %>%
    arrange(desc(resp_E)) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(wkr, resp_E_actual_winner = resp_E)
  all_preds <- left_join(all_preds, actual_winner_respE, by = "wkr")

  # For each model, calculate the error for the predicted winner
  for (model_name in names(model_formulas)) {
    winner_col <- paste0("winner_", model_name)
    error_col <- paste0("error_", model_name)
    all_preds[[error_col]] <- mapply(function(wkr, pred_party, actual_party) {
      resp_pred <- test_preds %>% filter(wkr == !!wkr, party == !!pred_party) %>% pull(!!sym(paste0("pred_", model_name)))
      resp_actual <- test_preds %>% filter(wkr == !!wkr, party == !!actual_party) %>% pull(resp_E)
      if (length(resp_pred) == 0 | length(resp_actual) == 0) return(NA)
      resp_pred - resp_actual
    }, all_preds$wkr, all_preds[[winner_col]], all_preds$actual_winner)
  }

  # Now, for example, get the top 5 closest (smallest absolute error) for each model
  top5_errors <- all_preds %>%
    arrange(abs(error_proportional_both_votes)) %>%
    mutate(year = year)

  # Or output all_preds as is

  # Get top 5 closest races (smallest actual margin)
  top10 <- all_preds %>%
    arrange(actual_margin) %>%
    mutate(year = year)

  results_list[[as.character(year)]] <- top10
}

# Combine all years
final_results <- bind_rows(results_list)

# ========== Output ==========
# Create output directories if they don't exist
if (!dir.exists("data/out")) dir.create("data/out", recursive = TRUE)
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

write_csv(final_results, "data/out/closest_races_comparison.csv")
print(final_results) 


## Save plot to pdf
pdf(file = "figures/error-density.pdf", height = 6, width = 10)
final_results$error_uniform_swing %>% density(na.rm = T) %>% plot(yli = c(0, 5), main = "Density of Errors")
final_results$error_proportional_both_votes %>% density(na.rm = T) %>% lines(col= "red")
dev.off()


# Assuming all_preds has columns: error_proportional_both_votes, year, actual_winner, state
# You may need to join state info if not present

# Combine all years for regression
all_errors <- bind_rows(results_list)

# Add binary correct columns for each model
for (model_name in names(model_formulas)) {
  winner_col <- paste0("winner_", model_name)
  correct_col <- paste0("correct_", model_name)
  all_errors[[correct_col]] <- all_errors[[winner_col]] == all_errors$actual_winner
}

all_errors$error_proportional_both_votes %>% summary
all_errors$error_uniform_swing %>% summary

# Linear regression for error
lm_error <- lm(abs(error_proportional_both_votes) ~ factor(year) + actual_winner + land, data = all_errors)
summary(lm_error)

# Linear regression for mispredicted cases - both models
lm_error_mispred_prop <- lm(abs(error_proportional_both_votes) ~ factor(year) + actual_winner + land, 
                           data = all_errors %>% filter(actual_winner != winner_proportional_both_votes))
lm_error_mispred_uniform <- lm(abs(error_uniform_swing) ~ factor(year) + actual_winner + land, 
                              data = all_errors %>% filter(actual_winner != winner_uniform_swing))

# Logistic regression for correct prediction - both models
glm_correct_prop <- glm(correct_proportional_both_votes ~ factor(year) + actual_winner + land, 
                        data = all_errors, family = binomial)
glm_correct_uniform <- glm(correct_uniform_swing ~ factor(year) + actual_winner + land, 
                           data = all_errors, family = binomial)

# Linear regression for error - both models
lm_error_prop <- lm(abs(error_proportional_both_votes) ~ factor(year) + actual_winner + land, data = all_errors)
lm_error_uniform <- lm(abs(error_uniform_swing) ~ factor(year) + actual_winner + land, data = all_errors)

# --- Error Magnitude Comparison (All Cases) ---
tidy_prop <- tidy(lm_error_prop) %>% mutate(model = "Proportional Both Votes")
tidy_uniform <- tidy(lm_error_uniform) %>% mutate(model = "Uniform Swing")
tidy_both <- bind_rows(tidy_prop, tidy_uniform) %>% filter(term != "(Intercept)")
ggplot(tidy_both, aes(x = term, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  labs(
    x = "Coefficient",
    y = "Estimate",
    color = "Model",
    shape = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("figures/coefplot_error_comparison_custom.pdf", width = 10, height = 6)

# --- Error Magnitude Comparison (Mispredicted Cases) ---
tidy_mispred_prop <- tidy(lm_error_mispred_prop) %>% mutate(model = "Proportional Both Votes")
tidy_mispred_uniform <- tidy(lm_error_mispred_uniform) %>% mutate(model = "Uniform Swing")
tidy_mispred_both <- bind_rows(tidy_mispred_prop, tidy_mispred_uniform) %>% filter(term != "(Intercept)")
ggplot(tidy_mispred_both, aes(x = term, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  labs(
    x = "Coefficient",
    y = "Estimate",
    color = "Model",
    shape = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("figures/coefplot_error_mispredicted_comparison_custom.pdf", width = 10, height = 6)

# --- Correct Prediction Comparison (Logistic) ---
tidy_logit_prop <- tidy(glm_correct_prop) %>% mutate(model = "Proportional Both Votes")
tidy_logit_uniform <- tidy(glm_correct_uniform) %>% mutate(model = "Uniform Swing")
tidy_logit_both <- bind_rows(tidy_logit_prop, tidy_logit_uniform) %>% filter(term != "(Intercept)")
ggplot(tidy_logit_both, aes(x = term, y = estimate, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                position = position_dodge(width = 0.5), width = 0.2) +
  coord_flip() +
  labs(
    x = "Coefficient",
    y = "Estimate (log-odds)",
    color = "Model",
    shape = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("figures/coefplot_correct_comparison_custom.pdf", width = 10, height = 6)

# Summary statistics comparison
cat("\n=== ERROR MAGNITUDE COMPARISON ===\n")
cat("Proportional Both Votes - Mean Error:", mean(abs(all_errors$error_proportional_both_votes), na.rm = TRUE), "\n")
cat("Uniform Swing - Mean Error:", mean(abs(all_errors$error_uniform_swing), na.rm = TRUE), "\n")

cat("\n=== CORRECT PREDICTION RATE COMPARISON ===\n")
cat("Proportional Both Votes - Correct Rate:", mean(all_errors$correct_proportional_both_votes, na.rm = TRUE), "\n")
cat("Uniform Swing - Correct Rate:", mean(all_errors$correct_uniform_swing, na.rm = TRUE), "\n")

# Paired t-test for error comparison
error_comparison <- t.test(abs(all_errors$error_proportional_both_votes), 
                          abs(all_errors$error_uniform_swing), 
                          paired = TRUE)
cat("\n=== PAIRED T-TEST FOR ERROR COMPARISON ===\n")
print(error_comparison)

# For proportional_both_votes model
mispredicted_proportional <- all_errors %>%
  filter(winner_proportional_both_votes != actual_winner) %>%
  mutate(
    predicted_resp_E_actual_winner = error_proportional_both_votes + resp_E_actual_winner,
    model = "proportional_both_votes"
  ) %>%
  dplyr::select(year, wkr, actual_winner, winner_proportional_both_votes, resp_E_actual_winner, predicted_resp_E_actual_winner, model)

# For uniform_swing model
mispredicted_uniform <- all_errors %>%
  filter(winner_uniform_swing != actual_winner) %>%
  mutate(
    predicted_resp_E_actual_winner = error_uniform_swing + resp_E_actual_winner,
    model = "uniform_swing"
  ) %>%
  dplyr::select(year, wkr, actual_winner, winner_uniform_swing, resp_E_actual_winner, predicted_resp_E_actual_winner, model)

# Combine both models
mispredicted_df <- bind_rows(mispredicted_proportional, mispredicted_uniform)

# Plot
ggplot(mispredicted_df, aes(x = resp_E_actual_winner, y = predicted_resp_E_actual_winner)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  stat_density_2d(geom = "polygon", aes(alpha = ..level.., fill = model)) +
  # geom_density_2d(aes(color = model), alpha = 0.7) +
  geom_point(aes(color = model, shape = model), alpha = 0.7) +
  facet_wrap(~ year) +
  labs(
    x = "Actual Candidate Vote Share of Winner",
    y = "Predicted Candidate Vote Share of Winner",
    title = "Actual vs Predicted Results for Mispredicted Constituency Winners"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank())


# Save as pdf
ggsave("figures/mispredicted_results.pdf", width = 10, height = 6)

# Create connected plot for 2025
mispredicted_2025 <- mispredicted_df %>% 
  filter(year == 2025) %>%
  dplyr::select(wkr, actual_winner, resp_E_actual_winner, predicted_resp_E_actual_winner, model) %>%
  pivot_wider(
    names_from = model,
    values_from = predicted_resp_E_actual_winner,
    names_prefix = "pred_"
  )

# Plot for 2025 with connected dots
ggplot(mispredicted_2025, aes(x = resp_E_actual_winner)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  geom_segment(
    aes(xend = resp_E_actual_winner, y = pred_proportional_both_votes, yend = pred_uniform_swing),
    alpha = 0.3, color = "black"
  ) +
  geom_point(aes(y = pred_proportional_both_votes, color = "proportional_both_votes", shape = "proportional_both_votes"), alpha = 0.7) +
  geom_point(aes(y = pred_uniform_swing, color = "uniform_swing", shape = "uniform_swing"), alpha = 0.7) +
  labs(
    x = "Actual Candidate Vote Share of Winner",
    y = "Predicted Candidate Vote Share of Winner",
    # title = "Actual vs Predicted Results for Mispredicted Constituency Winners (2025)",
    color = "Model",
    shape = "Model"
  ) +
  scale_color_manual(
    values = c("proportional_both_votes" = "#1b9e77", "uniform_swing" = "#d95f02"),
    labels = c("proportional_both_votes" = "Proportional Both Votes", "uniform_swing" = "Uniform Swing")
  ) +
  scale_shape_manual(
    values = c("proportional_both_votes" = 16, "uniform_swing" = 17),
    labels = c("proportional_both_votes" = "Proportional Both Votes", "uniform_swing" = "Uniform Swing")
  ) +
  theme_minimal()

# Save 2025 plot
ggsave("figures/mispredicted_results_2025_connected.pdf", width = 8, height = 6)

# Prepare data: one row per (year, wkr), columns for both models' predictions
mispredicted_wide <- mispredicted_df %>%
  dplyr::select(year, wkr, resp_E_actual_winner, model, predicted_resp_E_actual_winner) %>%
  pivot_wider(
    names_from = model,
    values_from = predicted_resp_E_actual_winner,
    names_prefix = "pred_"
  )

# Faceted plot with connecting lines, shaded density, no title, legends at the bottom
ggplot(mispredicted_wide, aes(x = resp_E_actual_winner)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  # Add shaded density for each model
  stat_density_2d(
    data = mispredicted_df %>% filter(model == "proportional_both_votes"),
    aes(y = predicted_resp_E_actual_winner, fill = "proportional_both_votes"),
    geom = "polygon",
    color = NA,
    alpha = 0.1,
    show.legend = FALSE
  ) +
  stat_density_2d(
    data = mispredicted_df %>% filter(model == "uniform_swing"),
    aes(y = predicted_resp_E_actual_winner, fill = "uniform_swing"),
    geom = "polygon",
    color = NA,
    alpha = 0.1,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(
      xend = resp_E_actual_winner,
      y = pred_proportional_both_votes,
      yend = pred_uniform_swing
    ),
    alpha = 0.3, color = "black"
  ) +
  geom_point(aes(y = pred_proportional_both_votes, color = "proportional_both_votes", shape = "proportional_both_votes"), alpha = 0.7) +
  geom_point(aes(y = pred_uniform_swing, color = "uniform_swing", shape = "uniform_swing"), alpha = 0.7) +
  facet_wrap(~ year) +
  labs(
    x = "Actual Candidate Vote Share of Winner",
    y = "Predicted Candidate Vote Share of Winner",
    color = "Model",
    shape = "Model"
  ) +
  scale_color_manual(
    values = c("proportional_both_votes" = "#1b9e77", "uniform_swing" = "#d95f02"),
    labels = c("proportional_both_votes" = "Proportional Both Votes", "uniform_swing" = "Uniform Swing")
  ) +
  scale_fill_manual(
    values = c("proportional_both_votes" = "#1b9e77", "uniform_swing" = "#d95f02"),
    labels = c("proportional_both_votes" = "Proportional Both Votes", "uniform_swing" = "Uniform Swing")
  ) +
  scale_shape_manual(
    values = c("proportional_both_votes" = 16, "uniform_swing" = 17),
    labels = c("proportional_both_votes" = "Proportional Both Votes", "uniform_swing" = "Uniform Swing")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_blank())

ggsave("figures/mispredicted_results_connected_facets.pdf", width = 12, height = 8)




