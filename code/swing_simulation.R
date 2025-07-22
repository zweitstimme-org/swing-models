# swing_simulation.R
# Systematic simulation of district-level election results and swing model performance
# Simulates many elections across a grid of scenarios

library(tidyverse)
library(MASS)

# ========== 1. Define Parameter Grid ==========
set.seed(123)

n_districts <- 100
n_sim_per_scenario <- 100

# Parameter grid
param_grid <- expand.grid(
  n_parties = c(3, 5, 7),
  baseline_type = c("flat", "realistic", "skewed"),
  swing_sd = c(0.01, 0.03, 0.06, 0.10, 0.15), # add more extreme swings
  district_swing_sd = c(0.01, 0.03, 0.06, 0.10, 0.15),
  stringsAsFactors = FALSE
)

# Helper: generate baseline shares
make_baseline_shares <- function(n_parties, type) {
  if (type == "flat") {
    rep(1/n_parties, n_parties)
  } else if (type == "realistic") {
    # 1-2 large, rest small
    x <- rep(0.1, n_parties)
    x[1] <- 0.4
    if (n_parties > 3) x[2] <- 0.25
    x <- x / sum(x)
    x
  } else if (type == "skewed") {
    # One dominant
    x <- rep(0.05, n_parties)
    x[1] <- 0.7
    x <- x / sum(x)
    x
  }
}

# Helper: classify party size
party_size_class <- function(baseline_share) {
  if (baseline_share >= 0.2) {
    "large"
  } else if (baseline_share >= 0.1) {
    "medium"
  } else {
    "small"
  }
}

# ========== 2. Simulation Function ==========
simulate_election <- function(n_parties, baseline_shares, swing_sd, district_swing_sd, n_districts) {
  # Baseline for each district
  baseline_matrix <- matrix(NA, nrow = n_districts, ncol = n_parties)
  for (p in 1:n_parties) {
    baseline_matrix[, p] <- rnorm(n_districts, mean = baseline_shares[p], sd = 0.02)
  }
  colnames(baseline_matrix) <- paste0("P", 1:n_parties)

  # National swing for each party
  proportional_threshold <- 0.1 # 10%
  national_swing <- numeric(n_parties)
  for (p in 1:n_parties) {
    if (baseline_shares[p] < proportional_threshold) {
      # Proportional swing for small parties
      prop_factor <- rnorm(1, mean = 0, sd = swing_sd) # e.g., mean 0, sd 0.2
      national_swing[p] <- baseline_shares[p] * prop_factor
    } else {
      # Absolute swing for large parties
      national_swing[p] <- rnorm(1, mean = 0, sd = swing_sd)
    }
  }

  # District-level swing (no spatial correlation for simplicity)
  district_swings <- lapply(1:n_parties, function(p) rnorm(n_districts, 0, district_swing_sd))

  # True vote shares
  true_vote_shares <- baseline_matrix
  for (p in 1:n_parties) {
    true_vote_shares[, p] <- baseline_matrix[, p] + national_swing[p] + district_swings[[p]]
  }
  true_vote_shares <- t(apply(true_vote_shares, 1, function(x) pmax(x, 0)))
  true_vote_shares <- t(apply(true_vote_shares, 1, function(x) x / sum(x)))

  # Simulate poll (national swing estimate)
  poll_error_sd <- 0.01
  polls <- national_swing + rnorm(n_parties, 0, poll_error_sd)

  # Uniform swing prediction
  pred_uniform <- baseline_matrix
  for (p in 1:n_parties) {
    pred_uniform[, p] <- baseline_matrix[, p] + polls[p]
  }
  pred_uniform <- t(apply(pred_uniform, 1, function(x) pmax(x, 0)))
  pred_uniform <- t(apply(pred_uniform, 1, function(x) x / sum(x)))

  # Suppose baseline_shares[p] is the national baseline for party p
  polling_change <- (polls - baseline_shares) / baseline_shares

  # Proportional swing model
  pred_prop <- baseline_matrix
  for (p in 1:n_parties) {
    pred_prop[, p] <- baseline_matrix[, p] * (1 + polling_change[p])
  }
  pred_prop <- t(apply(pred_prop, 1, function(x) pmax(x, 0)))
  pred_prop <- t(apply(pred_prop, 1, function(x) x / sum(x)))

  # RMSE by party
  rmse <- function(pred, truth) sqrt(mean((pred - truth)^2))
  results <- tibble(
    party = rep(colnames(true_vote_shares), each = n_districts),
    district = rep(1:n_districts, times = n_parties),
    baseline = rep(baseline_shares, each = n_districts),
    true = as.vector(true_vote_shares),
    uniform = as.vector(pred_uniform),
    proportional = as.vector(pred_prop)
  )
  results <- results %>%
    group_by(party, baseline) %>%
    summarize(
      RMSE_uniform = rmse(uniform, true),
      RMSE_proportional = rmse(proportional, true),
      .groups = "drop"
    )
  results$party_size <- sapply(results$baseline, party_size_class)
  results
}

# ========== 3. Run Simulations Across Grid ==========
all_results <- list()
for (i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  n_parties <- params$n_parties
  baseline_shares <- make_baseline_shares(n_parties, params$baseline_type)
  swing_sd <- params$swing_sd
  district_swing_sd <- params$district_swing_sd

  scenario_results <- map_dfr(1:n_sim_per_scenario, ~simulate_election(
    n_parties, baseline_shares, swing_sd, district_swing_sd, n_districts
  ))
  scenario_results <- scenario_results %>%
    mutate(
      n_parties = n_parties,
      baseline_type = params$baseline_type,
      swing_sd = swing_sd,
      district_swing_sd = district_swing_sd
    )
  all_results[[i]] <- scenario_results
  cat("Finished scenario", i, "of", nrow(param_grid), "\n")
}
all_results_df <- bind_rows(all_results)

# ========== 4. Aggregate and Summarize ==========
summary_results <- all_results_df %>%
  group_by(n_parties, baseline_type, swing_sd, district_swing_sd, party_size) %>%
  summarize(
    RMSE_uniform = mean(RMSE_uniform),
    RMSE_proportional = mean(RMSE_proportional),
    n = n(),
    .groups = "drop"
  )

print(summary_results)

# ========== 5. Visualize ==========
results_long <- summary_results %>%
  pivot_longer(cols = starts_with("RMSE"), names_to = "model", values_to = "RMSE")
ggplot(results_long, aes(x = party_size, y = RMSE, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(n_parties ~ baseline_type + swing_sd + district_swing_sd) +
  labs(title = "RMSE by party size, scenario, and swing model",
       y = "RMSE",
       x = "Party size") 

