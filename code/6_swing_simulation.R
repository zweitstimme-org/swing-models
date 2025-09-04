# swing_simulation.R
# Simplified simulation focusing on small parties with strongholds
# Tests whether proportional swing works better for clustered small parties

library(ggplot2)
library(tidyverse)
library(MASS)
library(patchwork)

# ========== 1. Define Simplified Parameter Grid ==========
set.seed(123)

n_districts <- 100
n_sim_per_scenario <- 1

# Simplified parameter grid focusing on small party clustering
param_grid <- expand.grid(
  n_parties = c(2, 3, 4, 5, 6, 7),  # At least 1 large party
  n_small_parties = c(0, 2, 4, 6),  # More small parties to test proportional
  n_stronghold_districts = c(10, 25, 40),  # How many districts have high support
  stronghold_share = c(0.3, 0.45, 0.60),   # Vote share in stronghold districts
  national_level_vote_variance = c(0.01, 0.03, 0.05, 0.07, 0.09),  # National-level vote volatility
  uniform_share = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),  # Data generation share of uniform swing
  between_district_vote_volatility = c(0.002, 0.005, 0.01, 0.015, 0.02, 0.025, 0.03),  # Between-district vote volatility
  stringsAsFactors = FALSE
)

# Filter out invalid combinations and unrealistic party combinations
param_grid <- param_grid %>%
  filter(n_small_parties <= n_parties - 1) %>%
  # Add realistic constraints: max 4-5 large parties when 0 small parties, 
  # and for each 2 small parties, reduce max large parties by 1
  mutate(n_large_parties = n_parties - n_small_parties) %>%
  filter(
    # When no small parties: max 5 large parties
    !(n_small_parties == 0 & n_large_parties > 5) &
    # When 2 small parties: max 4 large parties  
    !(n_small_parties == 2 & n_large_parties > 4) &
    # When 4 small parties: max 3 large parties
    !(n_small_parties == 4 & n_large_parties > 3) &
    # When 6 small parties: max 2 large parties
    !(n_small_parties == 6 & n_large_parties > 2) &
    # Always need at least 1 large party
    n_large_parties >= 1
  ) %>%
  dplyr::select(-n_large_parties)  # Remove the helper column



# Helper: generate baseline shares with small party strongholds
make_baseline_with_strongholds <- function(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, n_districts, between_district_vote_volatility) {
  n_large_parties <- n_parties - n_small_parties
  
  # Large parties: more balanced support
  if (n_large_parties == 1) {
    large_baseline <- 0.35  # Reduced from 0.6 to be less dominant
  } else if (n_large_parties == 2) {
    large_baseline <- c(0.30, 0.25)  # More balanced
  } else if (n_large_parties == 3) {
    large_baseline <- c(0.25, 0.20, 0.15)  # More balanced
  } else {
    # Fallback for any other number of large parties
    large_baseline <- rep(0.20, n_large_parties)  # Reduced from 0.25
  }
  
  # Small parties: more realistic baseline (like German small parties)
  small_baseline <- rep(0.10, n_small_parties)  # Increased from 0.12 to 0.15
  
  baseline_shares <- c(large_baseline, small_baseline)
  baseline_shares <- baseline_shares / sum(baseline_shares)  # Normalize
  
  # Create district-specific baseline matrix
  baseline_matrix <- matrix(NA, nrow = n_districts, ncol = n_parties)
  
  # Make party matrix
  baseline_matrix[, 1:n_parties] <- matrix(
      rnorm(n_districts * n_parties, 
            mean = rep(baseline_shares[1:n_parties], each = n_districts), 
            sd = between_district_vote_volatility),  # Increased from 0.03 to 0.08 for more variation
      nrow = n_districts, ncol = n_parties
    )
    # Ensure that all values between 0 and 100
    baseline_matrix[, 1:n_parties] <- pmax(pmin(baseline_matrix[, 1:n_parties], 0.9), 0.05)
  
    # colMeans(baseline_matrix)
  
    # Identify strongholds for each party
    stronghold_districts <- matrix(NA, n_stronghold_districts, ncol = n_parties)
    for (i in 1:n_parties) {
      # Randomly select (potential) stronghold districts for this party
      stronghold_districts[, i] <- sample(1:n_districts, n_stronghold_districts)
    }
  
  # Normalize each district to sum to 1
  baseline_matrix <- t(apply(baseline_matrix, 1, function(x) x / sum(x)))
  
  # Debug: Check baseline distribution
  if (FALSE) {  # Set to TRUE to debug
    cat("Baseline shares:", round(baseline_shares, 3), "\n")
    cat("Baseline matrix summary:\n")
    print(apply(baseline_matrix, 2, summary))
  }
  
  list(baseline_matrix = baseline_matrix, baseline_shares = baseline_shares, stronghold_districts)
}

# Helper: classify party size
party_size_class <- function(party_index, n_large_parties) {
  if (party_index <= n_large_parties) {
    "large"
  } else {
    "small"
  }
}

# ========== 2. Simulation Function ==========
simulate_election <- function(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_level_vote_variance, between_district_vote_volatility, n_districts, uniform_share) {
  # Generate training data (more elections with same parameters)
  n_elections <- 5  # Number of elections for training and testing
  
  # Generate training elections (historical data with same parameters)
  training_elections <- list()
  for (election in 1:n_elections) {
    # Generate baseline for this training election (same parameters)
    training_baseline_data <- make_baseline_with_strongholds(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, n_districts, between_district_vote_volatility)
    training_baseline_matrix <- training_baseline_data$baseline_matrix
    training_baseline_shares <- training_baseline_data$baseline_shares
    training_stronghold_districts <- training_baseline_data$stronghold_districts
    colnames(training_baseline_matrix) <- paste0("P", 1:n_parties)
    
    # Generate national swing for training election
    training_national_swing <- rnorm(n_parties, mean = 0, sd = national_level_vote_variance)
    
    # Apply proportional effects to training district-level swings with distribution
    # Create proportional effects as a distribution: strongholds get higher effects, non-strongholds get lower effects
    # if (party_system_regionalization > 0) {  # Higher regionalization = more concentration
      for (i in 1:n_parties) {
        # Identify stronghold districts for this party (districts with high baseline)
        
        
        # Assign higher baseline share in stronghold districts
        training_baseline_matrix[training_stronghold_districts[, i], i] <- rnorm(n_stronghold_districts, 
                                                               mean = stronghold_share, 
                                                               sd = between_district_vote_volatility)
        
        # Ensure that all values between 0 and 100
        training_baseline_matrix[, 1:n_parties] <- pmax(pmin(training_baseline_matrix[, 1:n_parties], 0.9), 0.05)
        
      }
    
    
    # Normalize each district to sum to 1
    training_baseline_matrix <- t(apply(training_baseline_matrix, 1, function(x) x / sum(x)))
    
    # True vote shares for training election - apply national swing with proportional effects
    # Start with baseline
    training_true_vote_shares <- training_baseline_matrix
    
    # Apply national swing with uniform component + district variation (proportional effects already applied to district swings)
    for (i in 1:n_parties) {
      # National swing for this party (same across all districts)
      national_swing_for_party <- training_national_swing[i]
      
      # Always apply uniform swing component
      uniform_swing <- rnorm(n_districts, 
                             mean = national_swing_for_party, 
                             sd = between_district_vote_volatility)
      
      # Calculate the actual swing (difference between true and baseline)
      
      # Add proportional swing
      proportional_swing <- (rnorm(n_districts, national_swing_for_party, between_district_vote_volatility) * 
                               (training_baseline_matrix[, i]))
      proportional_swing %>% mean
      
      actual_swing <- uniform_swing * (uniform_share) + proportional_swing * (1 - uniform_share)
      
      
      # Calculate final vote share
      training_true_vote_shares[, i] <- training_baseline_matrix[, i] + actual_swing
      
      # Debug: Check if national swing is being applied correctly
      if (i == 1) {  # Only for first party to avoid spam
        cat("Party", i, "national swing:", round(national_swing_for_party, 3), 
            "actual_swing mean:", round(mean(actual_swing), 3), "\n")
      }
      # }
      
      # Ensure non-negative and sum to 1
      training_true_vote_shares <- t(apply(training_true_vote_shares, 1, function(x) pmax(x, 0)))
      training_true_vote_shares <- t(apply(training_true_vote_shares, 1, function(x) x / sum(x)))
      
      training_elections[[election]] <- list(
        baseline = training_baseline_shares,
        baseline_matrix = training_baseline_matrix,
        true_vote_shares = training_true_vote_shares,
        national_swing = training_national_swing
      )
    }
    
    
    }
    
    
  
  # Train swing coefficients using regression formulas
  # Create long format training data with one row per district-party-election (vectorized)
  training_data <- map_dfr(1:(n_elections-1), function(election) {
    election_data <- training_elections[[election]]
    
    # Vectorized operations for all districts and parties at once
    # Convert matrices to vectors by rows (district-wise)
    baseline_matrix_long <- as.vector(t(election_data$baseline_matrix))
    true_shares_long <- as.vector(t(election_data$true_vote_shares))
    
    # Create indices for districts and parties (matching the vectorized order)
    district_indices <- rep(1:n_districts, each = n_parties)
    party_indices <- rep(1:n_parties, times = n_districts)
    
    # Calculate n_large_parties for the training data
    n_large_parties <- n_parties - n_small_parties
    
    # Calculate true swings
    # true_swings <- (election_data$true_vote_shares %>% colMeans) - (election_data$baseline_matrix %>% colMeans)
    
    # Create the data frame with national-level swing variables
    tibble(
      election = election,
      district = district_indices,
      party = paste0("P", party_indices),
      baseline_share = baseline_matrix_long,
      true_share = true_shares_long,
      # National-level variables for swing calculations
      national_baseline_share = rep(election_data$baseline, n_districts),
      national_swing = rep(election_data$national_swing, n_districts),
      is_small_party = party_indices > n_large_parties
    ) 
    
  })
  
  training_data$diff <- training_data$true_share - training_data$baseline_share
  
  # Create pre-calculated swing variables with single coefficients using national-level data
  training_data <- training_data %>%
    mutate(
      # Proportional swing: national_baseline_share * ((national_polling_share - national_baseline_share) / national_baseline_share)
      proportional_swing = national_baseline_share * (1 + national_swing),
      # Uniform swing: national_polling_share - national_baseline_share
      uniform_swing = national_baseline_share + national_swing
    )
  
  # training_data %>% View
  cor(dplyr::select(training_data, c(diff, national_swing)))
  
  # Debug: Check the relationship between swing and diff
  cat("Correlation between diff and actual_swing:", cor(training_data$diff, training_data$national_swing), "\n")
  cat("Summary of actual_swing:", summary(training_data$national_swing), "\n")
  cat("Summary of diff:", summary(training_data$diff), "\n")
  cat("Summary of true_share:", summary(training_data$true_share), "\n")
  cat("Summary of baseline_share:", summary(training_data$baseline_share), "\n")
  
  # Check if the national swing is actually being used in the training data
  cat("National swings used in training data:\n")
  for (election in 1:(n_elections - 1)) {
    cat("Election", election, ":", round(training_elections[[election]]$national_swing, 3), "\n")
  }
  
  
  # Train uniform swing model: national_actual_swing ~ uniform_swing (single coefficient)
  uniform_model <- lm(true_share ~ uniform_swing, data = training_data)
  cat("Uniform model:\n")
  print(summary(uniform_model))
  
  # Train proportional swing model: national_actual_swing ~ proportional_swing (single coefficient)
  proportional_model <- lm(true_share ~ proportional_swing, data = training_data)
  cat("Proportional model:\n")
  print(summary(proportional_model))
  
  # Train mixed swing model: national_actual_swing ~ uniform_swing + proportional_swing
  mixed_model <- lm(true_share ~ uniform_swing + proportional_swing, data = training_data)
  cat("Mixed model:\n")
  print(summary(mixed_model))
  
  # Extract coefficients for prediction
  uniform_coefficients <- coef(uniform_model)
  proportional_coefficients <- coef(proportional_model)
  mixed_coefficients <- coef(mixed_model)
  
  # No estimation error - we train on data generated with same parameters
  # The models learn the true swing patterns from the training data

  

  # Apply trained models to predict district-level vote shares
  # Create national-level polling data for prediction (noisy version of national true vote shares)?
  
  # Get test data
  test_election <- training_elections[[n_elections]]
  
  # Vectorized operations for all districts and parties at once
  # Convert matrices to vectors by rows (district-wise)
  baseline_matrix_long <- as.vector(t(test_election$baseline_matrix))
  true_shares_long <- as.vector(t(test_election$true_vote_shares))
  
  # Create indices for districts and parties (matching the vectorized order)
  district_indices <- rep(1:n_districts, each = n_parties)
  party_indices <- rep(1:n_parties, times = n_districts)
  
  # Calculate n_large_parties for the prediction data
  n_large_parties <- n_parties - n_small_parties
  
  # Calculate true swings
  # true_swings <- (election_data$true_vote_shares %>% colMeans) - (election_data$baseline_matrix %>% colMeans)
  
  # Create the data frame with national-level swing variables
  pred_data <- tibble(
    election = n_elections,  # This is the test election
    district = district_indices,
    party = paste0("P", party_indices),
    baseline_share = baseline_matrix_long,
    true_share = true_shares_long,
    # National-level variables for swing calculations
    national_baseline_share = rep(test_election$baseline, n_districts),
    national_swing = rep(test_election$national_swing, n_districts),
    is_small_party = party_indices > n_large_parties
  ) 
  pred_data$diff <- pred_data$true_share - pred_data$baseline_share
  
  # Create pre-calculated swing variables with single coefficients using national-level data
  pred_data <- pred_data %>%
    mutate(
      # Proportional swing: national_baseline_share * ((national_polling_share - national_baseline_share) / national_baseline_share)
      proportional_swing = national_baseline_share * (1 + national_swing),
      # Uniform swing: national_polling_share - national_baseline_share
      uniform_swing = national_baseline_share + national_swing
    )
  
  # Predict swing using trained models
  pred_data$uniform_pred <- predict(uniform_model, newdata = pred_data)
  pred_data$proportional_pred <- predict(proportional_model, newdata = pred_data)
  pred_data$mixed_pred <- predict(mixed_model, newdata = pred_data)
  
  # Reshape back to district × party matrices
  # pred_uniform <- matrix(pred_data$uniform_pred, nrow = n_districts, ncol = n_parties, byrow = TRUE)
  # pred_prop <- matrix(pred_data$proportional_pred, nrow = n_districts, ncol = n_parties, byrow = TRUE)
  
  # Ensure non-negative and sum to 1
  pred_data <- pred_data %>% 
    group_by(district) %>%
    mutate(
      uniform_pred = pmax(uniform_pred, 0),
      proportional_pred = pmax(proportional_pred, 0),
      mixed_pred = pmax(mixed_pred, 0)
    ) %>%
    ungroup() %>%
    dplyr::select(district, party, uniform_pred, proportional_pred, mixed_pred, national_swing, true_share, baseline_share)

  
  # For each district, determine winner and predictions
  district_results <- pred_data %>%
    group_by(district) %>%
    mutate(
      # Handle ties by taking the first party with max vote share
      # Add safety check for cases where all vote shares are zero
      true_winner = ifelse(all(true_share <= 0), party[1], party[which.max(true_share)]),
      uniform_winner = ifelse(all(uniform_pred <= 0), party[1], party[which.max(uniform_pred)]),
      proportional_winner = ifelse(all(proportional_pred <= 0), party[1], party[which.max(proportional_pred)]),
      mixed_winner = ifelse(all(mixed_pred <= 0), party[1], party[which.max(mixed_pred)]),
      uniform_correct = (uniform_winner == true_winner),
      proportional_correct = (proportional_winner == true_winner),
      mixed_correct = (mixed_winner == true_winner)
    ) %>%
    ungroup() %>% filter(party == true_winner)
  
  # Debug: Check winner distribution and model predictions
  winner_table <- district_results$true_winner %>% table
  cat("Winner distribution:\n")
  print(winner_table)
  
  # Check if models are making different predictions
  cat("Model prediction comparison:\n")
  cat("Uniform correct:", sum(district_results$uniform_correct), "/", nrow(district_results), "\n")
  cat("Proportional correct:", sum(district_results$proportional_correct), "/", nrow(district_results), "\n")
  cat("Mixed correct:", sum(district_results$mixed_correct), "/", nrow(district_results), "\n")
  cat("Same predictions (Uniform vs Proportional):", sum(district_results$uniform_correct == district_results$proportional_correct), "/", nrow(district_results), "\n")
  cat("Same predictions (Uniform vs Mixed):", sum(district_results$uniform_correct == district_results$mixed_correct), "/", nrow(district_results), "\n")
  cat("Same predictions (Proportional vs Mixed):", sum(district_results$proportional_correct == district_results$mixed_correct), "/", nrow(district_results), "\n")
  
  # Calculate accuracy at scenario level
  scenario_accuracy <- district_results %>%
    summarize(
      accuracy_uniform = mean(uniform_correct, na.rm = TRUE),
      accuracy_proportional = mean(proportional_correct, na.rm = TRUE),
      accuracy_mixed = mean(mixed_correct, na.rm = TRUE),
      n_districts = n_distinct(district)
    ) %>%
    mutate(
      party_size = "overall"  # This will be aggregated by scenario
    )
  
  # Calculate MAE and RMSE for vote share predictions
  # We need to get all predictions, not just winners
  all_predictions <- pred_data %>%
    group_by(district) %>%
    mutate(
      # Calculate MAE and RMSE for each district
      mae_uniform = mean(abs(uniform_pred - true_share), na.rm = TRUE),
      mae_proportional = mean(abs(proportional_pred - true_share), na.rm = TRUE),
      mae_mixed = mean(abs(mixed_pred - true_share), na.rm = TRUE),
      rmse_uniform = sqrt(mean((uniform_pred - true_share)^2, na.rm = TRUE)),
      rmse_proportional = sqrt(mean((proportional_pred - true_share)^2, na.rm = TRUE)),
      rmse_mixed = sqrt(mean((mixed_pred - true_share)^2, na.rm = TRUE))
    ) %>%
    ungroup() %>%
    summarize(
      mae_uniform = mean(mae_uniform, na.rm = TRUE),
      mae_proportional = mean(mae_proportional, na.rm = TRUE),
      mae_mixed = mean(mae_mixed, na.rm = TRUE),
      rmse_uniform = mean(rmse_uniform, na.rm = TRUE),
      rmse_proportional = mean(rmse_proportional, na.rm = TRUE),
      rmse_mixed = mean(rmse_mixed, na.rm = TRUE)
    )
  
  # Combine accuracy and error metrics
  scenario_accuracy <- bind_cols(scenario_accuracy, all_predictions)
  
  scenario_accuracy
}

  # simulate_election(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_level_vote_variance, party_system_nationalization, between_district_vote_volatility, n_districts)


  # ========== 3. Run Simulations (Vectorized) ==========
library(future)
library(furrr)
library(progressr)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)

# Set up progress tracking
handlers(global = TRUE)
handlers("progress")

# Vectorized simulation across parameter grid with progress tracking
with_progress({
  p <- progressor(steps = nrow(param_grid))
  
  all_results_df <- future_map_dfr(seq_len(nrow(param_grid)), function(i) {
    # Update progress
    p(sprintf("Scenario %d/%d", i, nrow(param_grid)))
  params <- param_grid[i, ]
  
  scenario_results <- map_dfr(1:n_sim_per_scenario, ~simulate_election(
    params$n_parties, params$n_small_parties, params$n_stronghold_districts, 
    params$stronghold_share, params$national_level_vote_variance, params$between_district_vote_volatility, n_districts, params$uniform_share
  ))
  
  # Add scenario identifier
  scenario_type <- "Artificial"
  
  scenario_results %>%
    mutate(
      n_parties = params$n_parties,
      n_small_parties = params$n_small_parties,
      n_stronghold_districts = params$n_stronghold_districts,
      stronghold_share = params$stronghold_share,
      national_level_vote_variance = params$national_level_vote_variance,
      uniform_share = params$uniform_share,
      between_district_vote_volatility = params$between_district_vote_volatility,
      scenario_type = scenario_type
    )
  }, .options = furrr_options(seed = TRUE))
})


# Clean up parallel processing
plan(sequential)

# ========== 4. Aggregate and Summarize ==========
summary_results <- all_results_df %>%
  group_by(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_level_vote_variance, uniform_share, between_district_vote_volatility, scenario_type) %>%
  summarize(
    accuracy_uniform = mean(accuracy_uniform),
    accuracy_proportional = mean(accuracy_proportional),
    accuracy_mixed = mean(accuracy_mixed),
    mae_uniform = mean(mae_uniform),
    mae_proportional = mean(mae_proportional),
    mae_mixed = mean(mae_mixed),
    rmse_uniform = mean(rmse_uniform),
    rmse_proportional = mean(rmse_proportional),
    rmse_mixed = mean(rmse_mixed),
    n_simulations = n(),
    .groups = "drop"
  )

print(summary_results)

# ========== 5. Visualize ==========

# Accuracy summaries
results_long <- summary_results %>%
  pivot_longer(cols = starts_with("accuracy"), names_to = "model", values_to = "accuracy")

results_long %>% 
  group_by(model) %>% 
  summarize(
    avg_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    n_scenarios = n()
  ) 

# MAE summaries
results_long <- summary_results %>%
  pivot_longer(cols = starts_with("mae"), names_to = "model", values_to = "mae")

results_long %>%
  group_by(model) %>% 
  summarize(
    avg_mae = mean(mae),
    sd_mae = sd(mae),
    n_scenarios = n()
  )

# RMSE summaries
results_long <- summary_results %>%
  pivot_longer(cols = starts_with("rmse"), names_to = "model", values_to = "rmse")

results_long %>%
  group_by(model) %>% 
  summarize(
    avg_rmse = mean(rmse),
    sd_rmse = sd(rmse),
    n_scenarios = n()
  )


# Create heatmap showing when each model performs better
model_comparison <- summary_results %>%
  group_by(n_parties, n_small_parties, uniform_share, between_district_vote_volatility) %>%
  summarize(
    uniform_accuracy = mean(accuracy_uniform),
    proportional_accuracy = mean(accuracy_proportional),
    accuracy_diff = uniform_accuracy - proportional_accuracy,
    uniform_mae = mean(mae_uniform),
    proportional_mae = mean(mae_proportional),
    mae_diff = uniform_mae - proportional_mae,  # Positive = uniform worse
    uniform_rmse = mean(rmse_uniform),
    proportional_rmse = mean(rmse_proportional),
    rmse_diff = uniform_rmse - proportional_rmse,  # Positive = uniform worse
    .groups = "drop"
  )

# Create parameter grid visualization
param_grid_viz <- param_grid %>%
  mutate(
    # Create a unique scenario ID
    scenario_id = row_number(),
    # Create a categorical variable for visualization
    param_combo = paste0("P", n_parties, "_S", n_small_parties, "_U", uniform_share)
  )

# Plot 0: Parameter grid structure
p0 <- ggplot(param_grid_viz, aes(x = n_parties, y = n_small_parties, 
                                 color = factor(uniform_share), 
                                 size = between_district_vote_volatility)) +
  geom_point(alpha = 0.7) +
  scale_color_brewer(palette = "Set1", name = "Uniform Share") +
  scale_size_continuous(name = "Between-District\nVote Volatility", range = c(2, 6)) +
  labs(
    title = "Parameter Grid Structure",
    subtitle = "Each point represents a scenario combination",
    x = "Number of Parties", 
    y = "Number of Small Parties",
    caption = paste("Total scenarios:", nrow(param_grid), 
                   "| Total simulations:", nrow(param_grid) * n_sim_per_scenario)
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Save plot 0
ggsave("figures/01_parameter_grid.png", p0, width = 6, height = 4, dpi = 300)
ggsave("figures/01_parameter_grid.pdf", p0, width = 6, height = 4)
print(p0)

# Plot 1: Heatmap of accuracy difference by number of parties and small parties
p1 <- ggplot(model_comparison, aes(x = n_parties, y = n_small_parties, fill = accuracy_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#56B4E9",
    mid = "white",
    high = "#E69F00",
    midpoint = 0,
    name = "Accuracy Difference\n(Uniform - Proportional)",
    labels = function(x) formatC(x, format = "f", digits = 3)
  ) +
  guides(
    fill = guide_colourbar(
      barwidth = 15,  # increase width
      barheight = 0.8
    )
  ) +
  labs(
    title = "When Does Each Model Perform Better?",
    subtitle = "Blue = Proportional better, Orange = Uniform better",
    x = "Number of Parties", 
    y = "Number of Small Parties"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot 1
ggsave("figures/02_model_performance_heatmap.png", p1, width = 6, height = 4, dpi = 300)
ggsave("figures/02_model_performance_heatmap.pdf", p1, width = 6, height = 4)
print(p1)

# Plot 2: Accuracy difference by swing concentration and district noise
  p2 <- ggplot(model_comparison, aes(x = uniform_share, y = between_district_vote_volatility, fill = accuracy_diff)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#56B4E9",
    mid = "white",
    high = "#E69F00",
    midpoint = 0,
    name = "Accuracy Difference\n(Uniform - Proportional)",
    labels = function(x) formatC(x, format = "f", digits = 3)
  ) +
  guides(
    fill = guide_colourbar(
      barwidth = 15,  # increase width
      barheight = 0.8
    )
  ) +
  labs(
    title = "Effect of Uniform Share and Between-District Vote Volatility",
    subtitle = "Blue = Proportional better, Orange = Uniform better",
          x = "Uniform Share", 
    y = "Between-District Vote Volatility"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot 2
  ggsave("figures/03_uniform_share_heatmap.png", p2, width = 6, height = 4, dpi = 300)
  ggsave("figures/03_uniform_share_heatmap.pdf", p2, width = 6, height = 4)
print(p2)

# Plot 3: Line plot showing the key interaction from regression
p3 <- ggplot(model_comparison, aes(
  x = n_parties,
  y = accuracy_diff,
  color = factor(n_small_parties),
  shape = factor(n_small_parties)
)) +
  geom_point(size = 2, position = position_jitter(width = 0.15, height = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_brewer(palette = "Set1", name = "Small Parties") +
  scale_shape_manual(values = 1:5, name = "Small Parties") +
  labs(
    title = "Effect of Number of Parties and Number of Small Parties",
    subtitle = "Positive = Uniform better, Negative = Proportional better",
    x = "Number of Parties", 
    y = "Accuracy Difference (Uniform - Proportional)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot 3
ggsave("figures/04_key_interaction.png", p3, width = 6, height = 4, dpi = 300)
ggsave("figures/04_key_interaction.pdf", p3, width = 6, height = 4)
print(p3)


# ========== 6. Regression Analysis ==========
# Prepare data for regression analysis at SCENARIO level
regression_data <- summary_results %>%
  mutate(
    # Performance difference (positive = uniform better, negative = proportional better)
    performance_diff = accuracy_uniform - accuracy_proportional,
    
    # Relative performance (percentage improvement of proportional over uniform)
    # Handle division by zero: if accuracy_uniform is 0, set relative_performance to 0
    relative_performance = case_when(
      accuracy_uniform == 0 ~ 0,
      TRUE ~ (accuracy_proportional - accuracy_uniform) / accuracy_uniform * 100
    ),
    
    # MAE and RMSE differences (positive = uniform worse, negative = proportional worse)
    mae_diff = mae_uniform - mae_proportional,
    rmse_diff = rmse_uniform - rmse_proportional,
    
    # Relative MAE and RMSE improvements
    # Handle division by zero: if mae_uniform or rmse_uniform is 0, set to 0
    relative_mae_improvement = case_when(
      mae_uniform == 0 ~ 0,
      TRUE ~ (mae_proportional - mae_uniform) / mae_uniform * 100
    ),
    relative_rmse_improvement = case_when(
      rmse_uniform == 0 ~ 0,
      TRUE ~ (rmse_proportional - rmse_uniform) / rmse_uniform * 100
    ),
    
    # Scenario characteristics only
    stronghold_ratio = n_stronghold_districts / n_districts,  # Proportion of stronghold districts
    
    # Handle log of zero or negative values by adding small constant
    log_national_level_vote_variance = log(national_level_vote_variance + 1e-1),
    log_between_district_vote_volatility = log(between_district_vote_volatility + 1e-1),
    log_n_parties = log(n_parties + 1e-1),
    log_n_small_parties = log(n_small_parties + 1e-1),
    
    # Interaction terms
    small_stronghold_ratio = log_n_small_parties * stronghold_ratio,
    small_swing = log_n_small_parties * log_national_level_vote_variance,
    uniform_effect = uniform_share * log_n_small_parties,  # Interaction of uniform share and small parties
    volatility_effect = between_district_vote_volatility * uniform_share  # Interaction of between-district volatility and uniform share
  ) %>%
  # Remove any rows with NA, NaN, or Inf values
  filter(!is.na(performance_diff) & !is.infinite(performance_diff),
         !is.na(mae_diff) & !is.infinite(mae_diff),
         !is.na(rmse_diff) & !is.infinite(rmse_diff),
         !is.na(log_national_level_vote_variance) & !is.infinite(log_national_level_vote_variance),
         !is.na(log_between_district_vote_volatility) & !is.infinite(log_between_district_vote_volatility),
         !is.na(log_n_parties) & !is.infinite(log_n_parties),
         !is.na(log_n_small_parties) & !is.infinite(log_n_small_parties))

# (n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_level_vote_variance, between_district_vote_volatility, n_districts, uniform_share

# Check data quality before regression
cat("Data quality check:\n")
cat("Original summary_results rows:", nrow(summary_results), "\n")
cat("Regression_data rows after cleaning:", nrow(regression_data), "\n")
cat("Rows removed due to NA/Inf:", nrow(summary_results) - nrow(regression_data), "\n")

# Check for any remaining issues
cat("\nChecking for remaining NA/Inf values:\n")
cat("NA in performance_diff:", sum(is.na(regression_data$performance_diff)), "\n")
cat("NA in mae_diff:", sum(is.na(regression_data$mae_diff)), "\n")
cat("NA in rmse_diff:", sum(is.na(regression_data$rmse_diff)), "\n")
cat("NA in log_national_level_vote_variance:", sum(is.na(regression_data$log_national_level_vote_variance)), "\n")

# Main regression: What explains the accuracy difference?
model1 <- lm(performance_diff ~ log_n_parties + log_n_small_parties + stronghold_ratio + log_national_level_vote_variance +
             uniform_share + log_between_district_vote_volatility + small_stronghold_ratio, data = regression_data)

print("Regression 1: Accuracy difference (uniform - proportional)")
print(summary(model1))

# MAE difference regression
model3 <- lm(mae_diff ~ log_n_parties + log_n_small_parties + stronghold_ratio + log_national_level_vote_variance +
             uniform_share + log_between_district_vote_volatility + small_stronghold_ratio, data = regression_data)

print("Regression 3: MAE difference (uniform - proportional)")
print(summary(model3))

# RMSE difference regression
model4 <- lm(rmse_diff ~ log_n_parties + log_n_small_parties + stronghold_ratio + log_national_level_vote_variance +
             uniform_share + log_between_district_vote_volatility + small_stronghold_ratio, data = regression_data)

print("Regression 4: RMSE difference (uniform - proportional)")
print(summary(model4))

# Create coefficient plot for the main regression
library(broom)

# Get coefficient data for all models
coef_data <- bind_rows(
  tidy(model1, conf.int = TRUE) %>% mutate(model = "Accuracy Difference"),
  tidy(model3, conf.int = TRUE) %>% mutate(model = "MAE Difference"),
  tidy(model4, conf.int = TRUE) %>% mutate(model = "RMSE Difference")
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "log_n_parties" ~ "Log(Number of Parties)",
      term == "log_n_small_parties" ~ "Log(Number of Small Parties)",
      term == "stronghold_ratio" ~ "Stronghold Ratio",
      term == "log_national_level_vote_variance" ~ "Log(National Level Vote Variance)",
          term == "uniform_share" ~ "Uniform Share",
    term == "log_between_district_vote_volatility" ~ "Log(Between-District Vote Volatility)",
      term == "small_stronghold_ratio" ~ "Small Party Stronghold Ratio",
      TRUE ~ term
    ),
    # Order by absolute effect size
    abs_estimate = abs(estimate),
    # For MAE and RMSE, positive means uniform worse, so we need to flip the interpretation
    model_label = case_when(
      model == "Accuracy Difference" ~ "Accuracy (Positive = Uniform Better)",
      model == "MAE Difference" ~ "MAE (Positive = Uniform Worse)",
      model == "RMSE Difference" ~ "RMSE (Positive = Uniform Worse)"
    )
  ) %>%
  arrange(desc(abs_estimate))

# Coefficient plot
p4 <- ggplot(coef_data, aes(x = estimate, y = reorder(term, abs_estimate), color = model_label, shape = model_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 2, position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.3)) +
  labs(
    # title = "Regression Coefficients: What Explains Model Performance Differences?",
    subtitle = "Comparing Accuracy, MAE, and RMSE differences between models",
    x = "Coefficient Estimate",
    y = NULL,
    color = "Outcome",
    shape = "Outcome",
    caption = "Error bars show 95% confidence intervals"
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#56B4E9")) +
  scale_shape_manual(values = c(16, 17, 15)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )

# Save plot 4
ggsave("figures/05_regression_coefficients.png", p4, width = 6, height = 4, dpi = 300)
ggsave("figures/05_regression_coefficients.pdf", p4, width = 6, height = 4)
print(p4)

# First Difference Plots (using existing regression results)
cat("\n=== FIRST DIFFERENCE PLOTS ===\n")

# Calculate variable ranges for first differences
variable_ranges <- summary_results %>%
  summarize(
    log_n_parties = max(log(n_parties)) - min(log(n_parties)),
    log_n_small_parties = max(log(n_small_parties + 1)) - min(log(n_small_parties + 1)),
    stronghold_ratio = max(stronghold_share) - min(stronghold_share),
    log_national_level_vote_variance = max(log(national_level_vote_variance)) - min(log(national_level_vote_variance)),
    uniform_share = max(uniform_share) - min(uniform_share),
    log_between_district_vote_volatility = max(log(between_district_vote_volatility)) - min(log(between_district_vote_volatility)),
    small_stronghold_ratio = max(stronghold_share * (n_small_parties / n_parties)) - min(stronghold_share * (n_small_parties / n_parties))
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "range")

# Calculate first differences for each model
first_diff_data <- bind_rows(
  # Model 1: Accuracy difference
  data.frame(
    model = "Accuracy Difference (Uniform - Proportional)",
    variable = variable_ranges$variable,
    first_diff = coef(model1)[variable_ranges$variable] * variable_ranges$range,
    coefficient = coef(model1)[variable_ranges$variable],
    range = variable_ranges$range
  ),
  # Model 3: MAE difference  
  data.frame(
    model = "MAE Difference (Uniform - Proportional)",
    variable = variable_ranges$variable,
    first_diff = coef(model3)[variable_ranges$variable] * variable_ranges$range,
    coefficient = coef(model3)[variable_ranges$variable],
    range = variable_ranges$range
  ),
  # Model 4: RMSE difference
  data.frame(
    model = "RMSE Difference (Uniform - Proportional)",
    variable = variable_ranges$variable,
    first_diff = coef(model4)[variable_ranges$variable] * variable_ranges$range,
    coefficient = coef(model4)[variable_ranges$variable],
    range = variable_ranges$range
  )
) %>%
  mutate(
    variable_clean = case_when(
      variable == "log_n_parties" ~ "Number of Parties (log)",
      variable == "log_n_small_parties" ~ "Number of Small Parties (log)",
      variable == "stronghold_ratio" ~ "Stronghold Share",
      variable == "log_national_level_vote_variance" ~ "National Vote Variance (log)",
      variable == "uniform_share" ~ "Uniform Share",
      variable == "log_between_district_vote_volatility" ~ "District Vote Volatility (log)",
      variable == "small_stronghold_ratio" ~ "Small Party Stronghold Ratio"
    )
  )

# Plot first differences
first_diff_plot <- ggplot(first_diff_data, aes(x = variable_clean, y = first_diff, fill = model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", first_diff)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  labs(title = "First Differences: Actual Impact on Model Performance",
       subtitle = "Shows the actual change in performance when moving from min to max of each variable",
       x = "Predictor Variables",
       y = "First Difference (Actual Performance Change)",
       fill = "Performance Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()

print(first_diff_plot)
ggsave("figures/06_first_differences.pdf", first_diff_plot, width = 12, height = 8)
ggsave("figures/06_first_differences.png", first_diff_plot, width = 12, height = 8)
cat("✓ First differences plot saved to figures/06_first_differences.pdf and .png\n")

# Summary table of first differences
cat("\nFirst Differences Summary:\n")
first_diff_summary <- first_diff_data %>%
  group_by(model) %>%
  arrange(desc(abs(first_diff))) %>%
  dplyr::select(model, variable_clean, first_diff, coefficient, range)
print(first_diff_summary)

# Separate regressions by number of small parties
few_small_data <- regression_data %>% filter(n_small_parties <= 2)
many_small_data <- regression_data %>% filter(n_small_parties >= 3)

model_few_small <- lm(performance_diff ~ stronghold_ratio + log_national_level_vote_variance,
                      data = few_small_data)
model_many_small <- lm(performance_diff ~ stronghold_ratio + log_national_level_vote_variance,
                       data = many_small_data)

print("Regression 3: Few small parties (≤2)")
print(summary(model_few_small))

print("Regression 4: Many small parties (≥3)")
print(summary(model_many_small))

# ========== 7. Key Insights Summary ==========
cat("\n=== KEY INSIGHTS ===\n")
cat("1. Average performance differences by number of small parties:\n")
regression_data %>%
  group_by(n_small_parties) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_mae_diff = mean(mae_diff),
    avg_rmse_diff = mean(rmse_diff),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    avg_mae_uniform = mean(mae_uniform),
    avg_mae_proportional = mean(mae_proportional),
    avg_rmse_uniform = mean(rmse_uniform),
    avg_rmse_proportional = mean(rmse_proportional),
    n = n()
  ) %>%
  print()

cat("\n2. Effect of stronghold ratio on performance:\n")
regression_data %>%
  group_by(stronghold_ratio) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_mae_diff = mean(mae_diff),
    avg_rmse_diff = mean(rmse_diff),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    avg_mae_uniform = mean(mae_uniform),
    avg_mae_proportional = mean(mae_proportional),
    avg_rmse_uniform = mean(rmse_uniform),
    avg_rmse_proportional = mean(rmse_proportional),
    n = n()
  ) %>%
  print()

cat("\n3. Effect of swing volatility:\n")
regression_data %>%
  group_by(national_level_vote_variance) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_mae_diff = mean(mae_diff),
    avg_rmse_diff = mean(rmse_diff),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    avg_mae_uniform = mean(mae_uniform),
    avg_mae_proportional = mean(mae_proportional),
    avg_rmse_uniform = mean(rmse_uniform),
    avg_rmse_proportional = mean(rmse_proportional),
    n = n()
  ) %>%
  print()

cat("\n4. Effect of district noise:\n")
regression_data %>%
  group_by(between_district_vote_volatility) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_mae_diff = mean(mae_diff),
    avg_rmse_diff = mean(rmse_diff),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    avg_mae_uniform = mean(mae_uniform),
    avg_mae_proportional = mean(mae_proportional),
    avg_rmse_uniform = mean(rmse_uniform),
    avg_rmse_proportional = mean(rmse_proportional),
    n = n()
  ) %>%
  print()

# ========== 8. Compare with Empirical German Results ==========
cat("\n=== COMPARISON WITH EMPIRICAL GERMAN RESULTS ===\n")

# Load empirical results
empirical_results <- read.csv("data/out/eval_aggregate.csv")
empirical_uniform <- empirical_results$avg_accuracy_winner[empirical_results$name == "uniform"]
empirical_proportional <- empirical_results$avg_accuracy_winner[empirical_results$name == "proportional second vote"]

cat("Empirical German Results (299 districts, 8 elections):\n")
cat("- Uniform swing accuracy:", round(empirical_uniform, 3), "\n")
cat("- Proportional swing accuracy:", round(empirical_proportional, 3), "\n")
cat("- Difference (Proportional - Uniform):", round(empirical_proportional - empirical_uniform, 3), "\n")

# Calculate simulation performance metrics
simulation_performance <- summary_results %>%
  summarize(
    avg_accuracy_uniform = mean(accuracy_uniform, na.rm = T),
    avg_accuracy_proportional = mean(accuracy_proportional, na.rm = T),
    avg_mae_uniform = mean(mae_uniform, na.rm = T),
    avg_mae_proportional = mean(mae_proportional, na.rm = T),
    avg_rmse_uniform = mean(rmse_uniform, na.rm = T),
    avg_rmse_proportional = mean(rmse_proportional, na.rm = T),
    avg_performance_diff = mean(accuracy_uniform - accuracy_proportional, na.rm = T),
    avg_mae_diff = mean(mae_uniform - mae_proportional, na.rm = T),
    avg_rmse_diff = mean(rmse_uniform - rmse_proportional, na.rm = T),
    n = n()
  )

cat("\nSimulation Results (averaged across scenarios):\n")
print(simulation_performance)

# Compare overall performance
cat("\n=== SIMULATION VS EMPIRICAL COMPARISON ===\n")
cat("Empirical: Both models perform similarly (difference:", round(empirical_proportional - empirical_uniform, 3), ")\n")
cat("Simulation: Check if models perform similarly or if one dominates\n")

# Check if simulation matches empirical pattern
overall_uniform_accuracy <- mean(summary_results$accuracy_uniform, na.rm = T)
overall_proportional_accuracy <- mean(summary_results$accuracy_proportional, na.rm = T)
simulation_diff <- overall_proportional_accuracy - overall_uniform_accuracy

cat("Simulation overall difference (Proportional - Uniform accuracy):", round(simulation_diff, 4), "\n")
if (abs(simulation_diff) < 0.01) {
  cat("✓ Simulation matches empirical pattern: both models perform similarly\n")
} else if (simulation_diff > 0) {
  cat("✗ Simulation shows proportional better (unlike empirical)\n")
} else {
  cat("✗ Simulation shows uniform better (unlike empirical)\n")
}

# ========== 9. Save Results and Create Publication Outputs ==========
cat("\n=== SAVING RESULTS AND CREATING OUTPUTS ===\n")

# Create output directories
dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create("data/out", showWarnings = FALSE, recursive = TRUE)

# Save the main results dataframe
write.csv(all_results_df, "data/out/simulation_results.csv", row.names = FALSE)
saveRDS(all_results_df, "data/out/simulation_results.rds")

# Save summary results
write.csv(summary_results, "data/out/simulation_summary.csv", row.names = FALSE)
saveRDS(summary_results, "data/out/simulation_summary.rds")

# Save regression data
write.csv(regression_data, "data/out/regression_data.csv", row.names = FALSE)
saveRDS(regression_data, "data/out/regression_data.rds")

# Save model comparison data
write.csv(model_comparison, "data/out/model_comparison.csv", row.names = FALSE)
saveRDS(model_comparison, "data/out/model_comparison.rds")

cat("✓ Results saved to data/out/\n")



# Create LaTeX tables using stargazer
library(stargazer)

# Table 1: Summary statistics
# stargazer(summary_results, 
#           type = "latex",
#           title = "Summary Statistics by Scenario",
#           out = "tables/summary_table.tex",
#           digits = 3,
#           summary = FALSE)

# Table 2: Regression results
stargazer(model1, model2, model3, model4,
          type = "latex",
          title = "Regression Results: Explaining Model Performance Differences",
          column.labels = c("Accuracy Diff", "Rel. Performance", "MAE Diff", "RMSE Diff"),
          dep.var.labels = c("Uniform - Proportional", "Proportional Improvement (%)", "Uniform - Proportional", "Uniform - Proportional"),
          out = "tables/regression_table.tex",
          digits = 4,
          style = "aer")

# Table 3: Model comparison by key parameters
model_comparison_table <- model_comparison %>%
  group_by(n_parties, n_small_parties) %>%
  summarize(
    n_scenarios = n(),
    # Accuracy
    uniform_better_acc = sum(accuracy_diff > 0),
    proportional_better_acc = sum(accuracy_diff < 0),
    avg_accuracy_diff = mean(accuracy_diff),
    # MAE
    uniform_better_mae = sum(mae_diff < 0),  # Lower MAE is better
    proportional_better_mae = sum(mae_diff > 0),
    avg_mae_diff = mean(mae_diff),
    # RMSE
    uniform_better_rmse = sum(rmse_diff < 0),  # Lower RMSE is better
    proportional_better_rmse = sum(rmse_diff > 0),
    avg_rmse_diff = mean(rmse_diff),
    .groups = "drop"
  ) %>%
  mutate(
    better_model_acc = case_when(
      uniform_better_acc > proportional_better_acc ~ "Uniform",
      proportional_better_acc > uniform_better_acc ~ "Proportional",
      TRUE ~ "Equal"
    ),
    better_model_mae = case_when(
      uniform_better_mae > proportional_better_mae ~ "Uniform",
      proportional_better_mae > uniform_better_mae ~ "Proportional",
      TRUE ~ "Equal"
    ),
    better_model_rmse = case_when(
      uniform_better_rmse > proportional_better_rmse ~ "Uniform",
      proportional_better_rmse > uniform_better_rmse ~ "Proportional",
      TRUE ~ "Equal"
    )
  )

stargazer(model_comparison_table,
          type = "latex",
          title = "Model Performance by Party Configuration",
          out = "tables/model_comparison_table.tex",
          digits = 3,
          summary = FALSE)

cat("✓ LaTeX tables saved to tables/\n")

# Create a summary report
sink("data/out/simulation_report.txt")
cat("SWING MODEL SIMULATION REPORT\n")
cat("============================\n\n")
cat("Date:", Sys.Date(), "\n")
cat("Total scenarios:", nrow(summary_results), "\n")
cat("Total simulations:", nrow(all_results_df), "\n\n")

cat("KEY FINDINGS:\n")
cat("-------------\n")
cat("1. Number of parties:", round(coef(model1)["log_n_parties"], 4), "effect on uniform advantage\n")
cat("2. Number of small parties:", round(coef(model1)["log_n_small_parties"], 4), "effect on proportional advantage\n")
cat("3. R-squared:", round(summary(model1)$r.squared, 3), "\n\n")

cat("SCENARIOS WHERE PROPORTIONAL WINS:\n")
cat("----------------------------------\n")
print(proportional_favorable %>%
  summarize(
    n_scenarios = n(),
    avg_n_small_parties = mean(n_small_parties),
    avg_national_level_vote_variance = mean(national_level_vote_variance),
    avg_stronghold_share = mean(stronghold_share)
  ))

cat("\nSCENARIOS WHERE UNIFORM WINS:\n")
cat("-----------------------------\n")
print(uniform_favorable %>%
  summarize(
    n_scenarios = n(),
    avg_n_small_parties = mean(n_small_parties),
    avg_national_level_vote_variance = mean(national_level_vote_variance),
    avg_stronghold_share = mean(stronghold_share)
  ))

sink()
cat("✓ Report saved to data/out/simulation_report.txt\n")

