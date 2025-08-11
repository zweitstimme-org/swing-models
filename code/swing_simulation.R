# swing_simulation.R
# Simplified simulation focusing on small parties with strongholds
# Tests whether proportional swing works better for clustered small parties

library(tidyverse)
library(MASS)

# ========== 1. Define Simplified Parameter Grid ==========
set.seed(123)

n_districts <- 100
n_sim_per_scenario <- 1

# Simplified parameter grid focusing on small party clustering
param_grid <- expand.grid(
  n_parties = c(4, 5, 6, 7),  # At least 1 large party
  n_small_parties = c(2, 4, 6),  # More small parties to test proportional
  n_stronghold_districts = c(10, 25, 40),  # How many districts have high support
  stronghold_share = c(0.3, 0.45, 0.60),   # Vote share in stronghold districts
  national_swing_variance = c(0.03, 0.05, 0.07),  # National-level swing volatility (increased)
  swing_concentration = c(0.0, 1.0, 2, 4),  # Degree of swing concentration (more realistic values)
  district_noise_sd = c(0.002, 0.005, 0.01),  # District-level noise variation
  stringsAsFactors = FALSE
)

# Filter out invalid combinations (n_small_parties cannot exceed n_parties - 1)
param_grid <- param_grid %>%
  filter(n_small_parties <= n_parties - 1)

# Load extracted German parameters
load("data/german_params.RData")

# Add realistic German scenario using extracted parameters
german_scenario <- data.frame(
  n_parties = length(german_params$large_parties) + length(german_params$small_parties),
  n_small_parties = length(german_params$small_parties),
  n_stronghold_districts = round(german_params$stronghold_ratio * 100) + 1,
  stronghold_share = 0.15,  # Threshold for stronghold definition
  national_swing_variance = german_params$swing_sd %>% round(2),  # Extracted German swing volatility
  swing_concentration = 0.8,  # German elections show moderate swing concentration
  district_noise_sd = 0.005,  # Moderate district-level noise for German scenario
  stringsAsFactors = FALSE
)


cat("German parameters loaded:\n")
cat("- Number of districts:", german_params$n_districts, "\n")
cat("- Large parties:", paste(german_params$large_parties, collapse = ", "), "\n")
cat("- Small parties:", paste(german_params$small_parties, collapse = ", "), "\n")
cat("- Swing volatility (SD):", round(german_params$swing_sd, 3), "\n")
cat("- Stronghold ratio:", round(german_params$stronghold_ratio, 3), "\n")

param_grid <- rbind(param_grid, german_scenario)

# Helper: generate baseline shares with small party strongholds
make_baseline_with_strongholds <- function(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, n_districts) {
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
  small_baseline <- rep(0.15, n_small_parties)  # Increased from 0.12 to 0.15
  
  baseline_shares <- c(large_baseline, small_baseline)
  baseline_shares <- baseline_shares / sum(baseline_shares)  # Normalize
  
  # Create district-specific baseline matrix
  baseline_matrix <- matrix(NA, nrow = n_districts, ncol = n_parties)
  
  # Large parties: more variation around national baseline (vectorized)
  if (n_large_parties > 0) {
    baseline_matrix[, 1:n_large_parties] <- matrix(
      rnorm(n_districts * n_large_parties, 
            mean = rep(baseline_shares[1:n_large_parties], each = n_districts), 
            sd = 0.08),  # Increased from 0.03 to 0.08 for more variation
      nrow = n_districts, ncol = n_large_parties
    )
  }
  
  # Small parties: strongholds + low support elsewhere (vectorized)
  if (n_small_parties > 0) {
    small_party_cols <- (n_large_parties + 1):n_parties
    
      # Check if this is the German scenario (using extracted parameters)
  if (n_parties == length(german_params$large_parties) + length(german_params$small_parties) && 
      n_small_parties == length(german_params$small_parties)) {
    # German scenario: use extracted baseline ranges
    small_baseline_mean <- mean(german_params$small_baseline_range)
    small_baseline_sd <- (german_params$small_baseline_range[2] - german_params$small_baseline_range[1]) / 4
    
    baseline_matrix[, small_party_cols] <- rnorm(n_districts * n_small_parties, 
                                                 mean = small_baseline_mean, 
                                                 sd = small_baseline_sd) %>%
      matrix(nrow = n_districts, ncol = n_small_parties)
    
    # Use extracted geographic patterns
    for (p in small_party_cols) {
      # Create gradual variation based on German patterns
      geographic_factor <- rnorm(n_districts, mean = 1, sd = 0.3)
      baseline_matrix[, p] <- baseline_matrix[, p] * pmax(geographic_factor, 0.3)
    }
  } else {
      # Artificial scenarios: more realistic small party support
      baseline_matrix[, small_party_cols] <- 0.12  # Increased from 0.10 to 0.12
      
        # Add strongholds for each small party (more gradual variation)
  for (p in small_party_cols) {
    stronghold_districts <- sample(1:n_districts, n_stronghold_districts)
    # More gradual stronghold effect (not extreme)
    baseline_matrix[stronghold_districts, p] <- 0.10 + stronghold_share * 0.2  # More gradual increase
    
    # Add some gradual geographic variation (like real German parties)
    geographic_variation <- rnorm(n_districts, mean = 1, sd = 0.15)
    baseline_matrix[, p] <- baseline_matrix[, p] * pmax(geographic_variation, 0.7)
      }
    }
  }
  
  # Normalize each district to sum to 1
  baseline_matrix <- t(apply(baseline_matrix, 1, function(x) x / sum(x)))
  
  # Debug: Check baseline distribution
  if (FALSE) {  # Set to TRUE to debug
    cat("Baseline shares:", round(baseline_shares, 3), "\n")
    cat("Baseline matrix summary:\n")
    print(apply(baseline_matrix, 2, summary))
  }
  
  list(baseline_matrix = baseline_matrix, baseline_shares = baseline_shares)
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
simulate_election <- function(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_swing_variance, swing_concentration, district_noise_sd, n_districts) {
  # Generate training data (more elections with same parameters)
  n_training_elections <- 4  # Number of historical elections to train on
  
  # Generate training elections (historical data with same parameters)
  training_elections <- list()
  for (election in 1:n_training_elections) {
    # Generate baseline for this training election (same parameters)
    training_baseline_data <- make_baseline_with_strongholds(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, n_districts)
    training_baseline_matrix <- training_baseline_data$baseline_matrix
    training_baseline_shares <- training_baseline_data$baseline_shares
    colnames(training_baseline_matrix) <- paste0("P", 1:n_parties)
    
    # Generate national swing for training election
    training_national_swing <- rnorm(n_parties, mean = 0, sd = national_swing_variance)
    
    # Generate district-level swing with realistic variation
    # Make district swings much smaller than national swing to preserve the relationship
    district_swing_sd_large <- 0.001  # Reduced from 0.015 to 0.001
    district_swing_sd_small <- 0.0005  # Reduced from 0.012 to 0.0005
    training_district_swings <- matrix(0, nrow = n_districts, ncol = n_parties)
    
    n_large_parties <- n_parties - n_small_parties
    if (n_large_parties > 0) {
      training_district_swings[, 1:n_large_parties] <- matrix(
        rnorm(n_districts * n_large_parties, 0, district_swing_sd_large),
        nrow = n_districts, ncol = n_large_parties
      )
    }
    if (n_small_parties > 0) {
      small_party_cols <- (n_large_parties + 1):n_parties
      training_district_swings[, small_party_cols] <- matrix(
        rnorm(n_districts * n_small_parties, 0, district_swing_sd_small),
        nrow = n_districts, ncol = n_small_parties
      )
    }
    
    # Apply proportional effects to training district-level swings (only in strongholds)
    if (swing_concentration > 0) {
      for (i in 1:n_parties) {
        # Identify stronghold districts for this party (districts with high baseline)
        stronghold_threshold <- quantile(training_baseline_matrix[, i], 0.8)  # Top 20% of districts
        stronghold_districts <- which(training_baseline_matrix[, i] >= stronghold_threshold)
        non_stronghold_districts <- which(training_baseline_matrix[, i] < stronghold_threshold)
        
        # Apply proportional effects only to stronghold districts
        if (length(stronghold_districts) > 0) {
          training_district_swings[stronghold_districts, i] <- training_district_swings[stronghold_districts, i] * 
            (1 + training_baseline_matrix[stronghold_districts, i] * swing_concentration * 5.0)  # Increased from 2.0 to 5.0
        }
        
        # Compensate in non-stronghold districts to maintain national swing
        if (length(non_stronghold_districts) > 0) {
          # Calculate how much the stronghold effect changed the national swing
          stronghold_effect <- sum(training_district_swings[stronghold_districts, i]) - 
            sum(training_district_swings[stronghold_districts, i] / (1 + training_baseline_matrix[stronghold_districts, i] * swing_concentration * 5.0))
          
          # Distribute the compensation across non-stronghold districts
          compensation_per_district <- -stronghold_effect / length(non_stronghold_districts)
          training_district_swings[non_stronghold_districts, i] <- training_district_swings[non_stronghold_districts, i] + compensation_per_district
        }
      }
    }
    
        # True vote shares for training election - apply national swing with proportional effects
    # Start with baseline
    training_true_vote_shares <- training_baseline_matrix
    
    # Apply national swing with uniform component + proportional effect
    for (i in 1:n_parties) {
      # National swing for this party (same across all districts)
      national_swing_for_party <- training_national_swing[i]
      
      # Always apply uniform swing component
      uniform_swing <- national_swing_for_party
      
      # Add proportional effect on top (can be 0 when swing_concentration = 0)
      # proportional_component <- national_swing_for_party * training_baseline_matrix[, i] * swing_concentration
      # total_swing_effect <- uniform_swing + proportional_component
      
      # Calculate the expected vote share after national swing
      expected_vote_share <- training_baseline_matrix[, i] + uniform_swing
      
      # Calculate the actual swing (difference between true and baseline)
      actual_swing <- uniform_swing
      
      # Use the district swings we already generated (with proportional effects applied)
      district_variation <- training_district_swings[, i]
      
      # Add district variation to the swing
      actual_swing <- actual_swing + district_variation
      
      # Calculate final vote share
      training_true_vote_shares[, i] <- training_baseline_matrix[, i] + actual_swing
      
      # Debug: Check if national swing is being applied correctly
      if (i == 1) {  # Only for first party to avoid spam
        cat("Party", i, "national swing:", round(national_swing_for_party, 3), 
            "total_swing_effect mean:", round(mean(total_swing_effect), 3),
            "actual_swing mean:", round(mean(actual_swing), 3), "\n")
      }
    }
    
    # Add final noise to make prediction task harder
    training_random_noise <- matrix(rnorm(n_districts * n_parties, 0, 0.01), nrow = n_districts, ncol = n_parties)
    training_true_vote_shares <- training_true_vote_shares + training_random_noise
    
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
  
  # Now generate the current election (same parameters, different realization)
  n_large_parties <- n_parties - n_small_parties
  
  # Generate baseline for current election (slightly different from training)
  baseline_data <- make_baseline_with_strongholds(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, n_districts)
  baseline_matrix <- baseline_data$baseline_matrix
  baseline_shares <- baseline_data$baseline_shares
  
  colnames(baseline_matrix) <- paste0("P", 1:n_parties)
  
  # Train swing coefficients using regression formulas
  # Create long format training data with one row per district-party-election (vectorized)
  training_data <- map_dfr(1:n_training_elections, function(election) {
    election_data <- training_elections[[election]]
    
    # Vectorized operations for all districts and parties at once
    # Convert matrices to vectors by rows (district-wise)
    baseline_matrix_long <- as.vector(t(election_data$baseline_matrix))
    true_shares_long <- as.vector(t(election_data$true_vote_shares))
    
    # Create indices for districts and parties (matching the vectorized order)
    district_indices <- rep(1:n_districts, each = n_parties)
    party_indices <- rep(1:n_parties, times = n_districts)
    


    # Create the data frame
    tibble(
      election = election,
      district = district_indices,
      party = paste0("P", party_indices),
      baseline_share = baseline_matrix_long,
      true_share = true_shares_long,
      is_small_party = party_indices > n_large_parties
    ) %>% 
    # Calculate the party level swing (which is the average difference between true and baseline and then add the sd)
      group_by(party, election) %>% 
      mutate(swing = mean(true_share - baseline_share) + rnorm(n_districts, 0, national_swing_variance)
             ) %>% ungroup
    
  })
  
  training_data$diff <- training_data$true_share - training_data$baseline_share
  # training_data %>% View
  cor(dplyr::select(training_data, c(diff, swing)))
  
  # Debug: Check the relationship between swing and diff
  cat("Correlation between diff and swing:", cor(training_data$diff, training_data$swing), "\n")
  cat("Summary of swing:", summary(training_data$swing), "\n")
  cat("Summary of diff:", summary(training_data$diff), "\n")
  cat("Summary of true_share:", summary(training_data$true_share), "\n")
  cat("Summary of baseline_share:", summary(training_data$baseline_share), "\n")
  
  # Check if the national swing is actually being used in the training data
  cat("National swings used in training data:\n")
  for (election in 1:n_training_elections) {
    cat("Election", election, ":", round(training_elections[[election]]$national_swing, 3), "\n")
  }
  
  
  # Train uniform swing model: true_share ~ baseline_share + swing
  uniform_model <- lm(true_share ~ baseline_share + swing, data = training_data)
  cat("Uniform model:\n")
  print(summary(uniform_model))
  
  # Train proportional swing model: true_share ~ baseline_share * swing (interaction)
  proportional_model <- lm(true_share ~ baseline_share * swing, data = training_data)
  cat("Proportional model:\n")
  print(summary(proportional_model))
  
  # Extract coefficients for prediction
  uniform_coefficients <- coef(uniform_model)
  proportional_coefficients <- coef(proportional_model)
  
  # No estimation error - we train on data generated with same parameters
  # The models learn the true swing patterns from the training data

  # Simulate swing with concentration parameter
  # Base swing (uniform component)
  national_swing <- rnorm(n_parties, mean = 0, sd = national_swing_variance)
  
  # District-level swing (vectorized) - controlled by district_noise_sd parameter
  # Make district swings much smaller than national swing to preserve the relationship
  district_swing_sd_large <- district_noise_sd * 0.1  # Reduced from 1.5 to 0.1
  district_swing_sd_small <- district_noise_sd * 0.05  # Reduced from 1.0 to 0.05
  
  district_swings <- matrix(0, nrow = n_districts, ncol = n_parties)
  
  # Large parties: more district variation
  if (n_large_parties > 0) {
    district_swings[, 1:n_large_parties] <- matrix(
      rnorm(n_districts * n_large_parties, 0, district_swing_sd_large),
      nrow = n_districts, ncol = n_large_parties
    )
  }
  
  # Small parties: moderate district variation
  if (n_small_parties > 0) {
    small_party_cols <- (n_large_parties + 1):n_parties
    district_swings[, small_party_cols] <- matrix(
      rnorm(n_districts * n_small_parties, 0, district_swing_sd_small),
      nrow = n_districts, ncol = n_small_parties
    )
  }
  
  # Apply proportional effects to district-level swings (only in strongholds)
    for (i in 1:n_parties) {
      # Identify stronghold districts for this party (districts with high baseline)
      stronghold_threshold <- quantile(baseline_matrix[, i], 0.8)  # Top 20% of districts
      stronghold_districts <- which(baseline_matrix[, i] >= stronghold_threshold)
      non_stronghold_districts <- which(baseline_matrix[, i] < stronghold_threshold)
      
      # Apply proportional effects only to stronghold districts
      if (length(stronghold_districts) > 0) {
        district_swings[stronghold_districts, i] <- district_swings[stronghold_districts, i] * 
          (1 + baseline_matrix[stronghold_districts, i] * swing_concentration * 5.0)  # Increased from 2.0 to 5.0
      }
      
      # Compensate in non-stronghold districts to maintain national swing
      if (length(non_stronghold_districts) > 0) {
        # Calculate how much the stronghold effect changed the national swing
        stronghold_effect <- sum(district_swings[stronghold_districts, i]) - 
          sum(district_swings[stronghold_districts, i] / (1 + baseline_matrix[stronghold_districts, i] * swing_concentration * 5.0))
        
        # Distribute the compensation across non-stronghold districts
        compensation_per_district <- -stronghold_effect / length(non_stronghold_districts)
        district_swings[non_stronghold_districts, i] <- district_swings[non_stronghold_districts, i] + compensation_per_district
      }
    }

  # True vote shares - apply national swing and add district-level variation
  # Start with baseline
  true_vote_shares <- baseline_matrix
  
  # Apply national swing with uniform component + proportional effect
  for (i in 1:n_parties) {
    # National swing for this party (same across all districts)
    national_swing_for_party <- national_swing[i]
    
    # Always apply uniform swing component
    uniform_swing <- national_swing_for_party
    
    # Add proportional effect on top (can be 0 when swing_concentration = 0)
    proportional_component <- national_swing_for_party * baseline_matrix[, i] * swing_concentration
    total_swing_effect <- uniform_swing + proportional_component
    
          # Calculate the expected vote share after national swing
      expected_vote_share <- baseline_matrix[, i] + total_swing_effect
      
      # Calculate the actual swing (difference between true and baseline)
      actual_swing <- total_swing_effect
      
      # Use the district swings we already generated (with proportional effects applied)
      district_variation <- district_swings[, i]
      
      # Add district variation to the swing
      actual_swing <- actual_swing + district_variation
      
      # Calculate final vote share
      true_vote_shares[, i] <- baseline_matrix[, i] + actual_swing
  }
  
  # Add final noise to make prediction task harder
  random_noise <- matrix(rnorm(n_districts * n_parties, 0, 0.01), nrow = n_districts, ncol = n_parties)
  true_vote_shares <- true_vote_shares + random_noise
  
  # Ensure non-negative and sum to 1
  true_vote_shares <- t(apply(true_vote_shares, 1, function(x) pmax(x, 0)))
  true_vote_shares <- t(apply(true_vote_shares, 1, function(x) x / sum(x)))
  
  # Debug: Check swing and vote share distribution
  if (TRUE) {  # Set to TRUE to debug
    cat("Swing concentration:", swing_concentration, "\n")
    cat("National swing:", round(national_swing, 3), "\n")
    cat("District swings before proportional effects:\n")
    print(summary(as.vector(district_swings)))
    cat("District swings after proportional effects:\n")
    print(summary(as.vector(district_swings)))
    cat("True vote shares summary:\n")
    print(apply(true_vote_shares, 2, summary))
  }

  # Apply trained models to predict district-level vote shares
  # Create prediction data for each district-party combination
  pred_data <- expand.grid(
    district = 1:n_districts,
    party = 1:n_parties
  ) %>%
    mutate(
      # Use same ordering as training data (transpose first)
      baseline_share = as.vector(t(baseline_matrix)),
      true_share = as.vector(t(true_vote_shares)),
      # Use the actual national swing that was used to generate the data
      swing = rep(national_swing, each = n_districts)
    )
  
  # Predict using trained models
  pred_data$uniform_pred <- predict(uniform_model, newdata = pred_data)
  pred_data$proportional_pred <- predict(proportional_model, newdata = pred_data)
  
  # Reshape back to district × party matrices
  # pred_uniform <- matrix(pred_data$uniform_pred, nrow = n_districts, ncol = n_parties, byrow = TRUE)
  # pred_prop <- matrix(pred_data$proportional_pred, nrow = n_districts, ncol = n_parties, byrow = TRUE)
  
  # Ensure non-negative and sum to 1
  pred_data <- pred_data %>% 
    group_by(district) %>%
    mutate(
      uniform_pred = pmax(uniform_pred, 0),
      proportional_pred = pmax(proportional_pred, 0)
    ) %>%
    ungroup() %>%
    dplyr::select(district, party, uniform_pred, proportional_pred, swing, true_share, baseline_share)

  # Calculate accuracy by predicting district winners
  results <- tibble(
    district = pred_data$district,
    party = paste0("P", pred_data$party),
    true = pred_data$true_share,
    uniform = pred_data$uniform_pred,
    proportional = pred_data$proportional_pred,
    swing = pred_data$swing,
    baseline = pred_data$baseline_share
  )
  
  # For each district, determine winner and predictions
  district_results <- results %>%
    group_by(district) %>%
    mutate(
      # Handle ties by taking the first party with max vote share
      # Add safety check for cases where all vote shares are zero
      true_winner = ifelse(all(true <= 0), party[1], party[which.max(true)]),
      uniform_winner = ifelse(all(uniform <= 0), party[1], party[which.max(uniform)]),
      proportional_winner = ifelse(all(proportional <= 0), party[1], party[which.max(proportional)]),
      uniform_correct = (uniform_winner == true_winner),
      proportional_correct = (proportional_winner == true_winner)
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
  cat("Same predictions:", sum(district_results$uniform_correct == district_results$proportional_correct), "/", nrow(district_results), "\n")
  
  # Calculate accuracy at scenario level
  scenario_accuracy <- district_results %>%
    summarize(
      accuracy_uniform = mean(uniform_correct, na.rm = TRUE),
      accuracy_proportional = mean(proportional_correct, na.rm = TRUE),
      n_districts = n_distinct(district)
    ) %>%
    mutate(
      party_size = "overall"  # This will be aggregated by scenario
    )
  
  scenario_accuracy
}

# simulate_election(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_swing_variance, swing_concentration, district_noise_sd, n_districts)


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
    params$stronghold_share, params$national_swing_variance, params$swing_concentration, params$district_noise_sd, n_districts
  ))
  
  # Add scenario identifier
  scenario_type <- if (params$n_parties == length(german_params$large_parties) + length(german_params$small_parties) && 
                       params$n_small_parties == length(german_params$small_parties)) {
    "German (Realistic)"
  } else {
    "Artificial"
  }
  
  scenario_results %>%
    mutate(
      n_parties = params$n_parties,
      n_small_parties = params$n_small_parties,
      n_stronghold_districts = params$n_stronghold_districts,
      stronghold_share = params$stronghold_share,
      national_swing_variance = params$national_swing_variance,
      swing_concentration = params$swing_concentration,
      district_noise_sd = params$district_noise_sd,
      scenario_type = scenario_type
    )
  }, .options = furrr_options(seed = TRUE))
})


# Clean up parallel processing
plan(sequential)

# ========== 4. Aggregate and Summarize ==========
summary_results <- all_results_df %>%
  group_by(n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_swing_variance, swing_concentration, district_noise_sd, scenario_type) %>%
  summarize(
    accuracy_uniform = mean(accuracy_uniform),
    accuracy_proportional = mean(accuracy_proportional),
    n_simulations = n(),
    .groups = "drop"
  )

print(summary_results)

# ========== 5. Visualize ==========
results_long <- summary_results %>%
  pivot_longer(cols = starts_with("accuracy"), names_to = "model", values_to = "accuracy")

# Add a flag to identify German scenario
results_long <- results_long %>%
  mutate(
    is_german = (n_parties == 7 & n_small_parties == 5 & n_stronghold_districts == 30 & stronghold_share == 0.15)
  )


# Create bar plot showing accuracy differences by key parameters
ggplot(results_long, aes(x = model, y = accuracy, fill = model)) +
  geom_boxplot() + 
  facet_grid(n_small_parties ~ stronghold_share + n_parties) +
  scale_fill_manual(values = c("accuracy_uniform" = "#E69F00", "accuracy_proportional" = "#56B4E9")) +
  scale_x_discrete(labels = c("accuracy_uniform" = "uni", "accuracy_proportional" = "prop")) +
  labs(title = "Accuracy by model, number of parties, small parties, and swing volatility",
       subtitle = "Faceted by ...",
       y = "Accuracy (share of correctly predicted districts)",
       x = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Simple density plot with lines
ggplot(results_long, aes(x = accuracy, color = model)) +
  geom_density(size = 1) +
  scale_color_manual(values = c("accuracy_uniform" = "#E69F00", "accuracy_proportional" = "#56B4E9"),
                     labels = c("accuracy_uniform" = "Uniform", "accuracy_proportional" = "Proportional")) +
  labs(title = "Accuracy distributions: Uniform vs Proportional",
       x = "Accuracy (share of correctly predicted districts)",
       y = "Density",
       color = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: Box plot comparing accuracy distributions
ggplot(results_long, aes(x = model, y = accuracy, fill = model)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("accuracy_uniform" = "#E69F00", "accuracy_proportional" = "#56B4E9")) +
  scale_x_discrete(labels = c("accuracy_uniform" = "Uniform", "accuracy_proportional" = "Proportional")) +
  labs(title = "Accuracy comparison: Uniform vs Proportional swing",
       subtitle = "Box plot showing median, quartiles, and outliers",
       y = "Accuracy (share of correctly predicted districts)",
       x = "Model") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Focus on scenarios with many small parties
many_small_party_results <- summary_results %>%
  filter(n_small_parties >= 3) %>%
  mutate(
    proportional_better = accuracy_proportional > accuracy_uniform,
    improvement = (accuracy_proportional - accuracy_uniform) / accuracy_uniform * 100
  )

# Check which scenarios favor proportional
proportional_favorable <- summary_results %>%
  mutate(
    proportional_better = accuracy_proportional > accuracy_uniform,
    performance_diff = accuracy_uniform - accuracy_proportional
  ) %>%
  filter(proportional_better == TRUE)

print("Scenarios where proportional is better:")
# print(proportional_favorable %>% dplyr::select(n_parties, n_small_parties, swing_sd, stronghold_share, accuracy_uniform, accuracy_proportional, performance_diff))

print("Summary of when proportional wins:")
print(proportional_favorable %>%
  summarize(
    n_scenarios = n(),
    avg_n_small_parties = mean(n_small_parties),
    avg_national_swing_variance = mean(national_swing_variance),
    avg_stronghold_share = mean(stronghold_share)
  ))

# Analyze by swing concentration
print("Performance by swing concentration:")
summary_results %>%
  group_by(swing_concentration) %>%
  summarize(
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    avg_performance_diff = mean(accuracy_uniform - accuracy_proportional),
    n_scenarios = n()
  ) %>%
  print()


# ========== 6. Regression Analysis ==========
# Prepare data for regression analysis at SCENARIO level
regression_data <- summary_results %>%
  mutate(
    # Performance difference (positive = uniform better, negative = proportional better)
    performance_diff = accuracy_uniform - accuracy_proportional,
    
    # Relative performance (percentage improvement of proportional over uniform)
    relative_performance = (accuracy_proportional - accuracy_uniform) / accuracy_uniform * 100,
    
    # Scenario characteristics only
    stronghold_ratio = n_stronghold_districts / n_districts,  # Proportion of stronghold districts
    log_national_swing_variance = log(national_swing_variance),
    log_district_noise = log(district_noise_sd),
    log_n_parties = log(n_parties),
    log_n_small_parties = log(n_small_parties),
    
    # Interaction terms
    small_stronghold_ratio = log_n_small_parties * stronghold_ratio,
    small_swing = log_n_small_parties * log_national_swing_variance,
    concentration_effect = swing_concentration * log_n_small_parties,  # Interaction of swing concentration and small parties
    noise_effect = district_noise_sd * swing_concentration  # Interaction of district noise and swing concentration
  )

# (n_parties, n_small_parties, n_stronghold_districts, stronghold_share, national_swing_variance, swing_concentration, district_noise_sd, n_districts

# Main regression: What explains the accuracy difference?
model1 <- lm(performance_diff ~ log_n_parties + log_n_small_parties + stronghold_ratio + log_national_swing_variance + 
             swing_concentration + log_district_noise + small_stronghold_ratio, data = regression_data)

print("Regression 1: Accuracy difference (uniform - proportional)")
print(summary(model1))

# Alternative: What explains relative accuracy improvement?
model2 <- lm(relative_performance ~ log_n_parties + log_n_small_parties + stronghold_ratio + log_national_swing_variance + 
             swing_concentration + log_district_noise + small_stronghold_ratio, data = regression_data)

print("Regression 2: Relative accuracy improvement (%)")
print(summary(model2))

# Separate regressions by number of small parties
few_small_data <- regression_data %>% filter(n_small_parties <= 2)
many_small_data <- regression_data %>% filter(n_small_parties >= 3)

model_few_small <- lm(performance_diff ~ stronghold_ratio + log_national_swing_variance, 
                      data = few_small_data)
model_many_small <- lm(performance_diff ~ stronghold_ratio + log_national_swing_variance, 
                       data = many_small_data)

print("Regression 3: Few small parties (≤2)")
print(summary(model_few_small))

print("Regression 4: Many small parties (≥3)")
print(summary(model_many_small))

# ========== 7. Key Insights Summary ==========
cat("\n=== KEY INSIGHTS ===\n")
cat("1. Average accuracy difference by number of small parties:\n")
regression_data %>%
  group_by(n_small_parties) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_relative_accuracy = mean(relative_performance),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    n = n()
  ) %>%
  print()

cat("\n2. Effect of stronghold ratio on accuracy:\n")
regression_data %>%
  group_by(stronghold_ratio) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_relative_accuracy = mean(relative_performance),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    n = n()
  ) %>%
  print()

cat("\n3. Effect of swing volatility:\n")
regression_data %>%
  group_by(national_swing_variance) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_relative_accuracy = mean(relative_performance),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    n = n()
  ) %>%
  print()

cat("\n4. Effect of district noise:\n")
regression_data %>%
  group_by(district_noise_sd) %>%
  summarize(
    avg_accuracy_diff = mean(performance_diff),
    avg_relative_accuracy = mean(relative_performance),
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    n = n()
  ) %>%
  print()

# ========== 8. Compare with Empirical German Results ==========
cat("\n=== COMPARISON WITH EMPIRICAL GERMAN RESULTS ===\n")

# Load empirical results
empirical_results <- read.csv("data/out/eval_aggregate.csv")
empirical_uniform <- empirical_results$avg_accuracy_winner[empirical_results$name == "uniform"]
empirical_proportional <- empirical_results$avg_accuracy_winner[empirical_results$name == "proportional second vote"]

cat("Empirical German Results (", german_params$n_districts, " districts, 8 elections):\n")
cat("- Uniform swing accuracy:", round(empirical_uniform, 3), "\n")
cat("- Proportional swing accuracy:", round(empirical_proportional, 3), "\n")
cat("- Difference (Proportional - Uniform):", round(empirical_proportional - empirical_uniform, 3), "\n")

# Calculate simulation performance metrics
simulation_performance <- summary_results %>%
  summarize(
    avg_accuracy_uniform = mean(accuracy_uniform),
    avg_accuracy_proportional = mean(accuracy_proportional),
    avg_performance_diff = mean(accuracy_uniform - accuracy_proportional),
    n = n()
  )

cat("\nSimulation Results (averaged across scenarios):\n")
print(simulation_performance)

# Compare overall performance
cat("\n=== SIMULATION VS EMPIRICAL COMPARISON ===\n")
cat("Empirical: Both models perform similarly (difference:", round(empirical_proportional - empirical_uniform, 3), ")\n")
cat("Simulation: Check if models perform similarly or if one dominates\n")

# Check if simulation matches empirical pattern
overall_uniform_accuracy <- mean(summary_results$accuracy_uniform)
overall_proportional_accuracy <- mean(summary_results$accuracy_proportional)
simulation_diff <- overall_proportional_accuracy - overall_uniform_accuracy

cat("Simulation overall difference (Proportional - Uniform accuracy):", round(simulation_diff, 4), "\n")
if (abs(simulation_diff) < 0.01) {
  cat("✓ Simulation matches empirical pattern: both models perform similarly\n")
} else if (simulation_diff > 0) {
  cat("✗ Simulation shows proportional better (unlike empirical)\n")
} else {
  cat("✗ Simulation shows uniform better (unlike empirical)\n")
}

# ========== 9. German Reality vs Simulation Assumptions ==========
cat("\n=== GERMAN REALITY VS SIMULATION ASSUMPTIONS ===\n")
cat("German Reality:\n")
cat("- Small parties: 5-15% baseline (FDP, Grüne, Linke)\n")
cat("- Gradual geographic variation\n")
cat("- Moderate swings: 1-3%\n")
cat("- ", german_params$n_districts, " districts\n")

cat("\nCurrent Simulation:\n")
cat("- Small parties: 5% baseline in most districts\n")
cat("- Extreme strongholds: 30-50% in 5-15 districts\n")
cat("- Swings: 1-3% SD (realistic)\n")
cat("- 100 districts\n")

cat("\nRecommendations to match German reality:\n")
cat("1. Increase small party baseline to 8-12%\n")
cat("2. Use gradual geographic variation instead of extreme strongholds\n")
cat("3. Add more realistic party characteristics\n")

