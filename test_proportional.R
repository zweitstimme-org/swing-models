# Simple test to get proportional swing model working
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Simple test case: 2 parties, 10 districts
n_districts <- 10
n_parties <- 2

# Create baseline vote shares (previous election results)
baseline_matrix <- matrix(c(
  # Party 1 baseline shares across districts
  0.45, 0.42, 0.48, 0.40, 0.50, 0.38, 0.52, 0.44, 0.46, 0.41,
  # Party 2 baseline shares across districts  
  0.55, 0.58, 0.52, 0.60, 0.50, 0.62, 0.48, 0.56, 0.54, 0.59
), nrow = n_districts, ncol = n_parties, byrow = FALSE)

# Normalize to sum to 1
baseline_matrix <- baseline_matrix / rowSums(baseline_matrix)

print("Baseline matrix (previous election):")
print(baseline_matrix)

# Create multiple training elections with different proportional swings
n_training_elections <- 4
training_data_list <- list()

for (election in 1:n_training_elections) {
  # Generate different national swings for each election
  national_swing_party1 <- rnorm(1, mean = 0, sd = 0.05)  # Random swing between -10% and +10%
  national_swing_party2 <- -national_swing_party1  # Opposite swing for party 2
  
  # Apply proportional swing: stronger parties get larger absolute swings
  true_vote_shares <- baseline_matrix
  true_vote_shares[, 1] <- baseline_matrix[, 1] + (baseline_matrix[, 1] * national_swing_party1)
  true_vote_shares[, 2] <- baseline_matrix[, 2] + (baseline_matrix[, 2] * national_swing_party2)
  
  # Add some district-level noise to make it more realistic
  district_noise <- matrix(rnorm(n_districts * n_parties, 0, 0.01), nrow = n_districts, ncol = n_parties)
  true_vote_shares <- true_vote_shares + district_noise
  
  # Normalize to sum to 1
  true_vote_shares <- true_vote_shares / rowSums(true_vote_shares)
  
  # Create POLLING data (noisy version of true vote shares)
  polling_noise <- matrix(rnorm(n_districts * n_parties, 0, 0.02), nrow = n_districts, ncol = n_parties)
  polling_shares <- true_vote_shares + polling_noise
  polling_shares <- polling_shares / rowSums(polling_shares)
  
  # Calculate swings from baseline to polling (this is what we observe)
  polling_swings <- polling_shares - baseline_matrix
  
  # Calculate swings from polling to actual results (this is what we want to predict)
  actual_swings <- true_vote_shares - polling_shares
  
  # Create training data for this election
  election_data <- data.frame(
    election = election,
    district = rep(1:n_districts, each = n_parties),
    party = rep(1:n_parties, times = n_districts),
    baseline_share = as.vector(t(baseline_matrix)),
    polling_share = as.vector(t(polling_shares)),
    true_share = as.vector(t(true_vote_shares)),
    polling_swing = as.vector(t(polling_swings)),
    actual_swing = as.vector(t(actual_swings)),
    national_swing_party1 = national_swing_party1,
    national_swing_party2 = national_swing_party2
  ) %>%
    mutate(
      # Proportional swing variable: baseline * polling_swing
      proportional_swing = baseline_share * polling_swing,
      # Uniform swing variable: baseline + polling_swing  
      uniform_swing = baseline_share + polling_swing
    )
  
  training_data_list[[election]] <- election_data
}

# Combine all training data
training_data <- bind_rows(training_data_list)

print("Training data summary:")
print(head(training_data, 20))

# Train models to predict actual swing from polling swing
uniform_model <- lm(actual_swing ~ uniform_swing, data = training_data)
proportional_model <- lm(actual_swing ~ proportional_swing, data = training_data)

print("Uniform model:")
print(summary(uniform_model))

print("Proportional model:")
print(summary(proportional_model))

# Test on a new election with DIFFERENT baseline shares
test_baseline_matrix <- matrix(c(
  # Party 1 baseline shares across districts (different from training)
  0.43, 0.44, 0.47, 0.41, 0.49, 0.39, 0.51, 0.45, 0.47, 0.42,
  # Party 2 baseline shares across districts (different from training)
  0.57, 0.56, 0.53, 0.59, 0.51, 0.61, 0.49, 0.55, 0.53, 0.58
), nrow = n_districts, ncol = n_parties, byrow = FALSE)

# Normalize to sum to 1
test_baseline_matrix <- test_baseline_matrix / rowSums(test_baseline_matrix)

print("Test baseline matrix:")
print(test_baseline_matrix)

# Create true test vote shares with proportional swing
test_national_swing_party1 <- 0.03  # +3% swing
test_national_swing_party2 <- -0.03  # -3% swing

test_true_vote_shares <- test_baseline_matrix
test_true_vote_shares[, 1] <- test_baseline_matrix[, 1] + (test_baseline_matrix[, 1] * test_national_swing_party1)
test_true_vote_shares[, 2] <- test_baseline_matrix[, 2] + (test_baseline_matrix[, 2] * test_national_swing_party2)

# Add district-level noise to test data too
test_district_noise <- matrix(rnorm(n_districts * n_parties, 0, 0.01), nrow = n_districts, ncol = n_parties)
test_true_vote_shares <- test_true_vote_shares + test_district_noise
test_true_vote_shares <- test_true_vote_shares / rowSums(test_true_vote_shares)

# Create test POLLING data (noisy version of true vote shares)
test_polling_noise <- matrix(rnorm(n_districts * n_parties, 0, 0.02), nrow = n_districts, ncol = n_parties)
test_polling_shares <- test_true_vote_shares + test_polling_noise
test_polling_shares <- test_polling_shares / rowSums(test_polling_shares)

# Calculate test swings from baseline to polling
test_polling_swings <- test_polling_shares - test_baseline_matrix

# Calculate test swings from polling to actual results
test_actual_swings <- test_true_vote_shares - test_polling_shares

# Create test data
test_data <- data.frame(
  district = rep(1:n_districts, each = n_parties),
  party = rep(1:n_parties, times = n_districts),
  baseline_share = as.vector(t(test_baseline_matrix)),
  polling_share = as.vector(t(test_polling_shares)),
  true_share = as.vector(t(test_true_vote_shares)),
  polling_swing = as.vector(t(test_polling_swings)),
  actual_swing = as.vector(t(test_actual_swings))
) %>%
  mutate(
    proportional_swing = baseline_share * polling_swing,
    uniform_swing = baseline_share + polling_swing
  )

# Make predictions of actual swing
test_data <- test_data %>%
  mutate(
    uniform_swing_pred = predict(uniform_model, newdata = .),
    proportional_swing_pred = predict(proportional_model, newdata = .)
  ) %>%
  mutate(
    # Convert swing predictions to vote share predictions
    uniform_pred = polling_share + uniform_swing_pred,
    proportional_pred = polling_share + proportional_swing_pred
  )

print("Test predictions:")
print(test_data %>% select(district, party, true_share, uniform_pred, proportional_pred))

# Calculate accuracy (winner prediction)
district_results <- test_data %>%
  group_by(district) %>%
  mutate(
    true_winner = party[which.max(true_share)],
    uniform_winner = party[which.max(uniform_pred)],
    proportional_winner = party[which.max(proportional_pred)],
    uniform_correct = (uniform_winner == true_winner),
    proportional_correct = (proportional_winner == true_winner)
  ) %>%
  ungroup() %>%
  filter(party == true_winner)

print("Accuracy results:")
print(district_results %>% select(district, true_winner, uniform_winner, proportional_winner, uniform_correct, proportional_correct))

print(paste("Uniform accuracy:", mean(district_results$uniform_correct)))
print(paste("Proportional accuracy:", mean(district_results$proportional_correct)))

# Calculate MAE and RMSE
mae_uniform <- mean(abs(test_data$uniform_pred - test_data$true_share))
mae_proportional <- mean(abs(test_data$proportional_pred - test_data$true_share))
rmse_uniform <- sqrt(mean((test_data$uniform_pred - test_data$true_share)^2))
rmse_proportional <- sqrt(mean((test_data$proportional_pred - test_data$true_share)^2))

print(paste("Uniform MAE:", mae_uniform))
print(paste("Proportional MAE:", mae_proportional))
print(paste("Uniform RMSE:", rmse_uniform))
print(paste("Proportional RMSE:", rmse_proportional))
