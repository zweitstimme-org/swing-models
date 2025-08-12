# extract_german_params.R
# Extract realistic German parameters from empirical data

library(tidyverse)

# Load the data
load("data/btw_candidates_1983-2025.RData")

# ========== Extract German Parameters ==========

# 1. Party baseline shares (from res_l1_Z - previous election results)
baseline_shares <- btw_candidates_1983_2025 %>%
  filter(!is.na(res_l1_Z), !is.na(party)) %>%
  group_by(party) %>%
  summarize(
    mean_baseline = mean(res_l1_Z, na.rm = TRUE),
    median_baseline = median(res_l1_Z, na.rm = TRUE),
    sd_baseline = sd(res_l1_Z, na.rm = TRUE),
    min_baseline = min(res_l1_Z, na.rm = TRUE),
    max_baseline = max(res_l1_Z, na.rm = TRUE),
    n_districts = n()
  ) %>%
  arrange(desc(mean_baseline))

print("German Party Baseline Shares:")
print(baseline_shares)

# 2. Swing volatility (from uniform swing variable)
swing_volatility <- btw_candidates_1983_2025 %>%
  filter(!is.na(uniform)) %>%
  group_by(party) %>%
  summarize(
    mean_swing = mean(uniform, na.rm = TRUE),
    sd_swing = sd(uniform, na.rm = TRUE),
    abs_mean_swing = mean(abs(uniform), na.rm = TRUE),
    n_observations = n()
  ) %>%
  arrange(desc(sd_swing))

print("\nGerman Swing Volatility:")
print(swing_volatility)

# 3. Geographic patterns (district-level variation)
# First, identify districts that appear frequently enough (>15 times)
frequent_districts <- btw_candidates_1983_2025 %>%
  filter(!is.na(wkr)) %>%
  group_by(wkr) %>%
  summarize(n_observations = n()) %>%
  filter(n_observations > 15) %>%
  pull(wkr)

cat("Number of districts with >15 observations:", length(frequent_districts), "\n")

geographic_patterns <- btw_candidates_1983_2025 %>%
  filter(!is.na(res_l1_Z), !is.na(wkr), wkr %in% frequent_districts) %>%
  group_by(party, wkr) %>%
  summarize(
    district_baseline = mean(res_l1_Z, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(party) %>%
  summarize(
    district_sd = sd(district_baseline, na.rm = TRUE),
    district_cv = district_sd / mean(district_baseline, na.rm = TRUE),  # Coefficient of variation
    n_districts = n(),
    min_district = min(district_baseline, na.rm = TRUE),
    max_district = max(district_baseline, na.rm = TRUE),
    stronghold_ratio = sum(district_baseline > 0.15, na.rm = TRUE) / n_districts  # Districts with >15% support
  ) %>%
  arrange(desc(district_sd))

print("\nGerman Geographic Patterns:")
print(geographic_patterns)

# 4. Number of parties per election
parties_per_election <- btw_candidates_1983_2025 %>%
  filter(!is.na(party)) %>%
  group_by(election) %>%
  summarize(
    n_parties = n_distinct(party),
    parties = paste(unique(party), collapse = ", ")
  ) %>%
  arrange(election)

print("\nParties per Election:")
print(parties_per_election)

# 5. Small vs Large party classification
party_classification <- baseline_shares %>%
  mutate(
    party_size = case_when(
      mean_baseline >= 0.20 ~ "large",
      mean_baseline >= 0.08 ~ "medium", 
      TRUE ~ "small"
    )
  )

print("\nParty Size Classification:")
print(party_classification)

# ========== Summary for Simulation ==========
cat("\n=== GERMAN PARAMETERS FOR SIMULATION ===\n")

# Large parties
large_parties <- party_classification %>% filter(party_size == "large")
cat("Large parties (baseline > 20%):", paste(large_parties$party, collapse = ", "), "\n")
cat("Large party baseline range:", round(min(large_parties$mean_baseline), 3), "-", round(max(large_parties$mean_baseline), 3), "\n")

# Small parties  
small_parties <- party_classification %>% filter(party_size == "small")
cat("Small parties (baseline < 8%):", paste(small_parties$party, collapse = ", "), "\n")
cat("Small party baseline range:", round(min(small_parties$mean_baseline), 3), "-", round(max(small_parties$mean_baseline), 3), "\n")

# Swing volatility (now in correct scale)
overall_swing_sd <- mean(swing_volatility$sd_swing, na.rm = TRUE)
cat("Overall swing volatility (SD):", round(overall_swing_sd, 3), "percentage points\n")

# Geographic variation
avg_stronghold_ratio <- mean(geographic_patterns$stronghold_ratio, na.rm = TRUE)
cat("Average stronghold ratio (districts >15%):", round(avg_stronghold_ratio, 3), "\n")

# Number of districts (only frequent ones)
n_districts_german <- length(frequent_districts)
cat("Number of districts (with >15 observations):", n_districts_german, "\n")

# Save parameters for simulation
german_params <- list(
  n_districts = n_districts_german,
  large_parties = large_parties$party,
  small_parties = small_parties$party,
  large_baseline_range = c(min(large_parties$mean_baseline), max(large_parties$mean_baseline)),
  small_baseline_range = c(min(small_parties$mean_baseline), max(small_parties$mean_baseline)),
  swing_sd = overall_swing_sd,
  stronghold_ratio = avg_stronghold_ratio,
  geographic_patterns = geographic_patterns
)

save(german_params, file = "data/out/german_params.RData")
cat("\nGerman parameters saved to data/out/german_params.RData\n") 