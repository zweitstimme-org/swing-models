# Party Vote Share Volatility Analysis
# Calculate volatility of party vote shares between districts for each country
# Based on CLEA data for the last 20 years (2004-2023)

library(dplyr)
library(ggplot2)
library(tidyr)

# Load CLEA data
load('data/clea/clea_lc_20240419_r/clea_lc_20240419.RData')

# Define ParlGov countries to restrict analysis
parlgov_countries <- c("Australia", "Austria", "Belgium", "Bulgaria", "Canada", "Switzerland", "Cyprus", "Czech Republic", "Germany", "Denmark", "Spain", "Estonia",
                      "Finland", "France", "UK", "Greece", "Croatia", "Hungary", "Ireland", "Iceland", "Israel", "Italy", "Japan", "Lithuania",
                      "Luxembourg", "Latvia", "Malta", "Netherlands", "Norway", "New Zealand", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Sweden", "Turkey")

# Filter for last 20 years (2004-2023) and ParlGov countries only
recent_data <- clea_lc_20240419 %>%
  filter(yr >= 2004 & yr <= 2023) %>%
  filter(ctr_n %in% parlgov_countries) %>%  # RESTRICT TO PARLGOV COUNTRIES
  # Remove missing values and invalid codes (-990)
  filter(!is.na(pvs1) & pvs1 > 0 & pvs1 <= 100) %>%  # CORRECTED: 0-100 format
  filter(!is.na(ctr_n) & ctr_n != "") %>%
  filter(!is.na(cst_n) & cst_n != "") %>%
  filter(!is.na(pty_n) & pty_n != "") %>%
  # Convert vote shares from 0-100 to 0-1 format
  mutate(pvs1 = pvs1 / 100)

cat("Filtered data dimensions:", dim(recent_data), "\n")
cat("Countries in filtered data:", length(unique(recent_data$ctr_n)), "\n")
cat("Years covered:", range(recent_data$yr), "\n")

# Calculate volatility for each party in each election
volatility_data <- recent_data %>%
  group_by(ctr_n, yr, pty_n) %>%
  summarise(
    n_districts = n(),
    mean_vote_share = mean(pvs1, na.rm = TRUE),
    sd_vote_share = sd(pvs1, na.rm = TRUE),
    cv_vote_share = ifelse(mean_vote_share > 0, sd_vote_share / mean_vote_share, NA),
    .groups = 'drop'
  ) %>%
  # Only include parties that contested in at least 2 districts
  filter(n_districts >= 2) %>%
  # Remove parties with very low vote shares (likely noise) - 0.5% in 0-1 format
  filter(mean_vote_share >= 0.005)

cat("Parties with sufficient data:", nrow(volatility_data), "\n")

# Calculate country-level volatility metrics
country_volatility <- volatility_data %>%
  group_by(ctr_n) %>%
  summarise(
    n_elections = n_distinct(yr),
    n_parties = n_distinct(pty_n),
    n_party_elections = n(),
    mean_n_districts = mean(n_districts, na.rm = TRUE),
    mean_cv = mean(cv_vote_share, na.rm = TRUE),
    median_cv = median(cv_vote_share, na.rm = TRUE),
    sd_cv = sd(cv_vote_share, na.rm = TRUE),
    mean_sd_vote_share = mean(sd_vote_share, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Only include countries with at least 1 election and 2 party observations
  filter(n_elections >= 1 & n_party_elections >= 2) %>%
  # Rank by mean coefficient of variation (volatility)
  arrange(desc(mean_cv)) %>%
  mutate(rank = row_number())

cat("Countries with sufficient data:", nrow(country_volatility), "\n")

# Display top 20 most volatile countries
cat("\nTop 20 Most Volatile Countries (by party vote share volatility):\n")
print(country_volatility[1:20, c("rank", "ctr_n", "n_elections", "n_parties", "mean_n_districts", "mean_cv", "median_cv")])

# Display bottom countries (all if less than 20)
cat("\nBottom Least Volatile Countries (by party vote share volatility):\n")
n_countries <- nrow(country_volatility)
if (n_countries <= 20) {
  print(country_volatility[, c("rank", "ctr_n", "n_elections", "n_parties", "mean_n_districts", "mean_cv", "median_cv")])
} else {
  print(country_volatility[(n_countries-19):n_countries, c("rank", "ctr_n", "n_elections", "n_parties", "mean_n_districts", "mean_cv", "median_cv")])
}

# Create visualization
p1 <- country_volatility %>%
  head(30) %>%
  ggplot(aes(x = reorder(ctr_n, mean_cv), y = mean_cv)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "ParlGov Countries by Party Vote Share Volatility (CORRECTED)",
    subtitle = "Coefficient of Variation of Party Vote Shares Between Districts (2004-2023)",
    x = "Country",
    y = "Mean Coefficient of Variation",
    caption = "Based on CLEA data. Vote shares converted from 0-100 to 0-1 format. Higher values indicate more volatile party performance across districts."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 8)
  )

ggsave("figures/party_volatility_ranking.pdf", p1, width = 12, height = 8)
ggsave("figures/party_volatility_ranking.png", p1, width = 12, height = 8, dpi = 300)

# Create a more detailed analysis for top countries
top_countries <- head(country_volatility$ctr_n, 10)

detailed_analysis <- volatility_data %>%
  filter(ctr_n %in% top_countries) %>%
  group_by(ctr_n) %>%
  summarise(
    n_elections = n_distinct(yr),
    n_parties = n_distinct(pty_n),
    mean_n_districts = mean(n_districts, na.rm = TRUE),
    mean_cv = mean(cv_vote_share, na.rm = TRUE),
    median_cv = median(cv_vote_share, na.rm = TRUE),
    q75_cv = quantile(cv_vote_share, 0.75, na.rm = TRUE),
    q25_cv = quantile(cv_vote_share, 0.25, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_cv))

cat("\nDetailed Analysis of Top 10 Most Volatile ParlGov Countries (CORRECTED):\n")
print(detailed_analysis)

# Save results
write.csv(country_volatility, "data/out/party_volatility_ranking_parlgov.csv", row.names = FALSE)
write.csv(detailed_analysis, "data/out/top_countries_detailed_parlgov.csv", row.names = FALSE)

cat("\nAnalysis complete! Results saved to:\n")
cat("- figures/party_volatility_ranking.pdf\n")
cat("- figures/party_volatility_ranking.png\n")
cat("- data/out/party_volatility_ranking_parlgov.csv\n")
cat("- data/out/top_countries_detailed_parlgov.csv\n")

# Compare with simulation parameters
cat("\n=== COMPARISON WITH SIMULATION PARAMETERS ===\n")
cat("CLEA Mean SD vote share range (0-1 format):", round(range(country_volatility$mean_sd_vote_share), 4), "\n")
cat("Simulation between_district_vote_volatility range: 0.002 to 0.03\n")
cat("Ratio of max CLEA to max simulation:", round(max(country_volatility$mean_sd_vote_share) / 0.03, 1), "x\n")
cat("Ratio of max CLEA to min simulation:", round(max(country_volatility$mean_sd_vote_share) / 0.002, 1), "x\n")
cat("Countries with volatility within simulation range:", 
    sum(country_volatility$mean_sd_vote_share <= 0.03), "out of", nrow(country_volatility), "\n")
cat("Average number of districts per country:", round(mean(country_volatility$mean_n_districts), 1), "\n")
cat("Range of districts per country:", round(range(country_volatility$mean_n_districts)), "\n")
