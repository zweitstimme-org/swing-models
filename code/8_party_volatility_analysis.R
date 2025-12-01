# Party Vote Share Volatility Analysis
# Calculate volatility of party vote shares between districts for each country
# Based on CLEA data for the last 20 years (2004-2023)

library(dplyr)
library(ggplot2)
library(tidyr)

# Load CLEA data
load('data/clea/clea_lc_20240419_r/clea_lc_20240419.RData')

# Define ParlGov countries with reliable data only (excluding countries with major data quality issues)
# Countries with correct or nearly correct district numbers: Germany (299), Luxembourg (4), UK (647), France (571), 
# Canada (338), Australia (151), Japan (303), Malta (13), New Zealand (73), Norway (19), Sweden (29), Portugal (22), Spain (54)
# Excluded: Slovakia (data corruption), Switzerland (missing constituencies), Belgium (missing constituencies), 
# Netherlands (missing constituencies), Denmark (missing constituencies), Finland (missing constituencies), 
# Austria (missing constituencies), Iceland (missing constituencies)
parlgov_countries <- c("Australia", "Bulgaria", "Canada", "Cyprus", "Czech Republic", "Germany", "Spain", "Estonia",
                      "France", "UK", "Greece", "Croatia", "Hungary", "Ireland", "Israel", "Italy", "Japan", "Lithuania",
                      "Luxembourg", "Latvia", "Malta", "Norway", "New Zealand", "Poland", "Portugal", "Romania", "Slovenia", "Sweden", "Turkey")

# Filter for last 20 years (2004-2023) and ParlGov countries only
recent_data <- clea_lc_20240419 %>%
  filter(yr >= 2004 & yr <= 2023) %>%
  filter(ctr_n %in% parlgov_countries) %>%  # RESTRICT TO PARLGOV COUNTRIES
  # Remove missing values and invalid codes (-990)
  filter(!is.na(pvs1) & pvs1 > 0 & pvs1 <= 1) %>%  # CORRECTED: 0-1 format
  filter(!is.na(ctr_n) & ctr_n != "") %>%
  filter(!is.na(cst_n) & cst_n != "") %>%
  filter(!is.na(pty_n) & pty_n != "")

cat("Filtered data dimensions:", dim(recent_data), "\n")
cat("Countries in filtered data:", length(unique(recent_data$ctr_n)), "\n")
cat("Years covered:", range(recent_data$yr), "\n")

# Limit to maximum 3 most recent elections per country
# First, identify distinct elections per country
country_elections <- recent_data %>%
  distinct(ctr_n, yr) %>%
  arrange(ctr_n, desc(yr)) %>%
  group_by(ctr_n) %>%
  slice_head(n = 3) %>%  # Keep only the 3 most recent elections per country
  ungroup()

# Filter recent_data to only include these elections
recent_data <- recent_data %>%
  semi_join(country_elections, by = c("ctr_n", "yr"))

cat("\nAfter limiting to max 3 elections per country:\n")
cat("Filtered data dimensions:", dim(recent_data), "\n")
cat("Elections per country:\n")
elections_per_country <- recent_data %>%
  distinct(ctr_n, yr) %>%
  count(ctr_n, name = "n_elections") %>%
  arrange(desc(n_elections))
print(elections_per_country)

# Diagnostic: Check Germany specifically
cat("\nGermany elections in filtered data (after election limit):\n")
germany_elections <- recent_data %>%
  filter(ctr_n == "Germany") %>%
  distinct(yr) %>%
  arrange(desc(yr))
print(germany_elections)

cat("\nGermany data counts by year (after election limit):\n")
germany_counts <- recent_data %>%
  filter(ctr_n == "Germany") %>%
  count(yr) %>%
  arrange(desc(yr))
print(germany_counts)

# Calculate volatility for each party in each election
# First deduplicate the data to avoid counting duplicate entries
deduplicated_data <- recent_data %>%
  group_by(ctr_n, yr, cst_n, pty_n) %>%
  summarise(
    pvs1 = mean(pvs1, na.rm = TRUE),  # Take mean if duplicates exist
    .groups = 'drop'
  )

volatility_data <- deduplicated_data %>%
  group_by(ctr_n, yr, pty_n) %>%
  summarise(
    n_districts = n(),
    mean_vote_share = mean(pvs1, na.rm = TRUE),
    sd_vote_share = sd(pvs1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Only include parties that contested in at least 2 districts
  filter(n_districts >= 2) %>%
  # Remove parties with very low vote shares (likely noise) - 1% in 0-1 format
  filter(mean_vote_share >= 0.01)

cat("Parties with sufficient data:", nrow(volatility_data), "\n")

# Diagnostic: Check Germany after volatility calculation
cat("\nGermany elections in volatility_data:\n")
germany_volatility_years <- volatility_data %>%
  filter(ctr_n == "Germany") %>%
  distinct(yr) %>%
  arrange(desc(yr))
print(germany_volatility_years)

# Calculate country-level volatility metrics
country_volatility <- volatility_data %>%
  group_by(ctr_n) %>%
  summarise(
    n_elections = n_distinct(yr),
    n_party_elections = n(),
    max_n_districts = max(n_districts, na.rm = TRUE),
    mean_sd_vote_share = mean(sd_vote_share, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Only include countries with at least 1 election and 2 party observations
  filter(n_elections >= 1 & n_party_elections >= 2) %>%
  # Rank by mean standard deviation of vote share (volatility)
  arrange(desc(mean_sd_vote_share)) %>%
  mutate(rank = row_number())

cat("Countries with sufficient data:", nrow(country_volatility), "\n")

# Display top 20 most volatile countries
cat("\nTop 20 Most Volatile Countries (by party vote share volatility):\n")
print(country_volatility[1:20, c("rank", "ctr_n", "n_elections", "max_n_districts", "mean_sd_vote_share")])

# Display bottom countries (all if less than 20)
cat("\nBottom Least Volatile Countries (by party vote share volatility):\n")
n_countries <- nrow(country_volatility)
if (n_countries <= 20) {
  print(country_volatility[, c("rank", "ctr_n", "n_elections", "max_n_districts", "mean_sd_vote_share")])
} else {
  print(country_volatility[(n_countries-19):n_countries, c("rank", "ctr_n", "n_elections", "max_n_districts", "mean_sd_vote_share")])
}

# Create visualization
p1 <- country_volatility %>%
  head(30) %>%
  ggplot(aes(x = reorder(ctr_n, mean_sd_vote_share), y = mean_sd_vote_share)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "ParlGov Countries by Party Vote Share Volatility (CORRECTED)",
    subtitle = "Mean Standard Deviation of Party Vote Shares Between Districts (2004-2023)",
    x = "Country",
    y = "Mean Standard Deviation of Vote Share",
    caption = "Based on CLEA data. Vote shares already in 0-1 format. Higher values indicate more volatile party performance across districts."
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
    max_n_districts = max(n_districts, na.rm = TRUE),
    mean_sd_vote_share = mean(sd_vote_share, na.rm = TRUE),
    median_sd_vote_share = median(sd_vote_share, na.rm = TRUE),
    q75_sd_vote_share = quantile(sd_vote_share, 0.75, na.rm = TRUE),
    q25_sd_vote_share = quantile(sd_vote_share, 0.25, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_sd_vote_share))

cat("\nDetailed Analysis of Top 10 Most Volatile ParlGov Countries (CORRECTED):\n")
print(detailed_analysis)

# Save results
write.csv(country_volatility, "data/out/party_volatility_ranking_parlgov.csv", row.names = FALSE)

# Create LaTeX table for between-district volatility
library(xtable)

# For table 3, recalculate volatility using only parties with ≥5% aggregate vote share
volatility_data_5pct <- deduplicated_data %>%
  group_by(ctr_n, yr, pty_n) %>%
  summarise(
    n_districts = n(),
    mean_vote_share = mean(pvs1, na.rm = TRUE),
    sd_vote_share = sd(pvs1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Only include parties that contested in at least 2 districts
  filter(n_districts >= 2) %>%
  # Filter to parties with ≥5% aggregate vote share (for table 3)
  filter(mean_vote_share >= 0.05)

# Calculate country-level volatility metrics for table 3 (using 5% threshold)
country_volatility_5pct <- volatility_data_5pct %>%
  group_by(ctr_n) %>%
  summarise(
    n_elections = n_distinct(yr),
    n_party_elections = n(),
    max_n_districts = max(n_districts, na.rm = TRUE),
    mean_sd_vote_share = mean(sd_vote_share, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Only include countries with at least 1 election and 2 party observations
  filter(n_elections >= 1 & n_party_elections >= 2) %>%
  # Rank by mean standard deviation of vote share (volatility)
  arrange(desc(mean_sd_vote_share)) %>%
  mutate(rank = row_number())

# Table: Between district volatility + max district number (using 5% threshold)
table3_data <- country_volatility_5pct %>%
  select(ctr_n, n_elections, mean_sd_vote_share, max_n_districts) %>%
  arrange(desc(mean_sd_vote_share)) %>%
  rename(Country = ctr_n, `Number of Elections` = n_elections, `Between District Volatility (SD)` = mean_sd_vote_share, `Max Number of Districts` = max_n_districts) %>%
  mutate(`Between District Volatility (SD)` = round(`Between District Volatility (SD)`, 4),
         `Max Number of Districts` = round(`Max Number of Districts`, 1))

# Generate LaTeX table
cat("\n=== LATEX TABLE ===\n")

# Table: Between district volatility + max districts
cat("Table: Between District Volatility and Max Number of Districts (parties with ≥5% aggregate vote share)\n")
print(xtable(table3_data, caption = "Between District Volatility and Max Number of Districts by Country (Parties with ≥5% Aggregate Vote Share)", label = "tab:district_volatility"), 
      include.rownames = FALSE, booktabs = TRUE, caption.placement = "top")

# Save LaTeX table to file
writeLines(capture.output(print(xtable(table3_data, caption = "Between District Volatility and Max Number of Districts by Country (Parties with ≥5% Aggregate Vote Share)", label = "tab:district_volatility"), 
                                include.rownames = FALSE, booktabs = TRUE, caption.placement = "top")), 
           "tables/district_volatility_table.tex")

cat("\nAnalysis complete! Results saved to:\n")
cat("- data/out/party_volatility_ranking_parlgov.csv\n")
cat("- tables/district_volatility_table.tex\n")
cat("\nNote: Tables 1 and 2 (Average Number of Parties and National Vote Volatility) are now generated\n")
cat("      from ParlGov data in script 7_election_volatility_analysis.R\n")

# Compare with simulation parameters
cat("\n=== COMPARISON WITH SIMULATION PARAMETERS ===\n")
cat("CLEA Mean SD vote share range (0-1 format):", round(range(country_volatility$mean_sd_vote_share), 4), "\n")
cat("Simulation between_district_vote_volatility range: 0.002 to 0.03\n")
cat("Ratio of max CLEA to max simulation:", round(max(country_volatility$mean_sd_vote_share) / 0.03, 1), "x\n")
cat("Ratio of max CLEA to min simulation:", round(max(country_volatility$mean_sd_vote_share) / 0.002, 1), "x\n")
cat("Countries with volatility within simulation range:", 
    sum(country_volatility$mean_sd_vote_share <= 0.03), "out of", nrow(country_volatility), "\n")
cat("Average max number of districts per country:", round(mean(country_volatility$max_n_districts), 1), "\n")
cat("Range of max districts per country:", round(range(country_volatility$max_n_districts)), "\n")
