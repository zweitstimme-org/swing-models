# Election Volatility and Party Fragmentation Analysis
# Using ParlGov data to analyze electoral volatility and party system fragmentation
# in the last 20 years (2004-2024)

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(xtable)

# Load the ParlGov election data
election_data <- read_csv("data/view_election.csv", show_col_types = FALSE)

# Examine the data structure
cat("Data dimensions:", dim(election_data), "\n")
cat("Date range:", min(election_data$date), "to", max(election_data$date), "\n")
cat("Countries:", length(unique(election_data$country)), "\n")

# Filter data to last 20 years (2004-2024) and parliamentary elections only
recent_data <- election_data %>%
  filter(
    date >= as.Date("2004-01-01"),
    date <= as.Date("2024-12-31"),
    type == "parliament",
    !is.na(vote_share),
    vote_share > 0
  ) %>%
  mutate(
    year = year(date),
    country = as.factor(country),
    # Convert vote shares from percentage (0-100) to proportion (0-1)
    vote_share = vote_share / 100
  )

cat("Filtered data dimensions:", dim(recent_data), "\n")
cat("Countries in filtered data:", length(unique(recent_data$country)), "\n")
cat("Date range in filtered data:", min(recent_data$date), "to", max(recent_data$date), "\n")

# Limit to maximum 3 most recent elections per country
# First, identify distinct elections per country
country_elections <- recent_data %>%
  distinct(country, election_id, date) %>%
  arrange(country, desc(date)) %>%
  group_by(country) %>%
  slice_head(n = 3) %>%  # Keep only the 3 most recent elections per country
  ungroup()

# Filter recent_data to only include these elections
recent_data <- recent_data %>%
  semi_join(country_elections, by = c("country", "election_id"))

cat("After limiting to max 3 elections per country:\n")
cat("Filtered data dimensions:", dim(recent_data), "\n")
cat("Elections per country:\n")
print(recent_data %>%
  distinct(country, election_id, date) %>%
  group_by(country) %>%
  summarise(n_elections = n(), .groups = 'drop') %>%
  arrange(desc(n_elections)))

# Filter to parties with >= 5% vote share (0.05 in proportion format)
recent_data_5pct <- recent_data %>%
  filter(vote_share >= 0.05)

cat("\nAfter filtering to parties with >= 5% vote share:\n")
cat("Filtered data dimensions:", dim(recent_data_5pct), "\n")

# Calculate average number of parties per election by country (only parties >= 5%)
party_count_analysis <- recent_data_5pct %>%
  group_by(country, election_id) %>%
  summarise(
    n_parties = n(),
    .groups = 'drop'
  ) %>%
  group_by(country) %>%
  summarise(
    avg_parties_per_election = mean(n_parties, na.rm = TRUE),
    n_elections = n(),
    .groups = 'drop'
  )

# Calculate volatility using a simpler approach
# For each country, calculate the standard deviation of vote shares across elections
# Only include parties with >= 5% vote share
# First, get the number of distinct elections per country
country_election_counts <- recent_data_5pct %>%
  distinct(country, election_id) %>%
  group_by(country) %>%
  summarise(n_elections = n(), .groups = 'drop')

volatility_analysis <- recent_data_5pct %>%
  # Group by country and party to get vote share time series
  group_by(country, party) %>%
  summarise(
    vote_share_sd = sd(vote_share, na.rm = TRUE),
    n_observations = n(),
    .groups = 'drop'
  ) %>%
  # Group by country to get average volatility
  group_by(country) %>%
  summarise(
    volatility = mean(vote_share_sd, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Join with actual election counts
  left_join(country_election_counts, by = "country")

# Combine the analyses
final_analysis <- volatility_analysis %>%
  left_join(party_count_analysis, by = "country") %>%
  # Filter countries with at least 2 elections for meaningful volatility calculation
  filter(n_elections.x >= 2) %>%
  # Create rankings
  mutate(
    volatility_rank = rank(desc(volatility), ties.method = "min"),
    parties_rank = rank(desc(avg_parties_per_election), ties.method = "min")
  ) %>%
  arrange(volatility_rank)

# Display results
cat("\n=== ELECTION VOLATILITY RANKINGS (Last 20 Years) ===\n")
cat("Countries ranked by vote share volatility between elections\n\n")

print(final_analysis %>% 
  select(country, volatility, volatility_rank, avg_parties_per_election, parties_rank, n_elections.x) %>%
  arrange(volatility_rank))

# Create visualizations
# 1. Volatility ranking
p1 <- final_analysis %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(country, volatility), y = volatility)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Electoral Volatility by Country (2004-2024)",
    subtitle = "Standard deviation of party vote shares across elections (max 3 most recent per country)",
    x = "Country",
    y = "Volatility Score (Proportion Scale)",
    caption = "Based on ParlGov data, parliamentary elections only"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# 2. Party fragmentation ranking
p2 <- final_analysis %>%
  slice_head(n = 20) %>%
  ggplot(aes(x = reorder(country, avg_parties_per_election), y = avg_parties_per_election)) +
  geom_col(fill = "darkgreen", alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Average Number of Parties per Election (2004-2024)",
    subtitle = "Party system fragmentation (max 3 most recent elections per country)",
    x = "Country",
    y = "Average Number of Parties",
    caption = "Based on ParlGov data, parliamentary elections only"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# 3. Scatter plot: Volatility vs Party Fragmentation
p3 <- final_analysis %>%
  ggplot(aes(x = avg_parties_per_election, y = volatility)) +
  geom_point(aes(size = n_elections.x), alpha = 0.6, color = "darkred") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Electoral Volatility vs Party Fragmentation",
    subtitle = "Relationship between party system fragmentation and vote share volatility (max 3 most recent elections per country)",
    x = "Average Number of Parties per Election",
    y = "Volatility Score (Proportion Scale)",
    size = "Number of Elections",
    caption = "Based on ParlGov data, parliamentary elections only"
  ) +
  theme_minimal()

# Save plots
ggsave("figures/electoral_volatility_ranking.pdf", p1, width = 10, height = 8)
ggsave("figures/party_fragmentation_ranking.pdf", p2, width = 10, height = 8)
ggsave("figures/volatility_vs_fragmentation.pdf", p3, width = 10, height = 8)

# Save results
write_csv(final_analysis, "data/out/electoral_volatility_analysis.csv")

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Countries analyzed:", nrow(final_analysis), "\n")
cat("Mean volatility:", round(mean(final_analysis$volatility, na.rm = TRUE), 3), "\n")
cat("Mean parties per election:", round(mean(final_analysis$avg_parties_per_election, na.rm = TRUE), 2), "\n")
cat("Correlation between volatility and party fragmentation:", 
    round(cor(final_analysis$volatility, final_analysis$avg_parties_per_election, use = "complete.obs"), 3), "\n")

# Display top 10 most volatile countries
cat("\n=== TOP 10 MOST VOLATILE COUNTRIES ===\n")
print(final_analysis %>% 
  select(country, volatility, n_elections.x) %>%
  slice_head(n = 10))

# Display top 10 most fragmented party systems
cat("\n=== TOP 10 MOST FRAGMENTED PARTY SYSTEMS ===\n")
print(final_analysis %>% 
  select(country, avg_parties_per_election, n_elections.x) %>%
  arrange(desc(avg_parties_per_election)) %>%
  slice_head(n = 10))

# Generate LaTeX tables
cat("\n=== LATEX TABLES ===\n")

# Create country code to name mapping
country_names <- c(
  "AUS" = "Australia", "AUT" = "Austria", "BEL" = "Belgium", "BGR" = "Bulgaria",
  "CAN" = "Canada", "CHE" = "Switzerland", "CYP" = "Cyprus", "CZE" = "Czech Republic",
  "DEU" = "Germany", "DNK" = "Denmark", "ESP" = "Spain", "EST" = "Estonia",
  "FIN" = "Finland", "FRA" = "France", "GBR" = "UK", "GRC" = "Greece",
  "HRV" = "Croatia", "HUN" = "Hungary", "IRL" = "Ireland", "ISL" = "Iceland",
  "ISR" = "Israel", "ITA" = "Italy", "JPN" = "Japan", "LTU" = "Lithuania",
  "LUX" = "Luxembourg", "LVA" = "Latvia", "MLT" = "Malta", "NLD" = "Netherlands",
  "NOR" = "Norway", "NZL" = "New Zealand", "POL" = "Poland", "PRT" = "Portugal",
  "ROU" = "Romania", "SVK" = "Slovakia", "SVN" = "Slovenia", "SWE" = "Sweden",
  "TUR" = "Turkey"
)

# Table 1: Average number of parties per election
table1_data <- final_analysis %>%
  select(country, n_elections.x, avg_parties_per_election) %>%
  mutate(Country = country_names[as.character(country)]) %>%
  select(Country, `Number of Elections` = n_elections.x, `Average Number of Parties` = avg_parties_per_election) %>%
  relocate(Country, .before = everything()) %>%
  arrange(desc(`Average Number of Parties`)) %>%
  mutate(`Average Number of Parties` = round(`Average Number of Parties`, 1))

cat("Table 1: Average Number of Parties (≥5%)\n")
print(xtable(table1_data, caption = "Average Number of Parties per Election by Country (≥5% vote share)", label = "tab:avg_parties"), 
      include.rownames = FALSE, booktabs = TRUE, caption.placement = "top")

# Table 2: National vote volatility
table2_data <- final_analysis %>%
  select(country, n_elections.x, volatility) %>%
  mutate(Country = country_names[as.character(country)]) %>%
  select(Country, `Number of Elections` = n_elections.x, `National Vote Volatility` = volatility) %>%
  relocate(Country, .before = everything()) %>%
  arrange(desc(`National Vote Volatility`)) %>%
  mutate(`National Vote Volatility` = round(`National Vote Volatility`, 4))

cat("\nTable 2: National Vote Volatility (≥5%)\n")
print(xtable(table2_data, caption = "National Vote Volatility by Country (parties ≥5% vote share)", label = "tab:national_volatility"), 
      include.rownames = FALSE, booktabs = TRUE, caption.placement = "top")

# Save LaTeX tables to files
writeLines(capture.output(print(xtable(table1_data, caption = "Average Number of Parties per Election by Country (≥5% vote share)", label = "tab:avg_parties"), 
                                include.rownames = FALSE, booktabs = TRUE, caption.placement = "top")), 
           "tables/avg_parties_table.tex")

writeLines(capture.output(print(xtable(table2_data, caption = "National Vote Volatility by Country (parties ≥5% vote share)", label = "tab:national_volatility"), 
                                include.rownames = FALSE, booktabs = TRUE, caption.placement = "top")), 
           "tables/national_volatility_table.tex")

cat("\nLaTeX tables saved to:\n")
cat("- tables/avg_parties_table.tex\n")
cat("- tables/national_volatility_table.tex\n")