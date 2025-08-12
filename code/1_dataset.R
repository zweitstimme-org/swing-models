# ----------------------------------------------------------
# District-Level Swing Model Project
# Authors: Lukas Stoetzer & Cornelius Erfort
# ----------------------------------------------------------

# ========== Libraries ==========
# Load required libraries for data manipulation, modeling, and reporting
suppressPackageStartupMessages({
  library(lubridate)
  library(dlm)
  library(xml2)
  library(xtable)
  library(tidyverse)

  # ... add others as needed
})

# ========== Source Utility Functions ==========
# Source custom utility functions from the utils.R script
source("code/0_utils.R")

# ========== Data Loading ==========

# Load candidate-level data for all federal elections 1983-2025
btw_candidates_1983_2025 <- read.csv("data/btw_candidates_1983-2025_full.csv", stringsAsFactors = FALSE)

# Check column names (for debugging/inspection)
btw_candidates_1983_2025 %>% names

# Load preliminary 2025 district-level results (raw, as provided by source)
district_results_raw <- read_delim(
  "data/btw_results_2025.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE, 
  skip = 9
)

# Process 2025 district results: filter, select, pivot, and standardize party names
# Also convert vote shares to numeric proportions
# Output: one row per (wkr, party, election)
district_results_2025 <- district_results_raw %>% 
  filter(Gebietsart == "Wahlkreis" & Gruppenart != "System-Gruppe") %>% 
  dplyr::select(Stimme, Prozent, Gebietsnummer, Gruppenname) %>% 
  # Pivot to wide format: columns for each Stimme (1st/2nd vote)
  pivot_wider(
    names_from = Stimme, 
    values_from = Prozent
  ) %>%
  mutate(
    resp_E = (`1` %>% str_replace("\\,", ".") %>% as.numeric)/100, # Erststimme
    resp_Z = (`2` %>% str_replace("\\,", ".") %>% as.numeric)/100, # Zweitstimme
    party = Gruppenname %>% str_replace_all(c(
      "CDU" = "cdu", 
      "CSU" = "cdu", 
      "GRÜNE" = "gru", 
      "SPD" = "spd", 
      "AfD" = "afd", 
      "Die Linke" = "lin", 
      "FDP" = "fdp", 
      "BSW" = "bsw"
    )),
    wkr = Gebietsnummer %>% as.numeric()
  ) %>% dplyr::select(-c(`1`, `2`, Gebietsnummer, Gruppenname)) %>% 
  mutate(election = 2025) %>% 
  dplyr::select(wkr, party, resp_E, resp_Z, election)


# Merge 2025 results into candidate dataset (update resp_E/resp_Z for 2025)
# Mark winner in each district (highest Erststimme)
btw_candidates_updated <- btw_candidates_1983_2025 %>%
  left_join(
    district_results_2025,
    by = c("wkr", "party", "election")
  ) %>%
  mutate(
    resp_E = coalesce(resp_E.x, resp_E.y),
    resp_Z = coalesce(resp_Z.x, resp_Z.y)
  ) %>%
  dplyr::select(-resp_E.x, -resp_E.y, -resp_Z.x, -resp_Z.y) %>% 
  group_by(election, wkr) %>%
  mutate(winner = resp_E == max(resp_E, na.rm = T))

# TODO: Recalculate res_Z_l1 and res_E_l1 by party, election, and wkr if needed

# ========== Historical Election Results ==========
# National-level party results for recent elections (for reference/comparison)
res25 <- c(28.5, 16.4, 8.8,  11.6, 4.3, 20.8, 4.97, 4.5) / 100
res21 <- c(24.2, 25.7, 2.45*2, 14.7, 11.4, 10.4, 2.45*2, 8.7) / 100
res17 <- c(32.9, 20.5, 4.6, 8.9, 10.7, 12.6, 4.6, 5.9) / 100
res13 <- c(41.5, 25.7, 4.3, 8.4, 4.8, 4.7, 4.3, 6.3) / 100
btw_bund_res <- rbind(res21, res17, res13)
colnames(btw_bund_res) <- c("CDU/CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "BSW", "And")
rownames(btw_bund_res) <- c("2021", "2017", "2013")

### Calculate predictions for lead times -----------------------
# (2 months, 2 weeks, 2 days before election)

# ========== Poll Data Loading and Processing ==========
# Load federal polling data (long format, drop unused columns)
federal_polls <- read.csv("data/germany-federal-polls.csv") %>% mutate(date = as.Date(date)) %>% 
  dplyr::select(-c(electiondate_lead1, electiondate_l1, vote_share, vote_share_l1))

# Optionally: add new polls (from web scraping or other sources)
# new_polls <- get_wahlrecht_polls()
add_polls <- pivot_longer(new_polls, cols = c(bsw, cdu, spd, gru, fdp, lin, afd), names_to = "party", values_to = "poll_share")
add_polls <- filter(add_polls, date > max(federal_polls$date, na.rm = T))

# Combine old and new polls, standardize, and filter
federal_polls <- bind_rows(federal_polls, add_polls) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(poll_share)) %>% 
  mutate(poll_share = poll_share / 100)

# Add election date for each poll (based on date ranges)
federal_polls$electiondate[federal_polls$date > ymd("2021-09-26") & federal_polls$date < ymd("2025-02-23")] <- "2025-02-23"
# federal_polls$electiondate_l1[federal_polls$date > ymd("2021-09-26") & federal_polls$date < ymd("2025-02-23")] <- "2021-09-26"
federal_polls$electiondate[federal_polls$date > ymd("2017-09-24") & federal_polls$date < ymd("2021-09-26")] <- "2021-09-26"
# federal_polls$electiondate_lead1[federal_polls$date > ymd("2017-09-24") & federal_polls$date < ymd("2021-09-26")] <- "2025-02-23"
# federal_polls$electiondate_l1[federal_polls$date > ymd("2017-09-24") & federal_polls$date < ymd("2021-09-26")] <- "2017-09-24"

# ========== Federal Election Results (for lagged vote share) ==========
# Load and process official federal election results (for all elections < 2021)
federal_results <- read.csv("data/federal-election-results.csv") %>% 
  mutate(election = year(electiondate) %>% as.numeric) %>% 
  filter(election < 2021) %>% 
  filter(!(party %in% c("fw", "pir", "npd")))

federal_results %>% names

# Add 2021 and 2025 results manually (for completeness)
results_2025 <- data.frame(election = 2025, 
                           party = c("cdu", "spd", "gru", "fdp", "lin", "afd", "bsw"),
                           vote_share = c(28.5, 16.4, 11.6, 4.3, 8.8, 20.8, 4.97),
                           electiondate = "2025-02-23")

results_2021 <- data.frame(election = 2021, 
                           party = c("cdu", "spd", "gru", "fdp", "lin", "afd", "bsw"),
                           vote_share = c(24.2, 25.7, 14.7, 11.4, 2.45*2, 10.4, 2.45*2),
                           electiondate = "2021-09-26")

federal_results <- federal_results %>%
  bind_rows(results_2021, results_2025) %>%
  arrange(election, party) %>% 
  mutate(vote_share = vote_share/100)

# Add lagged and lead vote shares and election dates by party
federal_results <- federal_results %>%
  group_by(party) %>%
  arrange(election) %>%
  mutate(
    vote_share_l1 = lag(vote_share),
    vote_share_lead1 = lead(vote_share),
    electiondate_l1 = lag(electiondate),
    electiondate_lead1 = lead(electiondate)
  ) %>%
  ungroup() %>% 
  filter(!is.na(vote_share))

# Merge federal results into poll data, clean up columns
federal_polls <- merge(federal_polls, federal_results, by = c("electiondate", "party"), all = T) %>% 
  mutate(auftraggeber = coalesce(auftraggeber, institute)) %>%
  filter(!is.na(date) & !is.na(vote_share)) %>% 
  dplyr::select(-c(id, ag, typ, reg, institute, sample_size, rnd, beg, end, meth, stat, stand)) %>% 
  arrange(date)

###########################
# Transformations
###########################

# ========== Calculate Polling Leads for Each Election ==========
# Prepare a table of lead times (2 months, 2 weeks, 2 days before each election)
# for each party, with corresponding poll and result values
federal_leads <- federal_results %>% dplyr::select(c(party, electiondate, electiondate_l1, vote_share, vote_share_lead1, vote_share_l1)) %>% unique
federal_leads$party %>% table

# Convert date columns to Date type
federal_leads$electiondate <- ymd(federal_leads$electiondate)
federal_leads$electiondate_l1 <- ymd(federal_leads$electiondate_l1)

# Calculate cutoff dates for polling leads
federal_leads$date_days <- federal_leads$electiondate - days(2)
federal_leads$date_weeks <- federal_leads$electiondate - weeks(2)
federal_leads$date_months <- federal_leads$electiondate - months(2)

# If months lead is NA (e.g. for early elections), use 8 weeks as fallback
federal_leads$date_months[is.na(federal_leads$date_months)] <- federal_leads$electiondate[is.na(federal_leads$date_months)] - weeks(8)

# Only keep elections for which we have polling data and a previous election
federal_leads <- filter(federal_leads, date_months >= min(federal_polls$date, na.rm = T)) %>% 
  filter(!is.na(electiondate_l1))

# Initialize columns for polling support at each lead time
federal_leads$polls_days <- NA
federal_leads$polls_weeks <- NA
federal_leads$polls_months <- NA
federal_leads$calculated <- F

# Calculate polling support for each party at each lead time using get_latent_support()
for (i in 1:nrow(federal_leads)) {
  if(federal_leads$calculated[i]) next
  print(i)
  federal_leads$polls_days[i] <- get_latent_support(federal_polls, federal_leads$party[i], federal_leads$date_days[i], federal_leads$electiondate_l1[i], "party", "date", "poll_share")
  federal_leads$polls_weeks[i] <- get_latent_support(federal_polls, federal_leads$party[i], federal_leads$date_weeks[i], federal_leads$electiondate_l1[i], "party", "date", "poll_share")
  federal_leads$polls_months[i] <- get_latent_support(federal_polls, federal_leads$party[i], federal_leads$date_months[i], federal_leads$electiondate_l1[i], "party", "date", "poll_share")
  federal_leads$calculated[i] <- T
}

# Inspect summary statistics for calculated polling leads
federal_leads$polls_days %>% summary
federal_leads$polls_weeks %>% summary
federal_leads$polls_months %>% summary

federal_leads$party %>% table

# Optionally: recode party names for reporting
# federal_leads <- federal_leads %>% mutate(party = case_when(
#   party == "cdu" ~ "CDU/CSU",
#   party == "spd" ~ "SPD",
#   party == "gru" ~ "GRUENE",
#   party == "fdp" ~ "FDP",
#   party == "lin" ~ "LINKE",
#   party == "afd" ~ "AFD",
#   party == "bsw" ~ "BSW"
# ))
federal_leads$election <- federal_leads$electiondate %>% year %>% as.numeric

# ========== Add 'oth' (Other) Party for Each Election ==========
# For each election, add a row for 'oth' (other parties) so that party shares sum to 1
for (el in unique(federal_results$election)) {
  parties_this_election <- federal_results %>% filter(election == el)
  sum_share <- sum(parties_this_election$vote_share, na.rm = TRUE)
  if (!any(parties_this_election$party == 'oth')) {
    new_row <- data.frame(
      election = el,
      party = 'oth',
      vote_share = 1 - sum_share,
      electiondate = unique(parties_this_election$electiondate)[1]
    )
    federal_results <- bind_rows(federal_results, new_row)
  }
}

# After calculating polls_days, polls_weeks, and polls_months for all other parties:
# Fill in 'oth' polling support as 1 minus the sum of other parties' support
# (ensures all party shares sum to 1 for each election)
grouped_elections <- unique(federal_leads$election)
for (el in grouped_elections) {
  idx_oth <- which(federal_leads$election == el & federal_leads$party == 'oth')
  if (length(idx_oth) == 1) {
    idx_non_oth <- which(federal_leads$election == el & federal_leads$party != 'oth')
    sum_days <- sum(federal_leads$polls_days[idx_non_oth], na.rm = TRUE)
    sum_weeks <- sum(federal_leads$polls_weeks[idx_non_oth], na.rm = TRUE)
    sum_months <- sum(federal_leads$polls_months[idx_non_oth], na.rm = TRUE)
    federal_leads$polls_days[idx_oth] <- 1 - sum_days
    federal_leads$polls_weeks[idx_oth] <- 1 - sum_weeks
    federal_leads$polls_months[idx_oth] <- 1 - sum_months
  }
}

# Merge polling leads into candidate-level data
btw_candidates_1983_2025 <- merge(btw_candidates_updated, federal_leads, 
                                  by = c("party", "election"), all.x = TRUE)


### Calculate swings -----------------------
# Merge swing data (from swing_df) and calculate proportional/uniform swings
btw_candidates_1983_2025 <- merge(btw_candidates_1983_2025, swing_df, 
                                  by = c("election", "party"), all.x = TRUE)

btw_candidates_1983_2025$proportional <- (btw_candidates_1983_2025$polls_days / btw_candidates_1983_2025$vote_share_l1)
btw_candidates_1983_2025$uniform <- (btw_candidates_1983_2025$polls_days - btw_candidates_1983_2025$vote_share_l1)

btw_candidates_1983_2025$res_l1_Z_uniform <- btw_candidates_1983_2025$res_l1_Z + btw_candidates_1983_2025$uniform

### Prepare Training and Test Data -----------------------
# Remove East German districts for 1990 (not comparable, no prior l1 election results)
btw_candidates_1983_2025$election %>% unique
btw_candidates_1983_2025 <- filter(btw_candidates_1983_2025, !(election == 1990 & east == 1))

# Save processed candidate-level data for modeling/analysis
 
save(btw_candidates_1983_2025, file = "data/btw_candidates_1983-2025.RData")


