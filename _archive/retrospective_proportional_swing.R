# ----------------------------------------------------------
# Retrospective Proportional Swing Predictions (2009+)
# Predicts resp_E and resp_Z per wkr, party, and election
# using only proportional swing from last election and 2 months polling lead
# Trains lm on all data available before each election
# Authors: Lukas Stoetzer & Cornelius Erfort (adapted)
# ----------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ========== Source Utility Functions ==========
source("code/utils.R")

# ========== Load Data ==========
btw_candidates <- read.csv("data/btw_candidates_1983-2025_full.csv", stringsAsFactors = FALSE)

# Only keep elections since 1998 for training and lead calculation
elections_all <- c(1994, 1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)
# Only predict for these:
elections_predict <- c(2009, 2013, 2017, 2021, 2025)

btw_candidates <- btw_candidates %>% filter(election %in% elections_all)

# Load federal leads (polls_months, vote_share_l1)
federal_results <- read.csv("data/federal-election-results.csv") %>% 
  mutate(election = year(as.Date(electiondate))) %>% 
  filter(election %in% elections_all) %>% 
  filter(!(party %in% c("fw", "pir", "npd")))

# Add 2021 and 2025 manually if not present
if (!2021 %in% federal_results$election) {
  results_2021 <- data.frame(election = 2021, 
    party = c("cdu", "spd", "gru", "fdp", "lin", "afd", "bsw"),
    vote_share = c(24.2, 25.7, 14.7, 11.4, 4.9, 10.4, 4.9),
    electiondate = "2021-09-26")
  federal_results <- bind_rows(federal_results, results_2021)
}
if (!2025 %in% federal_results$election) {
  results_2025 <- data.frame(election = 2025, 
    party = c("cdu", "spd", "gru", "fdp", "lin", "afd", "bsw"),
    vote_share = c(28.5, 16.4, 11.6, 4.3, 8.8, 20.8, 4.97),
    electiondate = "2025-02-23")
  federal_results <- bind_rows(federal_results, results_2025)
}
federal_results <- federal_results %>% arrange(election, party) %>% 
  mutate(vote_share = vote_share/100,
         vote_share_l1 = vote_share_l1/100)

# Add 'oth' (or 'and') as a party for each election, with vote_share = 1 - sum(other parties)
for (el in unique(federal_results$election)) {
  parties_this_election <- federal_results %>% filter(election == el)
  sum_share <- sum(parties_this_election$vote_share, na.rm = TRUE)
  if (!any(parties_this_election$party %in% c('oth', 'and', 'sonstige', 'other'))) {
    # Use 'and' as in your candidate data
    new_row <- data.frame(
      election = el,
      party = 'oth',
      vote_share = 1 - sum_share,
      electiondate = unique(parties_this_election$electiondate)[1]
    )
    federal_results <- bind_rows(federal_results, new_row)
  }
}

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

# Add lagged vote share
federal_results <- federal_results %>% group_by(party) %>% arrange(election) %>%
  mutate(vote_share_l1 = lag(vote_share),
         electiondate_l1 = lag(electiondate)) %>% ungroup()

# Vote share l1 for bsw and lin party in 2025 should be 1/2 of lin in 2021
federal_results$vote_share_l1[federal_results$party == "bsw" & federal_results$election == 2025] <- 0.049
federal_results$electiondate_l1[federal_results$party == "bsw" & federal_results$election == 2025] <- "2021-09-26"
federal_results$electiondate_l1[federal_results$party == "bsw" & federal_results$election == 2025]


# Load or calculate federal_leads (polls_months)
federal_polls <- read.csv("data/germany-federal-polls.csv") %>% mutate(date = as.Date(date),
                                                                       poll_share = poll_share / 100) %>% unique %>% 
  filter(!is.na(poll_share))

# Only keep one obs per date and auftraggeber
federal_polls <- federal_polls %>%
  group_by(date, auftraggeber, party) %>%
  slice(1) %>%
  ungroup()

# Add var for total_vote by data and auftraggeber
federal_polls <- federal_polls %>%
  group_by(date, auftraggeber) %>%
  mutate(total_vote = sum(poll_share, na.rm = TRUE)) %>%
  ungroup()


# For all dates after 2021-09-26 and before or on 2025-02-23 make electiondate 2025-02-23, and electiondate_l1 2021-09-26
federal_polls <- federal_polls %>%
  mutate(electiondate = ifelse(date > as.Date("2021-09-26") & date <= as.Date("2025-02-23"), 
                               as.Date("2025-02-23"), electiondate),
         electiondate_l1 = ifelse(date > as.Date("2021-09-26") & date <= as.Date("2025-02-23"), 
                                  as.Date("2021-09-26"), electiondate_l1))
# federal_polls$electiondate_l1[federal_polls$party == "bsw"] <- ymd("2021-09-26")

# Calculate polls_months for each party/election (2 months before election)
federal_leads <- federal_results %>%
  filter(!is.na(electiondate_l1)) %>%
  mutate(electiondate = as.Date(electiondate),
         date_months = electiondate - months(2))

federal_leads$polls_months <- NA
for (i in 1:nrow(federal_leads)) {
  federal_leads$polls_months[i] <- get_latent_support(
    federal_polls, 
    federal_leads$party[i], 
    federal_leads$date_months[i], 
    as.Date(federal_leads$electiondate_l1[i]),
    "party", "date", "poll_share"
  )
}

# Fill polls_months for 'oth' (other parties) as 100 minus sum of all other parties for that election
grouped_elections <- unique(federal_leads$election)
for (el in grouped_elections) {
  # Find index for 'oth' (or similar, e.g. 'and', 'sonstige', etc.)
  idx_oth <- which(federal_leads$election == el & federal_leads$party %in% c('oth'))
  if (length(idx_oth) == 1) {
    idx_non_oth <- which(federal_leads$election == el & !(federal_leads$party %in% c('oth')))
    sum_other <- sum(federal_leads$polls_months[idx_non_oth], na.rm = TRUE)
    federal_leads$polls_months[idx_oth] <- 1 - sum_other
  }
}

# ========== Prepare District Data ==========
btw_candidates <- btw_candidates %>% arrange(election, wkr, party)
btw_candidates <- btw_candidates %>%
  group_by(wkr, party) %>%
  arrange(election) %>%
  mutate(election_l1 = lag(election)) %>%
  ungroup()

# Merge in federal_leads (proportional swing)
btw_candidates <- btw_candidates %>%
  left_join(federal_leads %>% select(election, party, polls_months, vote_share_l1),
            by = c("election", "party"))

# Calculate proportional swing (matching Lukas's approach)
btw_candidates <- btw_candidates %>%
  mutate(
    proportional = (polls_months - vote_share_l1) / vote_share_l1,
  )

# ========== Retrospective Model Training and Prediction ==========
results_list <- list()

# For oth, these variables are the mean across all oth candidates, set to zero if not 1
btw_candidates$formercand[btw_candidates$formercand > 0 & btw_candidates$formercand < 1] <- 0
btw_candidates$female[btw_candidates$female > 0 & btw_candidates$female < 1] <- 0
btw_candidates$akad[btw_candidates$akad > 0 & btw_candidates$akad < 1] <- 0

btw_candidates$election %>% table

# In the for loop, use elections_predict for prediction, but allow training on all previous years since 1998
for (target_election in elections_predict) {
  # Training data: all data before the target election, since 1998
  train <- btw_candidates %>% filter(election < target_election)
  test <- btw_candidates %>% filter(election == target_election)

  # Only keep rows with complete cases for training
  train_E <- train %>% filter(!is.na(resp_E), !is.na(res_l1_E), !is.na(proportional))
  train_Z <- train %>% filter(!is.na(resp_Z), !is.na(res_l1_Z), !is.na(proportional))

  # Fit models
  if (nrow(train_E) > 10) {
    model_E <- lm(resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1, data = train_E)
  } else {
    model_E <- NULL
  }
  if (nrow(train_Z) > 10) {
    model_Z <- lm(resp_Z ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1, data = train_Z)
  } else {
    model_Z <- NULL
  }

  # Predict for test set
  test <- test %>% mutate(
    predicted_resp_E = if (!is.null(model_E)) predict(model_E, newdata = test) else NA,
    predicted_resp_Z = if (!is.null(model_Z)) predict(model_Z, newdata = test) else NA
  )
  test$predicted_resp_E
  
  results_list[[as.character(target_election)]] <- test %>%
    select(election, wkr, party, predicted_resp_E, predicted_resp_Z, resp_E, resp_Z, res_l1_E, res_l1_Z)
}

# Combine all results
output <- bind_rows(results_list)

# Set lower than zero to zero
output <- output %>%
  mutate(pred_E = ifelse(predicted_resp_E < 0, 0, predicted_resp_E),
         pred_Z = ifelse(predicted_resp_Z < 0, 0, predicted_resp_Z))

# For 'oth', set to 1 - sum of others for each year and district
output <- output %>%
  group_by(election, wkr) %>%
  mutate(
    pred_E = ifelse(party == "oth", 1 - sum(pred_E[party != "oth"], na.rm = TRUE), pred_E),
    pred_Z = ifelse(party == "oth", 1 - sum(pred_Z[party != "oth"], na.rm = TRUE), pred_Z)
  ) %>%
  ungroup()

# Scaling: ensure sum to 1 within each (year, district)
output <- output %>%
  group_by(election, wkr) %>%
  mutate(pred_E = pred_E / sum(pred_E, na.rm = TRUE),
         pred_Z = pred_Z / sum(pred_Z, na.rm = TRUE)) %>%
  ungroup()

# Rename columns for clarity
tidy_output <- output %>%
  rename(
    year = election,
    district = wkr,
    pred_E = pred_E,
    pred_Z = pred_Z,
    actual_E = resp_E,
    actual_Z = resp_Z,
    lag_E = res_l1_E,
    lag_Z = res_l1_Z
  ) %>%
  select(year, district, party, pred_E, pred_Z, actual_E, actual_Z, lag_E, lag_Z)

# Save to CSV
write.csv(tidy_output, "data/out/retrospective_proportional_swing_predictions.csv", row.names = FALSE)

cat("Retrospective proportional swing predictions (with lm) saved to data/out/retrospective_proportional_swing_predictions.csv\n") 