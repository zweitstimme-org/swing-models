### ----------------------------------------------------------
### District-Level Swing Model Project
### Authors: Lukas Stoetzer & Cornelius Erfort
### ----------------------------------------------------------
# # Get Latnet Support using random walk model ==== 
get_latent_support <- function(data, party, date, last_election_date,
                               var_party, var_date, var_percent) {
  
  #
  # Reuires DLM
  require(dlm)
  
  # Convert date column to Date class if not already
  data[[var_date]] <- as.Date(data[[var_date]])
  
  # No data after date
  filtered_data <- data[data[[var_party]] == party & data[[var_date]] < date, ]
  
  # Ensure the data is sorted by date
  filtered_data <- filtered_data[order(filtered_data[[var_date]]), ]
  
  # Times series format from last elction to date
  complete_data <- data.frame(Datum = seq.Date(from = last_election_date, to = date, by = "day"))
  
  merged_data <- merge(complete_data, filtered_data, by.x = "Datum", by.y = "date", all.x = TRUE)
  
  # Create Time-series data with missing values
  start_date <- min(merged_data$Datum)
  ts_data <- ts(merged_data$poll_share, start = c(as.numeric(format(start_date, "%Y")),
                                                  as.numeric(format(start_date, "%j"))), frequency = 365)
  
  # Define the DLM model with a random walk component
  build_dlm <- function(param) {
    dlmModPoly(order = 1, dV = exp(param[1]), dW = exp(param[2]))
  }
  
  # Fit the model using Maximum Likelihood Estimation
  fit <- dlmMLE(ts_data, parm = c(0, 0), build = build_dlm)
  
  # Build the final model with estimated parameters
  model <- build_dlm(fit$par)
  
  # Apply the Kalman filter to estimate the latent support
  filtered <- dlmFilter(ts_data, model)
  
  # Apply smoothing to get a refined estimate of the latent support
  smoothed <- dlmSmooth(filtered)
  latent_support <- smoothed$s[-1]  # Remove the first NA value
  
  # Return the latent support for the specified date
  if (nrow(filter(filtered_data, !is.na(poll_share))) > 1) return(latent_support[length(latent_support)]) else return(NA)
}


# Function to extract data from a single XML document
wahlrecht_xml_extract <- function(xml_url) {
  require(xml2)
  
  # xml_url <- "https://www.wahlrecht.de/umfragen/xml/bundestag_archiv.xml"
  
  # Load the XML document as a list
  # xml_url <- "https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml"
  xml_doc <- as_list(read_xml(xml_url))
  
  # Unnest the <umfragen> node to get individual <umfrage> entries
  xml_df <- as_tibble(xml_doc) %>% unnest_wider(umfragen) %>% 
    unnest_longer(werte) %>% 
    unnest(id) %>% 
    unnest(typ) %>% 
    unnest(reg) %>% 
    unnest(dat) %>% 
    unnest(dat) %>% 
    unnest(inst) %>% 
    unnest(inst) %>% 
    unnest(werte) %>% 
    unnest(werte) %>% 
    unnest(bfrg) %>% 
    unnest(bfrg) %>% 
    unnest(rnd) %>% 
    unnest(rnd) %>% 
    unnest(beg) %>% 
    unnest(beg) %>% 
    unnest(bfrg) %>% 
    unnest(bfrg) %>% 
    unnest(end) %>% 
    unnest(end) %>% 
    unnest(meth) %>% 
    unnest(meth) %>% 
    # unnest(agart) %>% 
    # unnest(agart) %>% 
    unnest(stat) %>% 
    unnest(stat) %>% 
    unnest(stand) %>% 
    unnest(stand) %>% 
    mutate(werte_id = case_when(werte_id == "grn" ~ "gru",
                                werte_id == "cxu" ~ "cdu",
                                werte_id == "lnk" ~ "lin",
                                werte_id == "fpd" ~ "fdp",
                                T ~ werte_id),
           werte = as.numeric(werte)
    ) %>%
    filter(!(werte_id %in% c("son", "frw"))) %>% 
    dplyr::rename(institute = inst, date = dat, party = werte_id, value = werte, sample_size = bfrg) %>% 
    as.data.frame %>% 
    mutate(date = as.Date(date)) %>%
    pivot_wider(values_from = "value", names_from = "party") 
  
  if("agart" %in% names(xml_df)) xml_df <- dplyr::select(xml_df, -agart)
  
  # unnest_wider(ag) %>% 
  
  # unnest(ag1) %>% 
  #   unnest(ag1) %>% 
  #   unnest(ag2) %>% 
  #   unnest(ag2) %>% 
  
  
  
  xml_df
}

get_wahlrecht_polls <- function() {
  
  # down <- get_surveys()
  # xml_url <- "https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml"
  
  new_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml") %>% wahlrecht_xml_extract
  old_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_archiv.xml") %>% wahlrecht_xml_extract
  
  old_polls$date %>% min
  
  wahlrecht_polls <- bind_rows(new_polls, old_polls)
  
  
  # sample_size
  wahlrecht_polls <- wahlrecht_polls %>% 
    # unnest(surveys) %>% 
    # unnest(survey) %>% 
    # select(institut = pollster, date, party, poll_share = percent, sample_size = respondents) %>%
    # mutate(date = ymd(date),
    #        party = case_when(party == "greens" ~ "gru",
    #                          party == "left" ~ "lin",
    #                          party == "others" ~ "oth",
    #                          TRUE ~ party)) %>%     
    # pivot_wider(names_from = party, values_from = poll_share) %>%
    pivot_longer(cols = cdu:afd, names_to = "party", values_to = "poll_share") %>% 
    # rename(institute = institut) %>% 
    pivot_wider(names_from = party, values_from = poll_share, values_fn = mean) 
  
  
  # If lin is not NA, subtract form oth
  # wahlrecht_polls <- wahlrecht_polls %>% 
  #   mutate(oth = ifelse(!is.na(lin), oth - lin, oth),
  #          oth = ifelse(!is.na(bsw), oth - bsw, oth),
  #          oth = ifelse(!is.na(fdp), oth - fdp, oth))
  
  # Arrange by date, decreasing
  wahlrecht_polls <- wahlrecht_polls %>% arrange(desc(date))
  
  wahlrecht_polls$sample_size <- as.numeric(wahlrecht_polls$sample_size)
  
  return(wahlrecht_polls)
}

library(tidyverse)

### Load and Process Candidate Data ----------------------

# Load historical candidate data
btw_candidates_1983_2025 <- read.csv("data/btw_candidates_1983-2025_full.csv", stringsAsFactors = FALSE)

# Filter out the 2025 election (no results)
btw_candidates_1983_2025 %>% names

# Load 2025 results
district_results_raw <- read_delim(
  "data/btw_results_2025.csv", 
  delim = ";", 
  escape_double = FALSE, 
  trim_ws = TRUE, 
  skip = 9
)

# Process district results
district_results_2025 <- district_results_raw %>% 
  filter(Gebietsart == "Wahlkreis" & Gruppenart != "System-Gruppe") %>% 
  select(Stimme, Prozent, Gebietsnummer, Gruppenname) %>% 
  # Pivot wider by Stimme
  pivot_wider(
    names_from = Stimme, 
    values_from = Prozent
  ) %>%
  mutate(
    resp_E = (`1` %>% str_replace("\\,", ".") %>% as.numeric)/100,
    resp_Z = (`2` %>% str_replace("\\,", ".") %>% as.numeric)/100,
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
  ) %>% select(-c(`1`, `2`, Gebietsnummer, Gruppenname)) %>%
  group_by(wkr) %>%
  mutate(winner = resp_E == max(resp_E, na.rm = T)) %>% 
  select(wkr, party, resp_E, resp_Z, winner) %>% 
  mutate(election = 2025)

# Now: Left join to update the candidate dataset
btw_candidates_updated <- btw_candidates_1983_2025 %>%
  left_join(
    district_results_2025,
    by = c("wkr", "party", "election")
  ) %>%
  mutate(
    resp_E = coalesce(resp_E.x, resp_E.y),
    resp_Z = coalesce(resp_Z.x, resp_Z.y),
    winner = coalesce(winner.x, winner.y)
  ) %>%
  select(-resp_E.x, -resp_E.y, -resp_Z.x, -resp_Z.y, -winner.x, -winner.y)

# Recalculate res_Z_l1 and res_E_l1 by party and election and wkr


# Load historical election results
res25 <- c(28.5, 16.4, 8.8,  11.6, 4.3, 20.8, 4.97, 4.5) / 100
res21 <- c(24.2, 25.7, 2.45*2, 14.7, 11.4, 10.4, 2.45*2, 8.7) / 100
res17 <- c(32.9, 20.5, 4.6, 8.9, 10.7, 12.6, 4.6, 5.9) / 100
res13 <- c(41.5, 25.7, 4.3, 8.4, 4.8, 4.7, 4.3, 6.3) / 100
btw_bund_res <- rbind(res21, res17, res13)
colnames(btw_bund_res) <- c("CDU/CSU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "BSW", "And")
rownames(btw_bund_res) <- c("2021", "2017", "2013")

### Calculate predictions for lead times -----------------------
# 2 months, 2 weeks, 2 days


# Load data for polls (includes election data)
federal_polls <- read.csv("data/germany-federal-polls.csv") %>% mutate(date = as.Date(date)) %>% 
  select(-c(electiondate_lead1, electiondate_l1, vote_share, vote_share_l1))

# new_polls <- get_wahlrecht_polls()
add_polls <- pivot_longer(new_polls, cols = c(bsw, cdu, spd, gru, fdp, lin, afd), names_to = "party", values_to = "poll_share")
add_polls <- filter(add_polls, date > max(federal_polls$date, na.rm = T))

federal_polls <- bind_rows(federal_polls, add_polls) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(poll_share))

federal_polls <- federal_polls %>%
  group_by(party) %>%
  arrange(date) %>%
  ungroup() %>% 
  filter(!is.na(poll_share))

# Add election date
federal_polls$electiondate[federal_polls$date > ymd("2021-09-26") & federal_polls$date < ymd("2025-02-23")] <- "2025-02-23"
# federal_polls$electiondate_l1[federal_polls$date > ymd("2021-09-26") & federal_polls$date < ymd("2025-02-23")] <- "2021-09-26"
federal_polls$electiondate[federal_polls$date > ymd("2017-09-24") & federal_polls$date < ymd("2021-09-26")] <- "2021-09-26"
# federal_polls$electiondate_lead1[federal_polls$date > ymd("2017-09-24") & federal_polls$date < ymd("2021-09-26")] <- "2025-02-23"
# federal_polls$electiondate_l1[federal_polls$date > ymd("2017-09-24") & federal_polls$date < ymd("2021-09-26")] <- "2017-09-24"

# Add federal election results to add lagged vote share
federal_results <- read.csv("data/federal-election-results.csv") %>% 
  mutate(election = year(electiondate) %>% as.numeric) %>% 
  filter(election < 2021)

federal_results %>% names

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

# Add vote share to federal polls
federal_polls <- merge(federal_polls, federal_results, by = c("electiondate", "party"), all = T) %>% 
  mutate(auftraggeber = coalesce(auftraggeber, institute)) %>%
  filter(!is.na(date) & !is.na(vote_share)) %>% 
  select(-c(id, ag, typ, reg, institute, sample_size, rnd, beg, end, meth, stat, stand)) %>% 
  arrange(date)

###########################
# Transformations
###########################


# Make df to calculate leads
# We take the election dates to calculate federal polling leads
federal_leads <- federal_results %>% select(c(party, electiondate, electiondate_l1, vote_share, vote_share_lead1, vote_share_l1)) %>% unique
federal_leads$party %>% table

# Make dates
federal_leads$electiondate <- ymd(federal_leads$electiondate)
federal_leads$electiondate_l1 <- ymd(federal_leads$electiondate_l1)

# Add cutoff days for leads
federal_leads$date_days <- federal_leads$electiondate - days(2)
federal_leads$date_weeks <- federal_leads$electiondate - weeks(2)
federal_leads$date_months <- federal_leads$electiondate - months(2)

# Correct NAs in months lead date
federal_leads$date_months[is.na(federal_leads$date_months)] <- federal_leads$electiondate[is.na(federal_leads$date_months)] - weeks(8)

# Only keep days in leads where we have polls
federal_leads <- filter(federal_leads, date_months >= min(federal_polls$date, na.rm = T)) %>% 
  filter(!is.na(electiondate_l1))

# Add columns for polls
federal_leads$polls_days <- NA
federal_leads$polls_weeks <- NA
federal_leads$polls_months <- NA
federal_leads$calculated <- F

# state_polls$date <- as.numeric(state_polls$date)

# Calculate leads using get_latent_support function
for (i in 1:nrow(federal_leads)) {
  if(federal_leads$calculated[i]) next
  print(i)
  federal_leads$polls_days[i] <- get_latent_support(federal_polls, federal_leads$party[i], federal_leads$date_days[i], federal_leads$electiondate_l1[i], "party", "date", "poll_share")
  federal_leads$polls_weeks[i] <- get_latent_support(federal_polls, federal_leads$party[i], federal_leads$date_weeks[i], federal_leads$electiondate_l1[i], "party", "date", "poll_share")
  federal_leads$polls_months[i] <- get_latent_support(federal_polls, federal_leads$party[i], federal_leads$date_months[i], federal_leads$electiondate_l1[i], "party", "date", "poll_share")
  federal_leads$calculated[i] <- T
}


federal_leads$polls_days %>% summary
federal_leads$polls_weeks %>% summary
federal_leads$polls_months %>% summary

federal_leads$party %>% table

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


btw_candidates_1983_2025 <- merge(btw_candidates_updated, federal_leads, 
                                  by = c("party", "election"), all.x = TRUE)


### Calculate swings -----------------------

btw_candidates_1983_2025 <- merge(btw_candidates_1983_2025, swing_df, 
                                  by = c("election", "party"), all.x = TRUE)

btw_candidates_1983_2025$proportional <- (btw_candidates_1983_2025$polls_days / btw_candidates_1983_2025$vote_share_l1) / 100
btw_candidates_1983_2025$uniform <- (btw_candidates_1983_2025$polls_days - btw_candidates_1983_2025$vote_share_l1) / 100

btw_candidates_1983_2025$res_l1_Z_uniform <- btw_candidates_1983_2025$res_l1_Z + btw_candidates_1983_2025$uniform

### Prepare Training and Test Data -----------------------
btw_candidates_1983_2025$election %>% unique
btw_candidates_1983_2025 <- filter(btw_candidates_1983_2025, !(election == 1990 & east == 1))

# Check this
btw_candidates_1983_2025$formercand[btw_candidates_1983_2025$formercand > 0 & btw_candidates_1983_2025$formercand < 1] <- 0
btw_candidates_1983_2025$female[btw_candidates_1983_2025$female > 0 & btw_candidates_1983_2025$female < 1] <- 0
btw_candidates_1983_2025$akad[btw_candidates_1983_2025$akad > 0 & btw_candidates_1983_2025$akad < 1] <- 0

for (this_election in c(1998, 2002, 2005, 2009, 2013, 2017, 2021, 2025)) {
  print(this_election)
  # election <- 2025
  election_l1 <- this_election - 4
  
  # Split data
  train <- btw_candidates_1983_2025 %>%
    filter(
      (election != this_election),
      partei != "AND"
    )
  
  test <- dplyr::filter(btw_candidates_1983_2025, election == this_election)
  ### Train Linear Model ----------------------------------
  
  
  
  # Define formula
  
  # model_formula <- "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1" # 28 wrong in 2021
  # model_formula <- "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z + uniform + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1" # 73 wrong in 2025
  # model_formula <- "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z*proportional + res_l1_Z*I(proportional^2)  + uniform + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1" # 31 wrong in 2025
  # model_formula <- "resp_E ~ ncand + propPlatz + alsoList + res_l1_E*proportional + res_l1_Z + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1" # 34 wrong in 2021
  # model_formula <- "resp_E ~ ncand + propPlatz + alsoList + res_l1_E*proportional + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1" # 28 wrong in 2021
  model_formula <- "resp_E ~ ncand + propPlatz + alsoList + res_l1_E + res_l1_Z + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1" # 28 wrong in 2021
  
  
  # Fit model
  reg <- lm(model_formula, data = train)
  
  summary(reg)
  
  
  # Predict on test data
  test$predicted <- predict(reg, newdata = test)
  test <- test %>% group_by(wkr) %>%  mutate(winner_pred = ifelse(predicted == max(predicted, na.rm = TRUE), 1, 0))
  
  table(test$winner, test$winner_pred) %>% print
}


