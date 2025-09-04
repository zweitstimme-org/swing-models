
###########################
# Prepare environment
###########################

source("00_Setup_Packages.R")

###########################
# SCRIPT Variables
###########################

# Party Names
party_names <- c("spd","cdu","gru","fdp","afd","lin","bsw")

# Election Dates
election_dates <- as.Date(c(
  "1953-09-06", "1957-09-15", "1961-09-17", "1965-09-19",
  "1969-09-28", "1972-11-19", "1976-10-03", "1980-10-05", "1983-03-06",
  "1987-01-25", "1990-12-02", "1994-10-16", "1998-09-27", "2002-09-22",
  "2005-09-18", "2009-09-27", "2013-09-22", "2017-09-24", "2021-09-26",
  "2025-02-23"
))

###########################
# Input data
###########################

# Load federal election results until 2017
load("in/data/polls_btw_wide.RData")

# Load Polls from Wahlrecht.de 
load("in/data/wahlrecht_polls_2024-11-22.RData")

# Load Structural Data
btw_elections <- readRDS("in/data/pre_train_data_25.rds")
rownames(btw_elections) <- NULL

###########################
# Polls data
###########################

# Load federal election results until 2017
polls_df_wide <- polls_df_wide %>% 
  mutate(bsw = NA) %>%
  select(institute, date, sample_size, all_of(party_names), election, election_date, days_to_election)

# Sect Polls form Wahlrecht.de 
wahlrecht_polls <- wahlrecht_polls %>% 
  filter(date > max(polls_df_wide$date)) %>%
  rowwise() %>%
  mutate(
    election_date = min(election_dates[election_dates >= date]),
    election = as.integer(format(election_date, "%Y")),
    days_to_election = difftime(election_date, date, units = "days")
  ) %>%
  ungroup() %>%
  select(institute, date, sample_size, all_of(party_names), election, election_date, days_to_election)

# Bind toegther
btw_polls <- bind_rows(polls_df_wide,wahlrecht_polls)  
rm(polls_df_wide,wahlrecht_polls)


# 
btw_polls <- btw_polls %>%
  mutate(
    date = as.Date(date),
    election_date = as.Date(election_date),
    days_to_election_num = as.numeric(days_to_election) # Convert to numeric
  )

# Function to filter data and calculate averages
aggregate_polls <- function(df, days_threshold) {
  df %>%
    filter(days_to_election_num <= days_threshold & days_to_election_num >= (days_threshold - 7)) %>%
    group_by(election, election_date) %>%
    summarise(
      spd = mean(spd, na.rm = TRUE),
      cdu = mean(cdu, na.rm = TRUE),
      gru = mean(gru, na.rm = TRUE),
      fdp = mean(fdp, na.rm = TRUE),
      afd = mean(afd, na.rm = TRUE),
      lin = mean(lin, na.rm = TRUE),
      bsw = mean(bsw, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = spd:bsw, names_to = "party", values_to = paste0("poll_value",days_threshold)) 
    
}

# Aggregate for 7 and 14 days
polls_7 <- aggregate_polls(btw_polls, 7) 
polls_14 <- aggregate_polls(btw_polls, 14)

# Combine and reshape into wide format
final_polls <- full_join(polls_7, polls_14, by = c("election", "election_date", "party")) 

###########################
# Election data Get latent Support
###########################


# Load DLM Results
rds_files <- list.files("/mnt/forecasts/dlm/", 
                        pattern = "res_dlm_elec_\\d{4}\\.RDS", 
                        full.names = TRUE)

# Get Results and save expected latent support
dlm_res <- lapply(rds_files, readRDS) %>%
  lapply(.,`[[`, 2) %>% 
  bind_rows()


# Results
dlm_m <- dlm_res %>%
  filter(days_prior_to_elec %in% c(366-7, 366-5, 366-2)) %>%  # Select relevant days
  select(party, value, election, days_prior_to_elec) %>%
  mutate(value = value * 100,  # Scale value
         election = as.double(election),
         days_prior_to_elec = case_when(
           days_prior_to_elec == (366 - 7) ~ "mt_7",
           days_prior_to_elec == (366 - 5) ~ "mt_5",
           days_prior_to_elec == (366 - 2) ~ "mt_2"
         )) %>%
  pivot_wider(names_from = days_prior_to_elec, values_from = value)


###########################
# Election data Bundestag
###########################

# BTW
btw_elec <- btw_elections %>%
  rowwise() %>%
  mutate(election_date = election_dates[election - 1],  # Extract election date
         election = as.integer(format(election_date, "%Y"))) %>%
  ungroup() %>%
  left_join(dlm_m, by = c("party", "election")) %>%  # Join all `mt_*` columns
  group_by(election) %>%
  mutate(
    sum_mt_7 = sum(mt_7, na.rm = TRUE),
    sum_mt_5 = sum(mt_5, na.rm = TRUE),
    sum_mt_2 = sum(mt_2, na.rm = TRUE)
  ) %>%
  mutate(
    mt_7 = case_when(party == "oth" ~ 100 - sum_mt_7, TRUE ~ mt_7),
    mt_5 = case_when(party == "oth" ~ 100 - sum_mt_5, TRUE ~ mt_5),
    mt_2 = case_when(party == "oth" ~ 100 - sum_mt_2, TRUE ~ mt_2)
  ) %>%
  ungroup()


###########################
# Wahlkreis data 
###########################

# BTW
wkr_data <- read.csv("in/data/btw_candidates_1983-2025_new.csv", stringsAsFactors = TRUE)
wkr_data <- wkr_data[,-1]

wkr_data %>% filter(election == 2025)
# Add wkr
wkr_data <- left_join(wkr_data, btw_elec, by=c("election","party"))


# BSW Data
wkr_data_bsw <- filter(wkr_data, election == 2025, party == "lin") %>% 
  select(wkr, wkr_name, party, res_l1_Z, res_l1_E, formercand , 
         east ,  female,  incumbent , akad , incumbent_in_wkr) %>%
  mutate(party = "bsw",
         formercand = 0,
         female = 0, 
         akad = 0,
         incumbent = 0,
         res_l1_Z = res_l1_Z/2,
         res_l1_E = res_l1_E/2)


wkr_data <- bind_rows(wkr_data, wkr_data_bsw) %>%
  mutate(voteshare_l1 =  case_when(party == "bsw" ~ 2.25,
                                   TRUE ~ voteshare_l1))



###########################
# Output data
###########################

save(btw_polls,btw_elec,wkr_data, file="in/model_data.RData")

# Rm
rm(list=ls())

