# Packages needed
p_needed <- c("tidyverse","brms","xml2","rstan")

# Install (if necessary)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install, dependencies = TRUE)
}

# Print Statement  
print(sapply(p_needed, require, character.only = TRUE)) %in% packages

# 
select <- dplyr::select

# Remove
rm(p_needed,packages,p_to_install)

# Co lour schemese
party_colors <- c(
  "cdu" = "#000000",   # Black
  "spd" = "#E3000F",   # Red
  "gru" = "#64A12D",   # Green
  "fdp" = "#FFED00",   # Yellow
  "lin" = "#BE3075",   # Magenta
  "afd" = "#009EE0"    # Blue
)


# 
# Function for ILR and ALR Transformations
simplex_transforms <- function(data) {
  ilr_data <- data %>%
    dplyr::select(wkr, election, party, resp_E, resp_Z, res_l1_E, res_l1_Z, unit_swing_polls, prop_swing_polls) %>%
    mutate(unit_swing_polls = pmax(unit_swing_polls + res_l1_E, 0.01),
           prop_swing_polls = pmax(prop_swing_polls + res_l1_E, 0.01)) %>%
    pivot_longer(cols = resp_E:prop_swing_polls, names_to = "component", values_to = "value") %>%
    mutate(value = pmax(value,0.01)) %>%
    group_by(wkr, election, component) %>%
    mutate(
      log = log(pmax(value, 0.01)),
      lr = log(pmax(value, 0.01) / (1-pmax(value, 0.01)))
    ) %>%
    dplyr::select(-value) %>%
    pivot_wider(names_from = "component", values_from = c("log","lr",
                                                          #"ilr", "alr"
    ))
  
  data <- left_join(data, ilr_data, by = join_by(election, wkr, party))
  return(data)
}


# Functions

get_wahlrecht_polls <- function() {
  
  new_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml") %>% wahlrecht_xml_extract
  old_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_archiv.xml") %>% wahlrecht_xml_extract
  
  old_polls$date %>% min
  
  wahlrecht_polls <- bind_rows(new_polls, old_polls)

  
  # sample_size
  wahlrecht_polls <- wahlrecht_polls %>% 
    pivot_longer(cols = cdu:afd, names_to = "party", values_to = "poll_share") %>% 
    pivot_wider(names_from = party, values_from = poll_share, values_fn = mean) 
  
  
  # Arrange by date, decreasing
  wahlrecht_polls <- wahlrecht_polls %>% arrange(desc(date))
  
  wahlrecht_polls$sample_size <- as.numeric(wahlrecht_polls$sample_size)
  
  return(wahlrecht_polls)
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
    rename(institute = inst, date = dat, party = werte_id, value = werte, sample_size = bfrg) %>% 
    as.data.frame %>% 
    mutate(date = as.Date(date)) %>%
    pivot_wider(values_from = "value", names_from = "party") 
  
  if("agart" %in% names(xml_df)) xml_df <- dplyr::select(xml_df, -agart)
  
  return(xml_df)
}




# Function toe estimate latent Support
estimate_dlm  <- function(polls=btw_polls, # data from 
                                  parties_to_include_all= c("cdu","spd","fdp","gru","lin","afd", "bsw"), 
                                  mdl_code = "model_code/random_walk_dlm_missing.stan", 
                                  Nchains = 10, Niter = 200,
                          start_date = start, elec = elec_date,
                          days_prior_to_election = 366) {

 
  
  # Generate a sequence of all dates within the time frame
  all_dates <- seq.Date(from = start_date, to = elec, by = "1 day")
  polls$t <- match(polls$date, all_dates)  # Match poll dates to the date sequence
  
  # Aggregate polling data by time step
  polls <- polls %>%
    group_by(t, days_to_election, date) %>%
    summarise(
      across(all_of(parties_to_include_all), ~ mean(.x, na.rm = TRUE)),  # Average poll results for each party
      sample_size = sum(sample_size),  # Sum the sample sizes for the grouped polls
      n_polls = n()  # Count the number of polls in each group
    ) %>% 
    ungroup() %>%
    arrange(t)  # Ensure data is ordered by time step
  
  # Generate a timeline for the specified number of days
  time_line <- 1:days_prior_to_election
  are_there_polls <- as.numeric(time_line %in% polls$t)  # Indicator for available polling data
  
  # Transform polling data into a matrix format required by STAN
  y <- t(as.matrix(polls[, parties_to_include_all]))
  time <- polls$t  # Extract the time steps
  
  # Replace missing values in the polling data with zeros
  y[is.na(y)] <- 0
  
  # Determine the number of observations and the length of the timeline
  N <- ncol(y)  # Number of observations
  T <- max(time_line)  # Total number of time steps
  K <- nrow(y)
  
  # Create an index mapping time steps to observations
  t_to_obs <- rep(0, T)  # Initialize with 0 (no observation)
  t_to_obs[time] <- 1:N  # Map time steps to corresponding observation indices
  
  # Create a list of data inputs for the STAN model
  stan_data <- list(
    T = T,  # Total number of time steps
    N = N,  # Number of observations
    K = K,  # Number of parties
    y = y / 100,  # Polling data scaled to proportions
    time = time,  # Time steps
    update = t_to_obs,  # Mapping of time steps to observations
    m0 = rep(1 / K, K),  # Prior on latent support (equal probability for all parties)
    C0 = diag(rep(10, K)),  # Prior variance for latent support
    run_backward_sampling = 0  # Disable backward sampling
  )
  
  # Run the STAN model using the specified inputs
  fit <- stan(
    file = mdl_code,  # Path to the STAN model code
    data = stan_data,  # Data inputs for the model
    chains = Nchains,  # Number of chains
    cores = Nchains,
    iter = Niter,  # Number of iterations
    seed = 1233452  # Seed for reproducibility
  )
  
  # Extract results from the STAN model
  post <- extract(fit)
  m <- apply(post$m, c(2, 3), mean)  # Compute the mean of the posterior samples
  colnames(m) <- parties_to_include_all  # Assign party names as column names
  
  # Convert the results into a tidy data frame
  m_df <- as.data.frame(m) %>%
    mutate(t = row_number()) %>%  # Add a column for time steps
    pivot_longer(
      cols = -t,  # Exclude the "t" column from pivoting
      names_to = "party",  # New column for party names
      values_to = "value"  # New column for values
    ) %>% 
    rename("days_prior_to_elec" = t) %>%  # Rename the time step column
    mutate(
      election = year(elec),  # Add the election year
      election_date = elec,  # Add the election date
    )
  
  # Return the STAN fit object and the processed results
  return(m_df)
}
