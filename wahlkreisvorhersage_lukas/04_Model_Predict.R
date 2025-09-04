

  # Set up Script
  setwd("~/projects/zweitstimme/wahlkreis-vorhersage-neu/")
  source("00_Setup_Packages.R")
  
  # Load Data
  load("in/model_data.RData")

# Get Polls DLM =================

  cat("Get Current Polls from wahlrecht.de\n")
  
  # Save Info
  elec_date <- as.Date("2025-02-23")
  start <-  elec_date - 365
  
  # Get polls from wahrecht.de
  btw_polls <- get_wahlrecht_polls() %>% 
    filter(institute != "gms", 
           date > start & date < elec_date) %>%
    mutate(days_to_election = elec_date - date) %>% 
    select(date, institute, sample_size, beg, end,days_to_election, bsw:afd) %>%
    mutate(oth = 100 - bsw -cdu -spd -gru -fdp - lin -afd)

# Estimate DLM =================

  cat("Estimating DLM model for: 2025\n")
  
  # Setup
  options(mc.cores = 10)
  
  # Estimate model
  dlm_res <- estimate_dlm(polls=btw_polls,
                          parties_to_include_all = c("cdu","spd","fdp","gru","lin","afd", "bsw"),
                          start_date = start, elec = elec_date,
                          mdl_code = "in/stan_models/random_walk_dlm_missing.stan", 
                          Nchains = 10, Niter = 2000)

  # Prepare
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

  # Replace values in wkr_nr using a left join
  wkr_data <- wkr_data %>%
    left_join(dlm_m, by = c("party", "election"), suffix = c("", ".new")) %>%
    mutate(
      mt_7 = coalesce(mt_7.new, mt_7),
      mt_5 = coalesce(mt_5.new, mt_5),
      mt_2 = coalesce(mt_2.new, mt_2)
    ) %>%
    select(-ends_with(".new"))  # Remove temporary columns



# Prepare Data ================
  
  # Set Days (7, 5, and 2)
  set_days <- paste("mt_",2,sep="") 
  
  # Prepare Wkr Data
  wkr_data <- wkr_data %>% 
    filter(election > 1990) %>%  # Filter elections after 1990
    group_by(party, election) %>%
    mutate(res_l1_E = replace(res_l1_E, res_l1_E == 0, mean(res_l1_E, na.rm = TRUE)),
           res_l1_Z = replace(res_l1_Z, res_l1_Z == 0, mean(res_l1_Z, na.rm = TRUE))) %>%
    mutate("mt" = get(set_days)) %>% 
    mutate(
      resp_E = ifelse(resp_E == 0, NA, resp_E),
      no_cand = ifelse(resp_E == 0, 1, 0 ),
      no_cand_l1 = ifelse(res_l1_E == 0, 1, 0 ),
      res_l1_E = pmax(res_l1_E, 0.01),
      res_l1_Z = pmax(res_l1_Z, 0.01),
      voteshare_l1 = pmax(voteshare_l1, 1),
      resp_E = pmax(resp_E, 0.01),
      prop_swing_true = (voteshare - voteshare_l1) / voteshare_l1,
      prop_swing_polls = ((mt - voteshare_l1) / voteshare_l1) * res_l1_Z,  # Based on last Erstimme
      unit_swing_polls = (mt - voteshare_l1) / 100,
      party_elec = paste(party, election, sep=""),
    )
  

  wkr_data <- simplex_transforms(wkr_data)
  

# Estimate Model =================
 
  # Estimate Model
  wkr_data_train <-   filter(wkr_data, election < 2025)
  mdl_est <- brm(bf(lr_resp_E  ~ lr_res_l1_E + lr_res_l1_Z + lr_prop_swing_polls+ 
                  formercand + east + female + incumbent + akad + incumbent_in_wkr), 
             data=wkr_data_train , 
             family = gaussian(), chains = 5, iter = 2000, cores = 5)
  
  print(summary(mdl_est))
  
  # Predict Data 2025
  wkr_data_test <-   filter(wkr_data, election == 2025)
  wkr_data_test$idx <- 1:nrow(wkr_data_test)
  
  idx_dat <- wkr_data_test %>% 
            select(idx, wkr, wkr_name, party) %>% 
            data.frame()
  
  
  # Get Prediction
  pred_draws <- posterior_predict(mdl_est, newdata = wkr_data_test)
  pred_draws <- plogis(pred_draws)
  
  
  # Get means
  pred_mean <- colMeans(pred_draws, na.rm=T)
  ci <- apply(pred_draws, 2, quantile, probs = c(2/12, 10/12), na.rm=T)
  pred_data <- tibble(wkr_data_test, pred = pred_mean, low = ci[1,], high = ci[2,])


  
  # Get probabiltiy & winner
  dat_winner <- do.call("rbind",lapply(split(idx_dat,idx_dat$wkr), function(dat){
    
    dat$prob_winner <- apply(apply(pred_draws[,dat$idx],1, function(x) max(x) == x),1,mean)
    dat$pred_winner <- dat$prob_winner == max(dat$prob_winner)
    
    return(dat)
    
  }))
  
  
  pred_data <- left_join(pred_data,dat_winner,by = join_by(election, wkr, wkr_name, party, idx))
  
  
  # Save
  save(pred_data, pred_draws, file = "out/model_pred2025.Rdata")
  
  
  
  # Plot
  ggplot(pred_data, aes(y=pred,ymin=pmax(low,0) , ymax=high, x = 0.3*res_l1_E + 0.7*res_l1_Z, col=party)) +
    geom_point(cex=0.7) +  
    geom_linerange() +  
    geom_abline(intercept = 0, slope = 1, alpha=0.2) +  # 45-degree reference line
    facet_wrap(election ~ party, scale="free") +
    scale_color_manual(values = party_colors) +
    ylim(0, 0.8) + xlim(0, 0.8) +
    xlab("Letztes 30% Erststimmen + 70% Zweistimmen Ergebnis") + 
    ylab("Vorhergesagtes Ergebnis") + 
    ggtitle("Forecasts District 20025 German Federal Elections") +
    theme_minimal()
      
  
  