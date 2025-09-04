  
  # Setup
  setwd("~/projects/zweitstimme/wahlkreis-vorhersage-neu/")
  source("00_Setup_Packages.R")

  # Load Model Data
  load("in/model_data.RData")


# Functions =======

  # Define Function to Predict and Compute Confidence Intervals
  update_brms <- function(model, train_data) {
    update(model, newdata = train_data, cores = 8)
  }
  
  # Prediction function
  predict_brms <- function(model, test_data, ci_lev=1/12) {
    pred <- posterior_predict(model, newdata = test_data)
    pred_mean <- colMeans(pred, na.rm=T)
    ci <- apply(pred, 2, quantile, probs = c(ci_lev, (1-ci_lev)), na.rm=T)
    tibble(test_data, pred = pred_mean, low = ci[1,], high = ci[2,])
  }
  


# Data Preparation ==============
  
  # days prior to election
  set_days <- paste("mt",7,sep="_") # choose from mt_2, mt_5, and mt_7
  
  # Same formatting
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
  
  
  # Apply Transformations
  wkr_data <- simplex_transforms(wkr_data)

# Linear Regression Models =====

  # Fit models
  mdls <- list()
  mdls[["sparse_unit"]] <- brm(bf(resp_E  ~ res_l1_E + res_l1_Z + unit_swing_polls), 
                               data = wkr_data, 
                               family = gaussian(), chains = 8, iter = 500, cores = 8)
  mdls[["full_unit"]] <- update(mdls[["sparse_unit"]], 
                                formula. =  ~ .  + ncand + propPlatz + formercand + east + female + incumbent + akad + incumbent_in_wkr, 
                                newdata=wkr_data, cores = 8)
  mdls[["sparse_prop"]] <- update(mdls[["sparse_unit"]], 
                                  formula. = ~ . - unit_swing_polls + prop_swing_polls, 
                                  newdata=wkr_data, cores = 8)
  mdls[["full_prop"]] <- update(mdls[["sparse_unit"]], 
                                formula. = ~ . - unit_swing_polls + prop_swing_polls + formercand + east + female + incumbent + akad + incumbent_in_wkr, 
                                newdata=wkr_data, cores = 8)

  mdls[["sparse_unit_lr"]] <- brm(bf(resp_E  ~ res_l1_E + res_l1_Z + unit_swing_polls), 
                               data = wkr_data, 
                               family = gaussian(), chains = 8, iter = 500, cores = 8)
  mdls[["full_unit_lr"]] <- update(mdls[["sparse_unit"]], 
                                formula. =  ~ .  + ncand + propPlatz + formercand + east + female + incumbent + akad + incumbent_in_wkr, 
                                newdata=wkr_data, cores = 8)
  mdls[["sparse_prop_lr"]] <- update(mdls[["sparse_unit"]], 
                                  formula. = ~ . - unit_swing_polls + prop_swing_polls, 
                                  newdata=wkr_data, cores = 8)
  mdls[["full_prop_lr"]] <- update(mdls[["sparse_unit"]], 
                                formula. = ~ . - unit_swing_polls + prop_swing_polls + formercand + east + female + incumbent + akad + incumbent_in_wkr, 
                                newdata=wkr_data, cores = 8)

  # Define Elections and Initialize Results List
  elcs <- c(2009, 2013, 2017, 2021, 2025)
  res_list <- list()
  
  # Loop Through Elections
  for (e in elcs) {
    wkr_data_train <- filter(wkr_data, election < e)
    wkr_data_test <- filter(wkr_data, election == e)
    
    # Fit Models with Trainign Data
    mdls_update <- lapply(mdls, update_brms, train_data = wkr_data_train)
    
    # PRedict in test data
    predictions <- lapply(mdls_update, predict_brms, test_data = wkr_data_test)
    
    # Bind
    predictions_long <- bind_rows(predictions, .id = "model")
    res_list[[paste(e)]] <- list("pred_dat" = predictions_long)
  }
  
  # Combine Data
  combined_data_lm <- bind_rows(lapply(res_list, function(x) x$pred_dat))
  
  # Compute Error Metrics
  error_summary_lm <- combined_data_lm %>% 
    group_by(model, election) %>%
    summarise(
      rmse = sqrt(mean((resp_E - pred)^2, na.rm = TRUE)),
      mae = mean(abs(resp_E - pred), na.rm = TRUE),
      sd = sd(resp_E - pred), 
      coverage = mean(resp_E >= low & resp_E <= high, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    arrange( election, rmse)
  
  error_summary_lm

  # Save models and error analysis
  save(mdls,combined_data_lm, error_summary_lm, file = "model_brms_linear.Rdata")
  
  
# Beta Regression Models =====
  
  # Set-up Models  
  mdls_beta <- list()
  mdls_beta[["full_unit_beta"]] <- brm(formula = bf(resp_E ~ lr_res_l1_E + lr_res_l1_Z + lr_unit_swing_polls,
                                               phi ~ 1), data = filter(wkr_data, no_cand==0), 
                                       family = Beta(), chains = 8, iter = 400, cores = 8)
  mdls_beta[["full_unit_beta"]] <- update(mdls[["sparse_unit"]], 
                                          formula. =  ~ .  + ncand + propPlatz + formercand + east + female + incumbent + akad + incumbent_in_wkr, 
                                          newdata=filter(wkr_data, no_cand==0), cores = 8)
  mdls_beta[["sparse_prop_beta"]] <- update(mdls[["sparse_unit"]], 
                                            formula. = ~ . - unit_swing_polls + prop_swing_polls, 
                                            newdata=filter(wkr_data, no_cand==0), cores = 8)
  mdls_beta[["full_prop_beta"]] <- update(mdls[["sparse_unit"]], 
                                          formula. = ~ . - unit_swing_polls + prop_swing_polls + formercand + east + female + incumbent + akad + incumbent_in_wkr, 
                                          newdata=filter(wkr_data, no_cand==0), cores = 8)

  
  # Define Elections and Initialize Results List
  elcs <- c(2009, 2013, 2017, 2021,2025)
  res_list <- list()
  
  # Loop Through Elections
  for (e in elcs) {
    wkr_data_train <- filter(wkr_data, election < e, no_cand==0)
    wkr_data_test <- filter(wkr_data, election == e, no_cand==0)
    
    # Fit Models with Trainign Data
    mdls_update <- lapply(mdls_beta, update_brms, train_data = wkr_data_train)
    
    # PRedict in test data
    predictions <- lapply(mdls_update, predict_brms, test_data = wkr_data_test)

    # Bind
    predictions_long <- bind_rows(predictions, .id = "model")
    res_list[[paste(e)]] <- list("pred_dat" = predictions_long)
  }
  
  # Combine Data
  combined_data_beta <- bind_rows(lapply(res_list, function(x) x$pred_dat))
  
  # Compute Error Metrics
  error_summary_beta <- combined_data_beta %>% 
    group_by(model, election) %>%
    summarise(
      rmse = sqrt(mean((resp_E - pred)^2, na.rm = TRUE)),
      mae = mean(abs(resp_E - pred), na.rm = TRUE),
      sd = sd(resp_E - pred, na.rm=T), 
      coverage = mean(resp_E >= low & resp_E <= high, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    arrange( election, rmse)
  
  error_summary_beta
  
  # Save models and error analysis
  save(mdls_beta,combined_data_beta, error_summary_beta, 
       file = "out/model_brms_beta.Rdata")
  
  

  
  
  