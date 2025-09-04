
# Load Data ============

# 
  source("00_Setup_Packages.R")

# Load Data
  full_df <- read_csv("in/data/btw_candidates_1983-2025_new.csv") %>% data.frame()

# Additional factors
  full_df$no_cand_l1 <- as.numeric(full_df$res_l1_E == 0)
  

# Past results
  res17 <- c(32.9, 20.5, 9.2, 8.9, 10.7,12.6,5) /100
  res13 <- c(41.5, 25.7, 8.6, 8.4, 4.8, 4.7, 6.3) / 100
  res09 <- c(33.8, 23, 11.9, 10.7, 14.6,1, 6) / 100
  res05 <- c(35.2, 34.2, 8.7, 8.1, 9.8, 1, 3.9) / 100
  
  btw_bund_res <- rbind(res17, res13, res09, res05)
  
  colnames(btw_bund_res) <-
    c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "And")
  rownames(btw_bund_res) <- c("2017", "2013", "2009", "2005")

# Party Colors
  party_colors <- c(
    "CDU" = "#000000",   # Black
    "SPD" = "#E3000F",   # Red
    "GRUENE" = "#64A12D", # Green
    "FDP" = "#FFED00",   # Yellow
    "LINKE" = "#BE3075", # Magenta
    "AFD" = "#009EE0"    # Blue
  )  
  
# Forecast Function ==========  

  forecast_election <- function(election, df=full_df, draws, 
                                model_form = "resp_E ~ ncand + propPlatz + alsoList + resp_Z + res_l1_E + formercand + 
                                east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1",
                                party_names_order = c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth"),
                                party_names_df = c("CDU", "SPD", "LINKE", "GRUENE", "FDP", "AFD", "AND"),
                                res_el, nsim = 100, 
                                ci_lev=1/6) {
    
    # Prepare Data ============
    
    # Set election
    election_l1 <- election - 4
    
    # Subset Data in test and train (inlcude only prior elections)
    train <- df[(df$election < election &
                   df$election != 1990) |
                       (df$election == 1990 & df$east != 1) , ]
    
    test <- df[df$election == election,]
    test$weight <- 1
    
    # Estimate model =============
    
    # Fit Regression Model
    reg <- lm(model_form, data = train)
    
    # Simulations ======
    
    # Number of simulations for model uncertainty (lm)
    S <- MASS::mvrnorm(n = nsim, coef(reg), vcov(reg))

    # Get forecast draws
    forecast <- draws$forecast
    colnames(forecast) <-draws$party_names
    adjustOrder <- match(party_names_order , colnames(forecast))
    forecast <- forecast[,adjustOrder]
    colnames(forecast) <- party_names_df

    # Calculate swings  
    sim.prop.swing <- t(sapply(1:nrow(forecast), function(draw)  (forecast[draw,] - res_el)/res_el))
    zs_pred <- matrix(0, nrow = nrow(test), ncol = nrow(sim.prop.swing))
    
    # Replace prediction data by swing
    for (i in 1:nrow(test)) {
      if (test[i, "partei"] %in% colnames(sim.prop.swing)) {
        zs_pred[i, ] <-
          test[i, "res_l1_Z"] + sim.prop.swing[, colnames(sim.prop.swing) %in% test[i, "partei"]] * test[i, "res_l1_Z"] * (test[i, "weight"])
      }
      if (test[i, "partei"] == "CSU") {
        zs_pred[i, ] <-
          test[i, "res_l1_Z"] + sim.prop.swing[, "CDU"] * test[i, "res_l1_Z"] * (test[i, "weight"])
      }
      
      if (!(test[i, "partei"] %in% colnames(sim.prop.swing)) &
          test[i, "partei"] != "CSU") {
        zs_pred[i, ] <-
          test[i, "res_l1_Z"] + sim.prop.swing[, "AND"] * test[i, "res_l1_Z"] * (test[i, "weight"])
      }
    }
    
    # Create test Simulation

    test_sim <- model.frame(model_form, data = test)
    test_sim$resp_Z <- zs_pred[, 2]

    # Put in test set
    # Make a matrix that is nsim wide and nrow(test) tall
    res_pred <- matrix(0, nrow = nrow(test), ncol = nsim)
    
    for (zsim in 1:nsim) {
      
      # Prepare Data
      test_sim$resp_Z <- zs_pred[, zsim]
      test_x <- as.matrix(test_sim[, 2:(ncol(test_sim))])
      reg_test_mat <- cbind(1, test_x)
      
      # Get prediction
      reg_preds <-  reg_test_mat %*% t(S) + matrix(rnorm(nrow(reg_test_mat) * nsim, 0, sd(residuals(reg))),
                                                   nrow = nrow(reg_test_mat),
                                                   ncol = nsim)
      
      # Standardize
      for (j in 1:299) {
        reg_preds[test$wkr == j,] <-
          sweep(reg_preds[test$wkr == j, ], 2, colSums(reg_preds[test$wkr == j,]), "/")
      }
      
      # Save means
      res_pred[, zsim] <- rowMeans(reg_preds)
      
    }
    
    # Calculate the 17% (lower) and 83% (upper) quantiles for each row
    
    ci_low <- ci_lev/2
    ci_high <-  (1-ci_low)
    test$low  <- round(apply(res_pred, 1, function(x) quantile(x, probs = ci_low))*100, 1)  # 17% quantile
    test$high <- round(apply(res_pred, 1, function(x) quantile(x, probs = ci_high))*100, 1)  # 83% quantile
    
    # Calculate the mean predicted value for each row
    test$value <- round(rowMeans(res_pred)*100, 1)
    test$res <- round(test$resp_E*100,1)
    
    return(test)
  }
  
  
  
# Caulcate for different elections
  
  # 2017 
  draws17_2 <- readRDS("in/draws/draws_forcast_levels_2017_2.RDS")
  test_btw17 <- forecast_election(election=2017,
                    res_el = btw_bund_res["2013",],
                    draws = draws17_2, ci_lev = 0.05 )
  
  # 2013 
  draws13_2 <- readRDS("in/draws/draws_forcast_levels_2013_2.RDS")
  test_btw13 <- forecast_election(election=2013,
                                  res_el = btw_bund_res["2009",],
                                  draws = draws13_2, ci_lev = 0.05)

  
  # 2009 
  draws09_2 <- readRDS("in/draws/draws_forcast_levels_2009_2.RDS")
  test_btw09 <- forecast_election(election=2009,
                                  res_el = btw_bund_res["2005",-6],
                                  party_names_order = c("cdu", "spd", "lin", "gru", "fdp", "oth"),
                                  party_names_df = c("CDU", "SPD", "LINKE", "GRUENE", "FDP",  "AND"),
                                  draws = draws09_2, ci_lev = 0.05)
  
  
  # Bind toegther
  test <- bind_rows(test_btw17,test_btw13,test_btw09)
  test <- mutate(test,partei= ifelse(partei=="CSU","CDU",partei))
  
  
  # BTW 17 ===============

  filter(test, partei!= "AND") %>%
  ggplot(aes(x=res, y=value, ymin = low, ymax = high, colour = partei)) +
    geom_abline(intercept = 0, slope = 1) +
    #coord_flip() +
    geom_point() + 
    geom_linerange() +
    facet_grid(election ~ partei) +
    scale_colour_manual(values = party_colors) + # Apply custom colors
    ylim(0,100) + xlim(0,100) +
    ylab("Vorhergesagtes Ergebnis") + xlab("Wahlergebnis") + 
    ggtitle("BTW Out-of Sample") +
    theme_minimal()

  ggsave("out/fig1_old.pdf")
  
  
  # Calculate Coverage ==============
  coverage_ci <- function(res, low, high) mean(ifelse(res > low & res < high, 1, 0))
  
  test %>%
    filter(party != "oth") %>%
    #group_by(election) %>%
    summarise("coverage" =  coverage_ci(res, low, high))
  
  
  # RMSE
  test %>%
    group_by(election) %>%
    mutate(se = (res - value)^2,
           ae = abs(res - value)) %>%
    group_by(partei) %>%
    summarize("MAE" = mean(ae),
              "RMSE" = sqrt(mean(se)))
  

