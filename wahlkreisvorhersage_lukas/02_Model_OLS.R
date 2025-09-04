# Load Model Data
load("in/model_data.RData")

# Define Party Colors
party_colors <- c(
  "cdu" = "#000000",   # Black
  "spd" = "#E3000F",   # Red
  "gru" = "#64A12D",   # Green
  "fdp" = "#FFED00",   # Yellow
  "lin" = "#BE3075",   # Magenta
  "afd" = "#009EE0"    # Blue
)

# days prior to election
set_days <- paste("mt",7,sep="_") # choose from mt_2, mt_5, and mt_7

 
# Data Preparation
wkr_data <- wkr_data %>% 
  filter(election > 1990) %>%  # Filter elections after 1990
  group_by(party, election) %>%
  mutate(res_l1_E = replace(res_l1_E, res_l1_E == 0, mean(res_l1_E, na.rm = TRUE)),
         res_l1_Z = replace(res_l1_Z, res_l1_Z == 0, mean(res_l1_Z, na.rm = TRUE))) %>%
  mutate("mt" = get(set_days)) %>%
  mutate(
    resp_E = ifelse(resp_E == 0, NA, resp_E),
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


# Define Model Formulas
models <- list(
  sparse_unit_lr = as.formula("lr_resp_E ~ lr_res_l1_E + lr_res_l1_Z + lr_unit_swing_polls"),
  full_unit_lr = as.formula("lr_resp_E ~ lr_res_l1_E + lr_res_l1_Z + lr_unit_swing_polls + ncand + propPlatz + formercand + east + female + incumbent + akad + incumbent_in_wkr"),
  
  sparse_prop_lr = as.formula("lr_resp_E ~ lr_res_l1_E + lr_res_l1_Z + lr_prop_swing_polls"),
  full_prop_lr = as.formula("lr_resp_E ~ lr_res_l1_E + lr_res_l1_Z + lr_prop_swing_polls + ncand + propPlatz + formercand + east + female + incumbent + akad + incumbent_in_wkr"),
  
  sparse_unit = as.formula("resp_E ~ res_l1_E + res_l1_Z + unit_swing_polls"),
  full_unit = as.formula("resp_E ~ res_l1_E + res_l1_Z + unit_swing_polls + ncand + propPlatz + formercand + east + female + incumbent + akad + incumbent_in_wkr"),
  sparse_prop = as.formula("resp_E ~ res_l1_E + res_l1_Z + prop_swing_polls"),
  full_prop = as.formula("resp_E ~ res_l1_E + res_l1_Z + prop_swing_polls + ncand + propPlatz + formercand + east + female + incumbent + akad + incumbent_in_wkr")
)

# Estimate all models
fit_mdls <- function(formula, data) {
  lm(formula, data = data)
}

mdls_ols <- lapply(models, function(m) fit_mdls(m, wkr_data))
texreg::screenreg(mdls_ols)

# Define Function to Fit Models and Predict with Confidence Intervals
fit_and_predict <- function(formula, train_data, test_data, level = 4/6) {
  model <- lm(formula, data = train_data)
  pred <- predict(model, test_data)
  ci <- predict(model, test_data, interval = "prediction", level = level)
  tibble(test_data, pred = pred, low = ci[,2], high = ci[,3])
}

# Define Elections and Initialize Results List
elcs <- c(2009, 2013, 2017, 2021, 2025)
res_list <- list()

# Loop Through Elections
for (e in elcs) {
  wkr_data_train <- filter(wkr_data, election < e)
  wkr_data_test <- filter(wkr_data, election == e)
  
  predictions <- lapply(models, fit_and_predict, train_data = wkr_data_train, test_data = wkr_data_test)
  names(predictions) <- names(models)
  
  predictions_long <- bind_rows(predictions, .id = "model") 
  
  res_list[[paste(e)]] <- list("pred_dat" = predictions_long)
}

# Combine Data
combined_data_ols <- bind_rows(lapply(res_list, function(x) x$pred_dat))

# Compute Error Metrics in Long Format
error_summary_ols <- combined_data_ols %>% 
  group_by(model) %>%
  mutate(pred = case_when(model %in% c("sparse_prop_lr", "full_prop_lr",
                                      "sparse_unit_lr", "full_unit_lr") ~
                            plogis(pred),
                          TRUE  ~ pred),
         low = case_when(model %in% c("sparse_prop_lr", "full_prop_lr",
                                       "sparse_unit_lr", "full_unit_lr") ~
                            plogis(low),
                          TRUE  ~ low),
         high = case_when(model %in% c("sparse_prop_lr", "full_prop_lr",
                                      "sparse_unit_lr", "full_unit_lr") ~
                           plogis(high),
                         TRUE  ~ high)
         ) %>%
  summarise(
    rmse = sqrt(mean((resp_E - pred)^2, na.rm = TRUE)),
    mae = mean(abs(resp_E - pred), na.rm = TRUE),
    sd = sd(resp_E - pred, na.rm=T), 
    coverage = mean(resp_E >= low & resp_E <= high, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(rmse)

error_summary_ols

# Compute Error Metrics
combined_data_ols %>% 
  filter(election != 2025, model == "full_prop_lr")  %>% 
filter(party != "oth") %>% 
ggplot(aes(x = resp_E, y = plogis(pred), ymin = plogis(low), ymax = plogis(high), col = party)) +
  geom_point(cex=0.8) +  
  geom_linerange() +  
  geom_abline(intercept = 0, slope = 1) +  # 45-degree reference line
  facet_grid(election ~ party) +
  scale_color_manual(values = party_colors) +
  ylim(0, 1) + xlim(0, 1) +
  xlab("Wahlergebnis") + 
  ylab("Vorhergesagtes Ergebnis") + 
  ggtitle("Out-of-Sample Forecasts and Final Results for German Federal Elections") +
  theme_minimal()


# Save
 save(combined_data_ols,error_summary_ols, mdls_ols,
      file ="out/model_ols.Rdata")


