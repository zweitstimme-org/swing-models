load("out/model_brms_beta.Rdata.Rdata")
load("out/model_brms_linear.Rdata")
load("out/model_ols.Rdata")

source("00_setup_packages.R")

# Errors ======
bind_rows(combined_data_beta, combined_data_lm) %>% 
  group_by(model) %>%
  #filter(party != "oth") %>%
  summarise(
    rmse = sqrt(mean((resp_E - pred)^2, na.rm = TRUE)),
    mae = mean(abs(resp_E - pred), na.rm = TRUE),
    sd = sd(resp_E - pred, na.rm=T), 
    coverage = mean(resp_E >= low & resp_E <= high, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  arrange(rmse) 

# Figures =====

# Compute Error Metrics
combined_data_lm %>% 
  filter(election != 2025, model == "full_prop")  %>% 
  filter(party != "oth") %>% 
  ggplot(aes(x = resp_E, y = pred, ymin = pmax(low,0), ymax = high, col = party)) +
  geom_point(cex=0.7) +  
  geom_linerange() +  
  geom_abline(intercept = 0, slope = 1) +  # 45-degree reference line
  facet_grid(election ~ party, scale="free") +
  scale_color_manual(values = party_colors) +
  ylim(0, 0.8) + xlim(0, 0.8) +
  xlab("Wahlergebnis") + 
  ylab("Vorhergesagtes Ergebnis") + 
  ggtitle("Out-of-Sample Forecasts and Final Results for German Federal Elections") +
  theme_minimal()

ggsave("out/fig1_error.pdf")


# 

combined_data_lm %>%
  filter(wkr == 85, election ==2025, model == "full_prop") %>%
  select(party, pred, low, high)
  
