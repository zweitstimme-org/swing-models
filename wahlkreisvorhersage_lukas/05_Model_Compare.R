# Set up Script
source("00_Setup_Packages.R")

# Load Data
load("in/model_data.RData")

# Load Prediction
load("out/model_pred2025.Rdata")


# ================================
# Eval Distirct Winner
# ========================

pred_data %>%
  filter(pred_winner) %>% 
  select(wkr,  "pred_winner" = party, "pred_prob" = prob_winner) %>%
  right_join(readRDS("in/data/district_winner.RDS") , by = "wkr") %>%
  mutate(correct_pred =  (pred_winner == true_winner)) %>%
  filter(pred_prob > 0.6) %>% 
  pull(correct_pred) %>% mean()


# ================================
# Compare with current Model
# ========================

# Get current Model
prediction_data_districts <- readRDS("~/projects/zweitstimme/prediction-2025/api/prediction_data_districts.rds")

prediction_data_districts <- prediction_data_districts %>% 
  dplyr::select(c(wkr, party, "pred"= value, low, high, value_l1))


compare_data <- left_join(pred_data, prediction_data_districts, by = c("wkr", "party"), suffix = c("_new","_current"))

# Comparsion compare_data
ggplot(compare_data) +
  geom_point(aes(y=pred_new, x=pred_current/100, col=party)) +
  geom_segment(aes(y=pred_new, yend=pred_new, x=pmax(low_current,0)/100, xend=pmax(high_current,0)/100, col=party)) +
  geom_segment(aes(y=pmax(low_new,0), yend=high_new, x=pred_current/100, xend=pred_current/100, col=party)) +
  facet_wrap(~ party) +  
  geom_abline(intercept = 0, slope = 1, alpha=0.2) +  # 45-degree reference line
  scale_color_manual(values = party_colors) +
  ylim(c(0,1)) + xlim(c(0,1)) +
  xlab("Current Erstime Results") + 
  ylab("New Erstimmen Result") + 
  ggtitle("Forecasts District 20025 German Federal Elections") +
  theme_minimal()
  


(cor(compare_data$pred_new,compare_data$pred_current/100, use = "complete.obs"))

ggplot(compare_data) +
  geom_point(aes(y=pred_new, x=pred_current/100, col=party)) +
  geom_segment(aes(y=pred_new, yend=pred_new, x=pmax(low_current,0)/100, xend=pmax(high_current,0)/100, col=party)) +
  geom_segment(aes(y=pmax(low_new,0), yend=high_new, x=pred_current/100, xend=pred_current/100, col=party)) +
  facet_wrap(~ party) +  
  geom_abline(intercept = 0, slope = 1, alpha=0.2) +  # 45-degree reference line
  scale_color_manual(values = party_colors) +
  ylim(c(0,1)) + xlim(c(0,1)) +
  xlab("Current Erstime Results") + 
  ylab("New Erstimmen Result") + 
  ggtitle("Forecasts District 20025 German Federal Elections") +
  theme_minimal()


compare_data <- compare_data %>% 
  mutate(diff = pred_new - pred_current/100,
         range_current = (high_current - low_current)/100,
         range_new = high_new - low_new
         )


# Boxplot difference 
ggplot(compare_data) +
  geom_boxplot(aes(y=diff, x=party, fill=party)) +
  scale_fill_manual(values = party_colors) +
  coord_flip() +
  theme_bw() + ylab("Prediction Neues Model - Altes Model") +
  ylim(c(-0.15,0.15)) + 
  geom_text(
    data = subset(compare_data, abs(diff) > 0.1),
    aes(y = diff, x = party, label = wkr_name),
    vjust = 0.5,hjust = -0.1, angle = 45, size=2  # Verschiebt das Label leicht nach rechts
  )

# Boxplot range 
compare_data %>% 
  select(party, range_new, range_current) %>%
  pivot_longer(range_new:range_current) %>% 
  ggplot() +
  geom_boxplot(aes(y=value, fill=party)) +
  facet_grid(~  name) +
  coord_flip() +
  scale_fill_manual(values = party_colors) +
  theme_minimal()


