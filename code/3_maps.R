# ========== Libraries ==========
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(dlm)
  library(xml2)
  library(xtable)
  library(patchwork) # for plot_grid
  library(sf)
  library(ggplot2)
  
  
  
  # ... add others as needed
})

# ========== Source Utility Functions ==========
# Source custom functions from code/utils.R
source("code/0_utils.R")

load("data/btw_candidates_1983-2025.RData")



# ----------------------------------------------------------
# Map years to shapefile paths and .shp filenames
# This list links each election year to its corresponding shapefile directory and .shp file.
# Adjust the 'dir' and 'shp' values if your file structure changes.
shapefile_info <- list(
  "1998" = list(
    dir = "data/district-shapefiles/btw98_geometrie_wahlkreise_shp_wgs84",
    shp = "Geometrie_Wahlkreise_14DBT_geo.shp"
  ),
  "2002" = list(
    dir = "data/district-shapefiles/btw02_geometrie_wahlkreise_shp_wgs84",
    shp = "Geometrie_Wahlkreise_15DBT_geo.shp"
  ),
  "2005" = list(
    dir = "data/district-shapefiles/btw05_geometrie_wahlkreise_shp_wgs84",
    shp = "Geometrie_Wahlkreise_16DBT_geo.shp"
  ),
  "2009" = list(
    dir = "data/district-shapefiles/btw09_geometrie_wahlkreise_shp_vg1000",
    shp = "Geometrie_Wahlkreise_17DBT_VG1000.shp"
  ),
  "2013" = list(
    dir = "data/district-shapefiles/btw13_geometrie_wahlkreise_etrs89-vg1000_geo_shp",
    shp = "Geometrie_Wahlkreise_18DBT_VG1000.shp"
  ),
  "2017" = list(
    dir = "data/district-shapefiles/btw17_geometrie_wahlkreise_vg250_geo_shp",
    shp = "Geometrie_Wahlkreise_19DBT_VG250_geo.shp"
  ),
  "2021" = list(
    dir = "data/district-shapefiles/btw21_geometrie_wahlkreise_vg250_geo_shp",
    shp = "Geometrie_Wahlkreise_20DBT_VG250_geo.shp"
  ),
  "2025" = list(
    dir = "data/district-shapefiles/btw25_geometrie_wahlkreise_shp",
    shp = "btw25_geometrie_wahlkreise_shp.shp"
  )
)

# ----------------------------------------------------------
# Run the model and plot incorrect forecasts for each year

# Define the formula for the "proportional both votes" model
model_formula <- as.formula(
  "resp_E ~ ncand + propPlatz + alsoList + res_l1_E*proportional + res_l1_Z*proportional + formercand + east + female + incumbent + akad + incumbent_in_wkr + no_cand_l1"
)




plots <- list() # Store plots for each year
test_summaries <- list()

# Define party colors
party_colors <- c(
  "cdu" = "#000000",
  "spd" = "#DD0000",
  "gru" = "#4C9A2A",
  "fdp" = "#FFCC00",
  "afd" = "#009EE0",
  "lin" = "purple",
  "bsw" = "#FF6A13",
  "oth" = "#B7B7B7"
)


# 1. Build all plots with legend.position = "none" except the first
for (i in seq_along(names(shapefile_info))) {
  year <- names(shapefile_info)[i]
  # 1. Read the shapefile for the current year
  shp_path <- file.path(shapefile_info[[year]]$dir, shapefile_info[[year]]$shp)
  districts_sf <- st_read(shp_path, quiet = TRUE)
  # Note: The shapefile must have a unique district identifier column (e.g., 'WKR_NR')
  
  # 2. Prepare train and test data
  train <- btw_candidates_1983_2025 %>%
    filter(election != as.numeric(year), partei != "AND")
  test <- btw_candidates_1983_2025 %>%
    filter(election == as.numeric(year))
  
  # 3. Fit the model on the training data
  reg <- lm(model_formula, data = train)
  
  # 4. Predict on the test data
  test$predicted <- predict(reg, newdata = test)
  
  # 5. For each district, determine the predicted winner
  test <- test %>%
    group_by(wkr) %>%
    mutate(winner_pred = ifelse(predicted == max(predicted, na.rm = TRUE), 1, 0)) %>%
    ungroup()
  
  # 6. Mark incorrect forecasts and get actual winner's party
  test_summary <- test %>%
    filter(winner == 1) %>%
    mutate(
      incorrect = winner != winner_pred,
      party_color = ifelse(incorrect, party, NA) # party column should be the actual winner's party
    ) %>%
    select(wkr, incorrect, party_color)
  test_summaries[[year]] <- test_summary
  
  # 7. Merge with shapefile data
  map_data <- districts_sf %>%
    left_join(test_summary, by = c("WKR_NR" = "wkr"))
  
  # 8. Plot: use party color for incorrect, grey for correct
  p <- ggplot(map_data) +
    geom_sf(
      aes(
        fill = ifelse(is.na(incorrect), NA, ifelse(incorrect, party_color, "correct"))
      ),
      color = "grey30", size = 0.2
    ) +
    scale_fill_manual(
      values = c(party_colors, "correct" = "grey90"),
      breaks = c(names(party_colors), "correct"),
      labels = c(
        "cdu" = "CDU/CSU", "spd" = "SPD", "gru" = "GRÜNE", "fdp" = "FDP",
        "lin" = "LINKE", "afd" = "AfD", "bsw" = "BSW", "oth" = "Other", "correct" = "Correct"
      ),
      na.value = "grey80",
      name = NULL
    ) +
    labs(title = year) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) + theme(legend.position = "none")
  
  plots[[year]] <- p
}

# 2. Use patchwork's plot_layout(guides = "collect") on a manual grid
plots_nolegend <- lapply(plots, function(p) p + theme(legend.position = "none"))
library(patchwork)

# Define the legend colors and labels
party_colors_legend <- c(
  "cdu" = "#000000",
  "spd" = "#DD0000",
  "gru" = "#4C9A2A",
  "fdp" = "#FFCC00",
  "bsw" = "#FF6A13",
  "lin" = "purple",
  "oth" = "#B7B7B7",
  "correct" = "grey90"
)
party_labels_legend <- c(
  "cdu" = "CDU/CSU", "spd" = "SPD", "gru" = "GRÜNE", "fdp" = "FDP",
  "bsw" = "BSW", "lin" = "LINKE", "oth" = "Other", "correct" = "Correct"
)

# Collect all parties that occur as incorrect winners
all_incorrect_parties <- unique(unlist(
  lapply(test_summaries, function(ts) ts$party_color[ts$incorrect & !is.na(ts$party_color)])
))
all_incorrect_parties <- all_incorrect_parties[!is.na(all_incorrect_parties)]

# Always include 'correct'
legend_parties <- unique(c(all_incorrect_parties, "correct"))

# party_colors_legend <- party_colors_legend[legend_parties]
party_labels_legend <- c(
  "cdu" = "CDU/CSU", "spd" = "SPD", "gru" = "GRÜNE", "lin" = "LINKE", "afd" = "AfD", "correct" = "Correct"
)[legend_parties]

# Dummy data
dummy <- data.frame(
  party = factor(names(party_labels_legend), levels = names(party_labels_legend)),
  y = 1
)

# Print for debugging
print("party_colors_legend:")
print(party_colors_legend)
print("levels(dummy$party):")
print(levels(dummy$party))

# Legend plot
legend_plot <- ggplot(dummy, aes(x = party, y = y, fill = party)) +
  geom_col(width = 0.00001) +
  scale_fill_manual(
    values = party_colors_legend,
    labels = party_labels_legend,
    name = NULL,
    drop = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

print(legend_plot)

final_plot <- (wrap_plots(plots_nolegend, ncol = 4) / legend_plot) +
  plot_layout(heights = c(10, 1))
print(final_plot)
ggsave("figures/all_years_incorrect_forecasts.pdf", final_plot, width = 16, height = 8)

