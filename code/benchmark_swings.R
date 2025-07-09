pacman::p_load("tidyverse")

results <- data.frame(
  party = c("CDU/CSU", "AfD", "SPD", "Grüne", "Linke", "FDP", "Sonstige"),
  # Careful: Nothing of this includes BSW!
  result_21 = c(24.1, 10.3, 25.7, 14.8, 4.9, 11.5, 8.7),
  result_25 = c(29.9, 20.5, 15.3, 13.0, 7.3, 4.4, 5.2)
  )

results["uniform_swing_21_to_25"] <- results$result_25 - results$result_21
results["proportional_swing_21_to_25"] <- results$result_25/results$result_21

wk_results_21 <- read.csv("BTW_21_Result_per_WK.csv")
wk_results_unif_swing <- wk_results_21 |>
  mutate(
    afd_swing = AfD_Stimme1 + results$uniform_swing_21_to_25[results$party == "AfD"],
    cdu_swing =CDU_Stimme1 + results$uniform_swing_21_to_25[results$party == "CDU/CSU"],
    csu_swing = CSU_Stimme1 + results$uniform_swing_21_to_25[results$party == "CDU/CSU"],
    linke_swing = DIE.LINKE_Stimme1 + results$uniform_swing_21_to_25[results$party == "Linke"],
    fdp_swing = FDP_Stimme1 + results$uniform_swing_21_to_25[results$party == "FDP"],
    gruene_swing = GRÜNE_Stimme1 + results$uniform_swing_21_to_25[results$party == "Grüne"],
    spd_swing = SPD_Stimme1 + results$uniform_swing_21_to_25[results$party == "SPD"],
    sonstige_swing = Sonstige_Stimme1 + results$uniform_swing_21_to_25[results$party == "Sonstige"]
  ) |>
  mutate(across(contains("swing"), ~ replace(.x, is.na(.x), 0))) |>
  mutate(across(contains("swing"), ~ ifelse(is.na(.x) | .x < 0, 0, .x)))

uniform_calls <- wk_results_unif_swing |>
  rowwise() %>%
  # Scale to 100
    mutate(across(contains("swing"), ~ .x / sum(c_across(contains("swing")), na.rm = TRUE) * 100)) |>
  # Identify winner
  mutate(max_swing_col = names(pick(contains("swing")))[which.max(c_across(contains("swing")))]) |>
  select(Gebietsnummer, max_swing_col) |>
  rename(WK_No = Gebietsnummer, Call_Uniform_Swing = max_swing_col)


wk_results_prop_swing <- wk_results_21 |>
  mutate(
    afd_swing = AfD_Stimme1 * results$proportional_swing_21_to_25[results$party == "AfD"],
    cdu_swing =CDU_Stimme1 * results$proportional_swing_21_to_25[results$party == "CDU/CSU"],
    csu_swing = CSU_Stimme1 * results$proportional_swing_21_to_25[results$party == "CDU/CSU"],
    linke_swing = DIE.LINKE_Stimme1 * results$proportional_swing_21_to_25[results$party == "Linke"],
    fdp_swing = FDP_Stimme1 * results$proportional_swing_21_to_25[results$party == "FDP"],
    gruene_swing = GRÜNE_Stimme1 * results$proportional_swing_21_to_25[results$party == "Grüne"],
    spd_swing = SPD_Stimme1 * results$proportional_swing_21_to_25[results$party == "SPD"],
    sonstige_swing = Sonstige_Stimme1 * results$proportional_swing_21_to_25[results$party == "Sonstige"]
  ) |>
  mutate(across(contains("swing"), ~ replace(.x, is.na(.x), 0))) |>
  mutate(across(contains("swing"), ~ ifelse(is.na(.x) | .x < 0, 0, .x)))

prop_calls <- wk_results_prop_swing |>
  rowwise() %>%
  # Scale to 100
  mutate(across(contains("swing"), ~ .x / sum(c_across(contains("swing")), na.rm = TRUE) * 100)) |>
  # Identify winner
  mutate(max_swing_col = names(pick(contains("swing")))[which.max(c_across(contains("swing")))]) |>
  select(Gebietsnummer, max_swing_col) |>
  rename(WK_No = Gebietsnummer, Call_Prop_Swing = max_swing_col)
  
calls <- left_join(uniform_calls, prop_calls) |>
  mutate(across(c(Call_Uniform_Swing, Call_Prop_Swing), ~ str_remove(.x, "_swing"))) |>
  mutate(across(c(Call_Uniform_Swing, Call_Prop_Swing), ~ str_replace_all(.x,
    c("^cdu$" = "CDU/CSU", "afd" = "AfD", "spd" = "SPD", "^csu$" = "CDU/CSU", "gruene" = "Grüne", "linke" = "Linke"))))

write.csv(calls, "benchmark_calls_national_trend_on_21.csv", row.names = FALSE)