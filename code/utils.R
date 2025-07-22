# ----------------------------------------------------------
# Utility Functions for District-Level Swing Model Project
# Authors: Lukas Stoetzer & Cornelius Erfort
# ----------------------------------------------------------

#' Estimate latent support using a random walk model
get_latent_support <- function(data, party, date, last_election_date,
                               var_party, var_date, var_percent) {
  require(dlm)
  data[[var_date]] <- as.Date(data[[var_date]])
  filtered_data <- data[data[[var_party]] == party & data[[var_date]] < date, ]
  filtered_data <- filtered_data[order(filtered_data[[var_date]]), ]
  complete_data <- data.frame(Datum = seq.Date(from = last_election_date, to = date, by = "day"))
  merged_data <- merge(complete_data, filtered_data, by.x = "Datum", by.y = "date", all.x = TRUE)
  start_date <- min(merged_data$Datum)
  ts_data <- ts(merged_data$poll_share, start = c(as.numeric(format(start_date, "%Y")),
                                                  as.numeric(format(start_date, "%j"))), frequency = 365)
  build_dlm <- function(param) {
    dlmModPoly(order = 1, dV = exp(param[1]), dW = exp(param[2]))
  }
  fit <- dlmMLE(ts_data, parm = c(0, 0), build = build_dlm)
  model <- build_dlm(fit$par)
  filtered <- dlmFilter(ts_data, model)
  smoothed <- dlmSmooth(filtered)
  latent_support <- smoothed$s[-1]
  if (nrow(filter(filtered_data, !is.na(poll_share))) > 1) return(latent_support[length(latent_support)]) else return(NA)
}

#' Extract polling data from Wahlrecht XML
wahlrecht_xml_extract <- function(xml_url) {
  require(xml2)
  xml_doc <- as_list(read_xml(xml_url))
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
    dplyr::rename(institute = inst, date = dat, party = werte_id, value = werte, sample_size = bfrg) %>% 
    as.data.frame %>% 
    mutate(date = as.Date(date)) %>%
    pivot_wider(values_from = "value", names_from = "party") 
  if("agart" %in% names(xml_df)) xml_df <- dplyr::select(xml_df, -agart)
  xml_df
}

#' Download and combine current and historical Wahlrecht polls
get_wahlrecht_polls <- function() {
  new_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_aktuell.xml") %>% wahlrecht_xml_extract
  old_polls <- ("https://www.wahlrecht.de/umfragen/xml/bundestag_archiv.xml") %>% wahlrecht_xml_extract
  old_polls$date %>% min
  wahlrecht_polls <- bind_rows(new_polls, old_polls)
  wahlrecht_polls <- wahlrecht_polls %>% 
    pivot_longer(cols = cdu:afd, names_to = "party", values_to = "poll_share") %>% 
    pivot_wider(names_from = party, values_from = poll_share, values_fn = mean) 
  wahlrecht_polls <- wahlrecht_polls %>% arrange(desc(date))
  wahlrecht_polls$sample_size <- as.numeric(wahlrecht_polls$sample_size)
  return(wahlrecht_polls)
} 