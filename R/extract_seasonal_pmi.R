# ==============================================================================
# Script: R/extract_seasonal_pmi.R
# Purpose: Extract ALL positive PMI seasonal words for a department and save.
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(tidyr)
library(here)

extract_all_seasonal_pmi <- function(target_dept) {
  cat(paste("\nExtracting all positive PMI words for:", target_dept, "\n"))
  
  boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)
  
  prep_data <- boston_data %>%
    filter(department == target_dept, !is.na(closure_reason), !is.na(open_dt)) %>%
    mutate(
      month_num = month(open_dt),
      season = case_when(
        month_num %in% c(12, 1, 2) ~ "Winter",
        month_num %in% c(3, 4, 5)  ~ "Spring",
        month_num %in% c(6, 7, 8)  ~ "Summer",
        month_num %in% c(9, 10, 11) ~ "Fall"
      )
    )
  
  tokens <- prep_data %>%
    unnest_tokens(word, closure_reason) %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    anti_join(stop_words, by = "word")
  
  N_total <- nrow(tokens)
  
  prob_season <- tokens %>% count(season, name = "N_c") %>% mutate(P_c = N_c / N_total)
  prob_word <- tokens %>% count(word, name = "N_w") %>% filter(N_w >= 10) %>% mutate(P_w = N_w / N_total)
  
  mi_data <- tokens %>%
    semi_join(prob_word, by = "word") %>%
    count(word, season, name = "N_wc") %>%
    mutate(P_wc = N_wc / N_total) %>%
    inner_join(prob_season, by = "season") %>%
    inner_join(prob_word, by = "word") %>%
    mutate(
      pmi = log2(P_wc / (P_w * P_c)),
      emi_component = (P_wc) * pmi 
    )
  
  word_emi <- mi_data %>% group_by(word) %>% summarise(emi = sum(emi_component), .groups = "drop")
  
  # Extract ALL words where EMI > 0 and PMI > 0
  all_words_df <- mi_data %>%
    inner_join(word_emi, by = "word") %>%
    filter(emi > 0, pmi > 0) %>%
    group_by(season) %>%
    arrange(desc(pmi)) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  # Pivot wider (will naturally fill with NAs for seasons with fewer words)
  clean_table <- all_words_df %>%
    select(season, rank, word) %>%
    pivot_wider(names_from = season, values_from = word) %>%
    arrange(rank)
  
  csv_filename <- here("data", paste0(target_dept, "_seasonal_pmi_all.csv"))
  write_csv(clean_table, csv_filename)
  cat(paste("Saved comprehensive seasonal vocabulary to:", csv_filename, "\n"))
}

# Run for PWDx
extract_all_seasonal_pmi("PWDx")