# ==============================================================================
# Script: R/calculate_npmi_topography.R
# Purpose: Extract a raw numerical NPMI dataframe and test for bimodality 
#          to determine data-driven vocabulary cutoffs.
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(diptest)
library(here)

target_dept <- "PWDx"

cat("Loading data and partitioning by Season...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)

# 1. Partition data and tokenize
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

# 2. Calculate Probabilities[cite: 3]
prob_season <- tokens %>% count(season, name = "N_c") %>% mutate(P_c = N_c / N_total)
prob_word <- tokens %>% count(word, name = "N_w") %>% filter(N_w >= 10) %>% mutate(P_w = N_w / N_total)

joint_counts <- tokens %>%
  semi_join(prob_word, by = "word") %>%
  count(word, season, name = "N_wc") %>%
  mutate(P_wc = N_wc / N_total)

# 3. EMI, PMI, and NPMI Calculation[cite: 3]
cat("Calculating Expected and Pointwise Mutual Information...\n")
mi_data <- joint_counts %>%
  inner_join(prob_season, by = "season") %>%
  inner_join(prob_word, by = "word") %>%
  mutate(
    pmi = log2(P_wc / (P_w * P_c)),
    npmi = pmi / -log2(P_c), 
    emi_component = (P_wc) * pmi 
  )

word_emi <- mi_data %>%
  group_by(word) %>%
  summarise(emi = sum(emi_component), .groups = "drop")

# 4. Extract numerical dataframe and filter for positive signals[cite: 3]
numerical_npmi_df <- mi_data %>%
  inner_join(word_emi, by = "word") %>%
  filter(emi > 0, pmi > 0)

csv_filename <- here("data", paste0(target_dept, "_numerical_npmi.csv"))
write_csv(numerical_npmi_df, csv_filename)
cat("Saved numerical NPMI dataframe to:", csv_filename, "\n\n")

# ==============================================================================
# Topography Analysis (Hartigan's Dip Test)
# ==============================================================================

cat("Running Hartigan's Dip Test for Bimodality...\n")

find_antimode <- function(x) {
  d <- density(x, n = 512)
  max_indices <- which(diff(sign(diff(d$y))) == -2) + 1
  
  if(length(max_indices) < 2) return(NA)
  
  main_peak_idx <- max_indices[which.max(d$y[max_indices])]
  right_peak_idx <- max(max_indices)
  
  if(main_peak_idx == right_peak_idx) return(NA)
  
  trough_idx <- main_peak_idx + which.min(d$y[main_peak_idx:right_peak_idx]) - 1
  return(d$x[trough_idx])
}

season_topography <- numerical_npmi_df %>%
  group_by(season) %>%
  summarise(
    dip_stat = dip.test(npmi)$statistic,
    dip_p_value = dip.test(npmi)$p.value,
    trough_npmi = find_antimode(npmi),
    justified_n = sum(npmi > trough_npmi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(dip_stat))

print(season_topography)