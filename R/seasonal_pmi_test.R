# ==============================================================================
# Script: R/seasonal_pmi_test.R
# Purpose: Apply the EMI/PMI pipeline to extract Seasonal vocabulary for PWDx.
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(tidyr)
library(here)

cat("Loading and prepping seasonal data...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)

# 1. Filter for PWDx and Engineer the 'Season' factor
prep_data <- boston_data %>%
  filter(department == "PWDx", !is.na(closure_reason), !is.na(open_dt)) %>%
  mutate(
    month_num = month(open_dt),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5)  ~ "Spring",
      month_num %in% c(6, 7, 8)  ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall"
    )
  )

# 2. Tokenize the text
cat("Tokenizing text...\n")
tokens <- prep_data %>%
  unnest_tokens(word, closure_reason) %>%
  filter(str_detect(word, "^[a-z]+$")) %>% # Keep only purely alphabetical words
  anti_join(stop_words, by = "word")

# 3. Calculate Global Probabilities
N_total <- nrow(tokens)

# Probability of each Season P(c)
prob_season <- tokens %>%
  count(season, name = "N_c") %>%
  mutate(P_c = N_c / N_total)

# Probability of each Word P(w)
prob_word <- tokens %>%
  count(word, name = "N_w") %>%
  filter(N_w >= 10) %>% # Basic frequency filter to drop extreme outliers
  mutate(P_w = N_w / N_total)

# 4. Calculate Joint Probability P(w, c) and PMI
cat("Calculating PMI...\n")
pmi_results <- tokens %>%
  # Filter to only keep words that passed the frequency threshold
  semi_join(prob_word, by = "word") %>% 
  # Count the word occurrences per season (this drops everything except word, season, and N_wc)
  count(word, season, name = "N_wc") %>%
  mutate(P_wc = N_wc / N_total) %>%
  # Bring the probabilities back in
  inner_join(prob_season, by = "season") %>%
  inner_join(prob_word, by = "word") %>% 
  mutate(
    # PMI Equation: log2 ( P(w,c) / (P(w) * P(c)) )
    pmi = log2(P_wc / (P_w * P_c))
  )

# 5. Extract the Top 20 Seasonal Words
top_seasonal_words <- pmi_results %>%
  filter(pmi > 0) %>% # Keep only words attracted to the season
  group_by(season) %>%
  slice_max(order_by = pmi, n = 20, with_ties = FALSE) %>%
  select(season, word, pmi, N_wc) %>%
  arrange(season, desc(pmi))

# 6. Pivot for clean console viewing
clean_table <- top_seasonal_words %>%
  mutate(rank = row_number()) %>%
  select(season, rank, word) %>%
  pivot_wider(names_from = season, values_from = word)

cat("\n======================================================\n")
cat("TOP 20 SEASONAL WORDS FOR PUBLIC WORKS (PWDx):\n")
cat("======================================================\n")
print(clean_table, n = 20)
cat("======================================================\n")