# ==============================================================================
# Script: R/pointwise_mutual_information.R
# Purpose: Compute Pointwise Mutual Information (PMI) for high-information words
#          mapped to specific factor levels (e.g., departments, queues).
# Output: data/pmi_results.csv
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(tidyr)
library(purrr)
library(stringr)
library(here)

cat("Loading clean data and MI results...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)
mi_results  <- read_csv(here("data", "mi_results.csv"), show_col_types = FALSE)

target_factors <- c("source", "subject", "department", "neighborhood", "reason", "type", "queue")

# 1. Prepare base tokenized dataset
cat("Preparing text tokens...\n")
text_data <- boston_data %>%
  filter(!is.na(closure_reason)) %>%
  select(case_enquiry_id, closure_reason, all_of(target_factors))

N <- n_distinct(text_data$case_enquiry_id)

tokens <- text_data %>%
  unnest_tokens(word, closure_reason) %>%
  anti_join(get_stopwords(), by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  distinct(case_enquiry_id, word, .keep_all = TRUE)

# Calculate global P(w)
word_stats <- tokens %>%
  count(word, name = "n_w") %>%
  filter(n_w >= 20) %>%
  mutate(p_w = n_w / N)

# 2. PMI Calculation Function per Factor
calculate_pmi_for_factor <- function(factor_name, token_data, mi_df, total_N, min_cooccur = 10) {
  cat("  -> Calculating PMI for factor:", factor_name, "\n")
  
  # Select high-signal words for this factor from Step 2
  informative_words <- mi_df %>%
    filter(factor == factor_name, expected_mi > 0) %>%
    pull(word)
  
  # Marginal P(c)
  category_stats <- text_data %>%
    count(level = as.character(!!sym(factor_name)), name = "n_c") %>%
    filter(!is.na(level)) %>%
    mutate(p_c = n_c / total_N)
  
  # Joint occurrences: n(w, c)
  joint_stats <- token_data %>%
    filter(word %in% informative_words) %>%
    mutate(level = as.character(!!sym(factor_name))) %>%
    filter(!is.na(level)) %>%
    count(word, level, name = "n_wc") %>%
    filter(n_wc >= min_cooccur) %>%
    mutate(p_wc = n_wc / total_N)
  
  # Compute PMI
  pmi_df <- joint_stats %>%
    inner_join(word_stats %>% select(word, n_w, p_w), by = "word") %>%
    inner_join(category_stats %>% select(level, n_c, p_c), by = "level") %>%
    mutate(
      pmi = log2(p_wc / (p_w * p_c)),
      factor = factor_name
    ) %>%
    select(factor, level, word, n_wc, n_w, n_c, pmi) %>%
    arrange(desc(pmi))
  
  return(pmi_df)
}

# 3. Execute across all factors
cat("Running PMI computation across target factors...\n")
all_pmi_results <- map_dfr(target_factors, ~calculate_pmi_for_factor(.x, tokens, mi_results, N))

write_csv(all_pmi_results, here("data", "pmi_results.csv"))
cat("PMI processing complete. Saved to data/pmi_results.csv\n")