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
  filter(
    !str_detect(word, "^[0-9]+$"),                # Kills pure numbers
    !str_detect(word, "^[a-z]+\\.[a-z]+$"),       # Kills names/emails (eric.mcgevna, recovered.jg)
    !str_detect(word, "^[0-9a-f]{15,}$"),         # Kills massive hex strings/UUIDs
    !str_detect(word, "^[0-9]+[a-z]{1,2}$"),      # Kills short codes (3dw, 1dt)
    !str_detect(word, "\\.com$|\\.html$|\\.org$") # Kills domains and web files
  ) %>%
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
      max_pmi = -log2(p_c),
      scaled_pmi = pmi / max_pmi,
      factor = factor_name
    ) %>%
    filter(pmi > 0) %>%
    select(factor, level, word, n_wc, n_w, n_c, pmi, max_pmi, scaled_pmi) %>%
    arrange(desc(scaled_pmi))
  
  return(pmi_df)
}

# 3. Execute across all factors
cat("Running PMI computation across target factors...\n")
all_pmi_results <- map_dfr(target_factors, ~calculate_pmi_for_factor(.x, tokens, mi_results, N))

write_csv(all_pmi_results, here("data", "pmi_results.csv"))
cat("PMI processing complete. Saved to data/pmi_results.csv\n")

# ==============================================================================
# 4. Generate Wide CSV for Excel Exploration (All 19 Depts, Top 200 Words & Scaled PMI)
# ==============================================================================
cat("Generating wide-format CSV for Excel exploration with Scaled PMI scores...\n")

# Identify ALL 19 departments, ordered by overall ticket volume (n_c)
all_deps <- all_pmi_results %>%
  filter(factor == "department") %>%
  distinct(level, n_c) %>%
  arrange(desc(n_c)) %>%
  pull(level)

# Create the wide dataframe (Top 200 words per department, storing both word and scaled_pmi)
department_wide <- all_pmi_results %>%
  filter(factor == "department") %>%
  group_by(level) %>%
  # Rank by Scaled PMI first, tie-break by co-occurrence volume
  arrange(desc(scaled_pmi), desc(n_wc)) %>%
  mutate(
    # Format the Scaled PMI score to two decimal places immediately
    formatted_pmi = sprintf("%.2f", scaled_pmi),
    # Combine word and scaled pmi score into a single string (e.g., "globe (0.97)")
    word_with_pmi = paste0(word, " (", formatted_pmi, ")")
  ) %>%
  slice_head(n = 200) %>% 
  # Create a row index strictly for pivot_wider alignment
  mutate(row_id = row_number()) %>% 
  ungroup() %>%
  select(row_id, level, word_with_pmi) %>%
  pivot_wider(
    id_cols = row_id, 
    names_from = level,
    values_from = word_with_pmi,
    names_glue = "{level}_word" # Keeps the naming convention expected by your Quarto script
  ) %>%
  select(-row_id) 

# Ensure columns align with the volume-ordered department list
ordered_cols <- paste0(all_deps, "_word")
department_wide <- department_wide %>% select(any_of(ordered_cols))

write_csv(department_wide, here("data", "department_all19_pmi_excel.csv"))
cat("Saved wide format with Scaled PMI values to data/department_all19_pmi_excel.csv\n")