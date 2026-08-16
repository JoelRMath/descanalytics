# ==============================================================================
# Script: R/expected_mutual_information.R
# Purpose: Calculate Expected Mutual Information (MI) between closure notes 
#          and structural municipal factors to isolate high-signal vocabulary.
# Output: data/mi_results.csv
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(tidyr)
library(purrr)
library(stringr)
library(here)

cat("Loading dataset...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)

# Define the "Goldilocks" factors established in Step 1
target_factors <- c("source", "subject", "department", "neighborhood", "reason", "type", "queue")

# 1. Isolate target variables and drop empty text fields
cat("Preparing text data...\n")
text_data <- boston_data %>%
  filter(!is.na(closure_reason)) %>%
  select(case_enquiry_id, closure_reason, all_of(target_factors))

# Total number of valid documents (N)
N <- n_distinct(text_data$case_enquiry_id)

# 2. Tokenize and calculate Document Frequency
cat("Tokenizing unstructured text (extracting Document Frequency)...\n")
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

# Calculate P(word) and filter out extremely rare noise (min 20 occurrences)
word_counts <- tokens %>%
  count(word, name = "n_word") %>%
  filter(n_word >= 20) %>%
  mutate(p_word = n_word / N)

# Bind the clean, filtered words back to the dataset
clean_tokens <- tokens %>%
  inner_join(word_counts %>% select(word), by = "word")

# 3. MI Calculation Function
calculate_mi_for_factor <- function(factor_name, token_data, total_N) {
  cat("  -> Calculating Expected MI for:", factor_name, "\n")
  
  # P(c): Probability of the category
  category_counts <- text_data %>%
    count(!!sym(factor_name), name = "n_c") %>%
    mutate(p_c = n_c / total_N)
  
  # P(word, c): Joint probability of the word and the category
  joint_counts <- token_data %>%
    count(word, !!sym(factor_name), name = "n_word_c") %>%
    mutate(p_word_c = n_word_c / total_N)
  
  # Execute the MI Formula
  mi_data <- joint_counts %>%
    inner_join(word_counts %>% select(word, p_word), by = "word") %>%
    inner_join(category_counts %>% select(!!sym(factor_name), p_c), by = factor_name) %>%
    mutate(
      # MI Component: P(w,c) * log2( P(w,c) / (P(w)*P(c)) )
      mi_component = p_word_c * log2(p_word_c / (p_word * p_c))
    ) %>%
    # Sum across all categories for each word to get Expected MI
    group_by(word) %>%
    summarise(
      expected_mi = sum(mi_component),
      .groups = "drop"
    ) %>%
    mutate(factor = factor_name) %>%
    arrange(desc(expected_mi))
  
  return(mi_data)
}

# 4. Execute across all factors and save
cat("Initiating Mutual Information calculations...\n")
all_mi_results <- map_dfr(target_factors, ~calculate_mi_for_factor(.x, clean_tokens, N))

write_csv(all_mi_results, here("data", "mi_results.csv"))
cat("Process complete. Results saved to data/mi_results.csv\n")