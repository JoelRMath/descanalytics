# ==============================================================================
# Script: R/generalized_seasonal_pipeline.R
# Purpose: Generalized EMI/PMI + LLM pipeline, saving tabular output & LLM profiles.
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(tidyr)
library(ggplot2)
library(httr2)
library(here)

OLLAMA_MODEL <- "llama3" 
OLLAMA_URL <- "http://localhost:11434/api/generate"

run_seasonal_pipeline <- function(target_dept, top_n = 30, call_nb = 1) {
  cat(paste("\n======================================================"))
  cat(paste("\nSTARTING PIPELINE FOR DEPARTMENT:", target_dept, "| CALL:", call_nb, "\n"))
  cat(paste("======================================================\n"))
  
  # 1. Load and Prep Data
  cat("Loading data and partitioning by Season...\n")
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
  
  if(nrow(prep_data) == 0) stop("No data found for this department.")
  
  # 2. Tokenize
  tokens <- prep_data %>%
    unnest_tokens(word, closure_reason) %>%
    filter(str_detect(word, "^[a-z]+$")) %>%
    anti_join(stop_words, by = "word")
  
  N_total <- nrow(tokens)
  
  # 3. Probabilities
  prob_season <- tokens %>% count(season, name = "N_c") %>% mutate(P_c = N_c / N_total)
  prob_word <- tokens %>% count(word, name = "N_w") %>% filter(N_w >= 10) %>% mutate(P_w = N_w / N_total)
  
  joint_counts <- tokens %>%
    semi_join(prob_word, by = "word") %>%
    count(word, season, name = "N_wc") %>%
    mutate(P_wc = N_wc / N_total)
  
  # 4. EMI & PMI Calculation
  cat("Calculating Expected and Pointwise Mutual Information...\n")
  mi_data <- joint_counts %>%
    inner_join(prob_season, by = "season") %>%
    inner_join(prob_word, by = "word") %>%
    mutate(
      pmi = log2(P_wc / (P_w * P_c)),
      emi_component = (P_wc) * pmi 
    )
  
  word_emi <- mi_data %>%
    group_by(word) %>%
    summarise(emi = sum(emi_component), .groups = "drop")
  
  # 5. Extract Top N and Pivot for Inspection
  cat(str_glue("Extracting top {top_n} seasonal vocabulary...\n"))
  top_words_df <- mi_data %>%
    inner_join(word_emi, by = "word") %>%
    filter(emi > 0, pmi > 0) %>%
    group_by(season) %>%
    slice_max(order_by = pmi, n = top_n, with_ties = FALSE) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  clean_table <- top_words_df %>%
    select(season, rank, word) %>%
    pivot_wider(names_from = season, values_from = word)
  
  csv_filename <- str_glue(here("data", "{target_dept}_seasonal_pmi_top{top_n}_call{call_nb}.csv"))
  write_csv(clean_table, csv_filename)
  
  # 6. LLM Blind Test
  cat("Connecting to local LLM for Blind Translation...\n")
  
  seasonal_vectors <- top_words_df %>%
    group_by(season) %>%
    summarise(words = paste(word, collapse = ", "), .groups = "drop")
  
  llm_results <- tibble(Season = character(), Profile = character())
  
  for (i in 1:nrow(seasonal_vectors)) {
    s <- seasonal_vectors$season[i]
    w_list <- seasonal_vectors$words[i]
    
    prompt_text <- str_glue(
      "You are an operations analyst. I am providing you with a list of statistically significant words extracted from the work logs of a team during a specific season. The words are sorted by statistical importance.
      
      Based *only* on these words, write a concise, 2-sentence operational profile describing exactly what physical tasks this team is performing. Do not invent tasks outside of this vocabulary.
      
      Words: {w_list}"
    )
    
    req <- request(OLLAMA_URL) %>%
      req_body_json(list(model = OLLAMA_MODEL, prompt = prompt_text, stream = FALSE)) %>%
      req_timeout(180) 
    
    resp <- tryCatch(req_perform(req), error = function(e) NULL)
    
    if(!is.null(resp)) {
      profile_text <- trimws(resp_body_json(resp)$response)
    } else {
      profile_text <- "[LLM Request Failed/Timed Out]"
    }
    
    llm_results <- bind_rows(llm_results, tibble(Season = str_to_title(s), Profile = profile_text))
    
    cat("\n------------------------------------------------------\n")
    cat(toupper(s), "PROFILE:\n")
    cat(profile_text, "\n")
  }
  
  llm_csv_filename <- str_glue(here("data", "{target_dept}_seasonal_llm_profiles_top{top_n}_call{call_nb}.csv"))
  write_csv(llm_results, llm_csv_filename)
  cat("\n======================================================\n")
  cat(paste("Saved LLM Profiles to:", llm_csv_filename, "\n"))
  cat("======================================================\n")
}

# Run the function twice to demonstrate generative volatility
run_seasonal_pipeline("PWDx", top_n = 30, call_nb = 1)
run_seasonal_pipeline("PWDx", top_n = 30, call_nb = 2)