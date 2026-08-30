# ==============================================================================
# Script: R/1_generate_llm_variance_data.R
# Purpose: Calculate PMI once, then query the LLM 100 times per season to 
#          build a dataset for variance analysis.
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(httr2)
library(here)

OLLAMA_MODEL <- "llama3" 
OLLAMA_URL <- "http://localhost:11434/api/generate"
TARGET_DEPT <- "PWDx"
TOP_N <- 30
TOTAL_RUNS <- 400

cat("Step 1: Loading data and calculating PMI once...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)

tokens <- boston_data %>%
  filter(department == TARGET_DEPT, !is.na(closure_reason), !is.na(open_dt)) %>%
  mutate(
    season = case_when(
      month(open_dt) %in% c(12, 1, 2) ~ "Winter",
      month(open_dt) %in% c(3, 4, 5)  ~ "Spring",
      month(open_dt) %in% c(6, 7, 8)  ~ "Summer",
      month(open_dt) %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  unnest_tokens(word, closure_reason) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  anti_join(stop_words, by = "word")

N_total <- nrow(tokens)
prob_season <- tokens %>% count(season, name = "N_c") %>% mutate(P_c = N_c / N_total)
prob_word <- tokens %>% count(word, name = "N_w") %>% filter(N_w >= 10) %>% mutate(P_w = N_w / N_total)

seasonal_vectors <- tokens %>%
  semi_join(prob_word, by = "word") %>%
  count(word, season, name = "N_wc") %>%
  mutate(P_wc = N_wc / N_total) %>%
  inner_join(prob_season, by = "season") %>%
  inner_join(prob_word, by = "word") %>%
  mutate(
    pmi = log2(P_wc / (P_w * P_c)),
    emi = (P_wc) * pmi
  ) %>%
  filter(emi > 0, pmi > 0) %>%
  group_by(season) %>%
  slice_max(order_by = pmi, n = TOP_N, with_ties = FALSE) %>%
  summarise(words = paste(word, collapse = ", "), .groups = "drop")

cat("Step 2: Commencing 100 LLM generation loops per season...\n")
llm_results <- tibble(season = character(), run_id = integer(), profile_text = character())

for (run in 1:TOTAL_RUNS) {
  message(str_glue("  -> Executing Run {run} of {TOTAL_RUNS}..."))
  
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
    
    profile_text <- if(!is.null(resp)) trimws(resp_body_json(resp)$response) else "[FAILED]"
    
    llm_results <- bind_rows(llm_results, tibble(
      season = s, 
      run_id = run, 
      profile_text = profile_text
    ))
  }
}

output_file <- here("data", str_glue("{TARGET_DEPT}_llm_variance_runs.csv"))
write_csv(llm_results, output_file)
cat("\n======================================================\n")
cat(str_glue("Process Complete. {nrow(llm_results)} LLM profiles saved to {output_file}\n"))