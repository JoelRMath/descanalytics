# ==============================================================================
# Script: R/ablation_test_ollama.R
# Purpose: Ablation study to test LLM generation on raw text vs PMI vocabulary.
# ==============================================================================

library(readr)
library(dplyr)
library(httr2)
library(stringr)
library(here)

# 1. Configuration
OLLAMA_MODEL <- "llama3" 
OLLAMA_URL <- "http://localhost:11434/api/generate"
TARGET_DEPT <- "ANML"

cat("Loading raw Boston 311 data...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)

# 2. Extract raw text for the target department
raw_notes <- boston_data %>%
  filter(department == TARGET_DEPT, !is.na(closure_reason)) %>%
  pull(closure_reason) %>%
  # Sample 50 random tickets to fit the LLM context window safely
  sample(min(50, length(.))) 

# Collapse into a single block of raw text, separated by newlines
raw_text_block <- paste(raw_notes, collapse = "\n- ")

cat(paste("Pulled", length(raw_notes), "raw tickets. Sending to Ollama...\n"))

# 3. The Prompt (Modified for Raw Text)
prompt_text <- str_glue(
  "You are a municipal operations analyst. I am providing you with a random sample of raw 'closure notes' written by municipal workers in a specific city department when they close out a work ticket. 

  Based *only* on these raw notes, write a concise, 3-sentence operational profile describing exactly what this department does in the physical world.
  
  Raw Ticket Notes:
  - {raw_text_block}"
)

# 4. API Call
req <- request(OLLAMA_URL) %>%
  req_body_json(list(
    model = OLLAMA_MODEL,
    prompt = prompt_text,
    stream = FALSE
  )) %>%
  req_timeout(120) 

resp <- req_perform(req)
result <- resp_body_json(resp)$response

cat("\n======================================================\n")
cat("LLM PROFILE BASED ON RAW TEXT (THE ABLATION TEST):\n")
cat("======================================================\n")
cat(trimws(result), "\n")
cat("======================================================\n")