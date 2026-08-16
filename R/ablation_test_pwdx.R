# ==============================================================================
# Script: R/ablation_test_pwdx.R
# Purpose: Ablation study on a multi-domain department (PWDx) using 100 raw tickets.
# ==============================================================================

library(readr)
library(dplyr)
library(httr2)
library(stringr)
library(here)

OLLAMA_MODEL <- "llama3" 
OLLAMA_URL <- "http://localhost:11434/api/generate"
TARGET_DEPT <- "PWDx"

cat("Loading raw Boston 311 data...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)


# Extract raw text for PWDx
raw_notes <- boston_data %>%
  filter(department == TARGET_DEPT, !is.na(closure_reason)) %>%
  pull(closure_reason) %>%
  # Bumped the sample size to 100
  sample(min(100, length(.))) 

raw_text_block <- paste(raw_notes, collapse = "\n- ")

cat(paste("Pulled", length(raw_notes), "raw tickets for", TARGET_DEPT, ". Sending to Ollama...\n"))

# The Prompt
prompt_text <- str_glue(
  "You are a municipal operations analyst. I am providing you with a random sample of 100 raw 'closure notes' written by municipal workers in a specific city department when they close out a work ticket. 

  Based *only* on these raw notes, write a concise, 3-sentence operational profile describing exactly what this department does in the physical world.
  
  Raw Ticket Notes:
  - {raw_text_block}"
)

# API Call
req <- request(OLLAMA_URL) %>%
  req_body_json(list(
    model = OLLAMA_MODEL,
    prompt = prompt_text,
    stream = FALSE
  )) %>%
  req_timeout(180) # Increased timeout slightly for the larger text block

resp <- req_perform(req)
result <- resp_body_json(resp)$response

cat("\n======================================================\n")
cat(paste("LLM PROFILE BASED ON 100 RAW", TARGET_DEPT, "TICKETS:\n"))
cat("======================================================\n")
cat(trimws(result), "\n")
cat("======================================================\n")