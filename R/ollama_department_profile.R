# ==============================================================================
# Script: R/ollama_department_profiles.R
# Purpose: Pass the sanitized PMI vocabularies to a local Ollama LLM to 
#          generate concise, 3-sentence operational profiles for each department.
# Output: data/department_profiles_llm.csv
# ==============================================================================

library(readr)
library(dplyr)
library(purrr)
library(httr2)
library(stringr)
library(tidyr)
library(here)

# 1. Configuration
# Change this to whatever model you are running locally (e.g., "llama3", "mistral")
OLLAMA_MODEL <- "llama3" 
OLLAMA_URL <- "http://localhost:11434/api/generate"

cat("Loading sanitized PMI vocabulary...\n")
pmi_data <- read_csv(here("data", "department_all19_pmi_excel.csv"), show_col_types = FALSE)

# 2. Extract just the word columns and their department names
word_cols <- grep("_word$", names(pmi_data), value = TRUE)

# 3. Define the LLM Prompt Function
generate_profile <- function(dept_col_name, word_vector) {
  dept_name <- str_remove(dept_col_name, "_word$")
  cat("  -> Requesting LLM profile for:", dept_name, "...\n")
  
  # Remove NAs and collapse into a single comma-separated string
  clean_words <- na.omit(word_vector)
  
  # Safety check for tiny departments
  if(length(clean_words) < 5) {
    return("Insufficient vocabulary to generate a structural profile.")
  }
  
  word_list <- paste(clean_words, collapse = ", ")
  
  # The engineered prompt
  prompt_text <- str_glue(
    "You are a municipal operations analyst. I am providing you with a list of the top statistically significant words extracted from the 'closure notes' of a specific city department, sorted by their Pointwise Mutual Information (PMI) score. This means these words represent the highly specific, unique physical tasks and vocabulary of this department.

    Based *only* on these words, write a concise, 3-sentence operational profile describing exactly what this department does in the physical world. Do not invent tasks outside of this vocabulary.

    Department Words: {word_list}"
  )
  
  # 4. API Call to Local Ollama
  req <- request(OLLAMA_URL) %>%
    req_body_json(list(
      model = OLLAMA_MODEL,
      prompt = prompt_text,
      stream = FALSE
    )) %>%
    # Add a timeout just in case the local model takes a minute to load into VRAM
    req_timeout(120) 
  
  resp <- req_perform(req)
  result <- resp_body_json(resp)$response
  
  return(trimws(result))
}

# 5. Execute the loop across all departments
cat(paste("Waking up local Ollama model:", OLLAMA_MODEL, "\n"))

profiles <- map2_dfr(
  word_cols, 
  pmi_data[word_cols], 
  ~tibble(
    department = str_remove(.x, "_word$"),
    operational_profile = generate_profile(.x, .y)
  )
)

write_csv(profiles, here("data", "department_profiles_llm.csv"))
cat("Process complete. Profiles saved to data/department_profiles_llm.csv\n")