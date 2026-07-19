library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Load data
boston_311 <- read_csv("data/tmpm461rr5o.csv")

cat("\n--- 1. Missingness Audit ---\n")
# Check how many cases are actually closed vs missing a closed date
boston_311 %>%
  group_by(case_status) %>%
  summarise(
    Total_Cases = n(),
    Missing_Closed_Date = sum(is.na(closed_dt))
  ) %>%
  print()

cat("\n--- 2. Resolution Time Distribution (Days) ---\n")
# Calculate resolution time and look at the quick summary stats
boston_metrics <- boston_311 %>%
  filter(case_status == "Closed", !is.na(closed_dt)) %>%
  mutate(
    # difftime calculates the exact distance; we specify "days"
    days_to_close = as.numeric(difftime(closed_dt, open_dt, units = "days"))
  )

summary(boston_metrics$days_to_close)

cat("\n--- 3. Unpacking closure_reason Text ---\n")
# Print a sample of closure reasons to see if it's purely programmatic or has manual notes
boston_311 %>%
  filter(!is.na(closure_reason)) %>%
  select(closure_reason) %>%
  head(10) %>%
  pull(closure_reason) %>%
  walk(~cat("- ", ., "\n"))