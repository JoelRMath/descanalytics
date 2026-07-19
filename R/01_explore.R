# Load required libraries
library(readr)
library(dplyr)

# 1. Load the raw dataset
# Using read_csv from the readr package is faster and handles dates/strings better than base R
boston_311 <- read_csv("data/tmpm461rr5o.csv")

# 2. High-level structural overview
cat("\n--- Dataset Dimensions ---\n")
print(dim(boston_311)) # Shows [Rows] [Columns]

cat("\n--- Data Structure ---\n")
glimpse(boston_311) # Provides a clean vertical view of column names, types, and first few values

# 3. Calculate unique values per column
# This uses dplyr to apply n_distinct to every column, then transposes it for easy reading
unique_counts <- boston_311 %>%
  summarise(across(everything(), n_distinct)) %>%
  t() %>% 
  as.data.frame() %>%
  rename(Unique_Values = V1)

cat("\n--- Unique Values per Column ---\n")
print(unique_counts)