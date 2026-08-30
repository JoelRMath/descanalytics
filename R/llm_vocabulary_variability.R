library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)

# 1. Load the 400 runs (100 per season)
llm_runs <- read_csv("data/PWDx_llm_variance_runs.csv", show_col_types = FALSE) %>%
  filter(profile_text != "[FAILED]")

# 2. Tokenize and remove stopwords
tokens <- llm_runs %>%
  unnest_tokens(word, profile_text) %>%
  anti_join(stop_words, by = "word")

# 3. Calculate Global Seasonal Vocabulary (|V_s|)
global_vocab <- tokens %>%
  group_by(season) %>%
  summarise(total_unique_words = n_distinct(word), .groups = "drop")

# 4. Calculate Profile Vocabulary (|v_s,i|) and Coverage Ratio
coverage_data <- tokens %>%
  group_by(season, run_id) %>%
  summarise(run_unique_words = n_distinct(word), .groups = "drop") %>%
  inner_join(global_vocab, by = "season") %>%
  mutate(coverage_ratio = run_unique_words / total_unique_words) %>%
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))

# Print the Global Vocabularies
print(global_vocab)

# 5. Plot the Distribution of the Coverage Ratio
coverage_plot <- ggplot(coverage_data, aes(x = season, y = coverage_ratio, fill = season)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal(base_size = 14) +
  labs(
    title = "LLM Generative Stability: Vocabulary Coverage Distribution",
    subtitle = "Lower coverage indicates higher generative variability across 100 runs.",
    x = "Season",
    y = "Coverage of Global Seasonal Vocabulary (%)"
  ) +
  theme(legend.position = "none")
print(coverage_plot)