# ==============================================================================
# Script: R/3_analyze_llm_sentiment_bleed.R
# Purpose: Quantify semantic hallucination by measuring the intrusion of 
#          sentiment/emotion into physical operational profiles.
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(here)

TARGET_DEPT <- "PWDx"
input_file <- here("data", str_glue("{TARGET_DEPT}_llm_variance_runs.csv"))

cat("Loading cached LLM generations...\n")
llm_runs <- read_csv(input_file, show_col_types = FALSE) %>%
  filter(profile_text != "[FAILED]")

# Tokenize the generated AI profiles
llm_tokens <- llm_runs %>%
  unnest_tokens(word, profile_text) %>%
  anti_join(stop_words, by = "word")

# Count total words per run (to use as a denominator)
total_words_per_run <- llm_tokens %>%
  count(season, run_id, name = "total_words")

cat("Calculating Affective Density (Behavioral Bleed)...\n")
# Join with the bing sentiment lexicon to flag behavioral/emotional words
sentiment_bleed <- llm_tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(season, run_id, name = "sentiment_words") %>%
  # Join back to the total words to calculate a percentage
  right_join(total_words_per_run, by = c("season", "run_id")) %>%
  # Replace NAs with 0 (runs that were perfectly neutral)
  mutate(
    sentiment_words = replace_na(sentiment_words, 0),
    affective_density = sentiment_words / total_words
  ) %>%
  # Calculate the mean affective density across all 400 runs per season
  group_by(season) %>%
  summarise(mean_affective_density = mean(affective_density), .groups = "drop") %>%
  # Order chronologically
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))

# Generate the Behavioral Bleed Plot
bleed_plot <- ggplot(sentiment_bleed, aes(x = season, y = mean_affective_density, fill = season)) +
  geom_col(color = "black", alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Semantic Hallucination: Behavioral Bleed by Season",
    subtitle = "Percentage of generated text utilizing sentiment-charged behavioral vocabulary.",
    x = "Season",
    y = "Affective Density (Mean % of Output)"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

ggsave(here("images", "fig_llm_sentiment_bleed.png"), plot = bleed_plot, width = 8, height = 5, dpi = 300)
cat("Analysis complete. Sentiment plot saved.\n")
print(sentiment_bleed)