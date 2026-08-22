# ==============================================================================
# Script: R/plot_seasonal_pmi.R
# Purpose: Plot PMI distributions by season for PWDx to inspect signal tails.
# ==============================================================================

library(readr)
library(dplyr)
library(tidytext)
library(lubridate)
library(stringr)
library(ggplot2)
library(here)

cat("Loading and prepping seasonal data...\n")
boston_data <- read_csv(here("data", "boston_clean.csv"), show_col_types = FALSE)

prep_data <- boston_data %>%
  filter(department == "PWDx", !is.na(closure_reason), !is.na(open_dt)) %>%
  mutate(
    month_num = month(open_dt),
    season = case_when(
      month_num %in% c(12, 1, 2) ~ "Winter",
      month_num %in% c(3, 4, 5)  ~ "Spring",
      month_num %in% c(6, 7, 8)  ~ "Summer",
      month_num %in% c(9, 10, 11) ~ "Fall"
    ),
    # Lock in the factor order for the plot facets
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
  )

tokens <- prep_data %>%
  unnest_tokens(word, closure_reason) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  anti_join(stop_words, by = "word")

N_total <- nrow(tokens)

prob_season <- tokens %>% count(season, name = "N_c") %>% mutate(P_c = N_c / N_total)
prob_word <- tokens %>% count(word, name = "N_w") %>% filter(N_w >= 10) %>% mutate(P_w = N_w / N_total)

mi_data <- tokens %>%
  semi_join(prob_word, by = "word") %>%
  count(word, season, name = "N_wc") %>%
  mutate(P_wc = N_wc / N_total) %>%
  inner_join(prob_season, by = "season") %>%
  inner_join(prob_word, by = "word") %>%
  mutate(
    pmi = log2(P_wc / (P_w * P_c)),
    emi_component = (P_wc) * pmi 
  )

word_emi <- mi_data %>% group_by(word) %>% summarise(emi = sum(emi_component), .groups = "drop")

# Filter for valid structural words (EMI > 0 and PMI > 0)
plot_data <- mi_data %>%
  inner_join(word_emi, by = "word") %>%
  filter(emi > 0, pmi > 0)

cat("Generating PMI Distribution Plot...\n")
p <- ggplot(plot_data, aes(x = pmi, fill = season)) +
  geom_histogram(bins = 40, color = "black", linewidth = 0.2, alpha = 0.8) +
  facet_wrap(~ season, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(
    trans = "log1p", 
    breaks = c(0, 1, 10, 100, 1000), 
    labels = scales::comma
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "The Signal Filter: Pointwise Mutual Information (PMI) by Season",
    subtitle = "Distribution of positive PMI scores for PWDx. Inspecting the right tail for signal density.",
    x = "Pointwise Mutual Information (Bits)",
    y = "Number of Words (log scale)",
    fill = "Season"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

print(p)