library(readr)
library(dplyr)
library(diptest)
library(purrr)
library(ggplot2)

# Assume mi_data is the dataframe containing the NPMI scores from your earlier extraction script.
# (Filtering for emi > 0 and npmi > 0 to match our established baseline)

# 1. Function to find the local minimum (the trough) between two peaks in a density curve
find_antimode <- function(x) {
  d <- density(x, n = 512)
  
  # Find the indices of the peaks (local maxima)
  max_indices <- which(diff(sign(diff(d$y))) == -2) + 1
  
  # If less than 2 peaks are found, return NA (unimodal)
  if(length(max_indices) < 2) return(NA)
  
  # Focus on the highest peak (the main bulk) and the right-most peak (the 1.0 spike)
  main_peak_idx <- max_indices[which.max(d$y[max_indices])]
  right_peak_idx <- max(max_indices)
  
  # If they are the same, no true dip exists in the right tail
  if(main_peak_idx == right_peak_idx) return(NA)
  
  # Find the minimum (trough) between the main peak and the right-most peak
  trough_idx <- main_peak_idx + which.min(d$y[main_peak_idx:right_peak_idx]) - 1
  
  return(d$x[trough_idx])
}

# 2. Apply Hartigan's Dip Test and Cutoff Extraction by Season
season_topography <- mi_data %>%
  filter(emi > 0, npmi > 0) %>%
  group_by(season) %>%
  summarise(
    # Hartigan's Dip Test for unimodality
    dip_stat = dip.test(npmi)$statistic,
    dip_p_value = dip.test(npmi)$p.value,
    
    # Extract the mathematical trough
    trough_npmi = find_antimode(npmi),
    
    # Calculate how many words fall to the right of the trough
    justified_n = sum(npmi > trough_npmi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(dip_stat))

print(season_topography)