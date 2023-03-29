# Title:    Creating contingency tables
# File:     02_04_ContingencyTables.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(magrittr, pacman, rio, tidyverse)
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Save categorical variables and shorten factor labels
df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(state_code, region, psychRegions) %>%
  mutate(
    psychRegions = as.factor(psychRegions),
    psychRegions = fct_recode(psychRegions,
      "Friendly" = "Friendly and Conventional",
      "Relaxed" = "Relaxed and Creative",
      "Temperamental" = "Temperamental and Uninhibited"
    )
  ) %>%
  print()

# ANALYZE DATA #############################################

# Create contingency table "ct"
ct <- df %>%
  select(region, psychRegions) %>%
  table() %>%
  print()

# Row frequencies 
ct %>% rowSums()

# Row percentages
ct %>%
  prop.table(1) %>%  # 1 is for row percentages
  round(2) %>%       # Round to two decimal places
  `*`(100)           # Multiply by 100 to read as percent

# Column frequencies
ct %>% colSums()

# Column percentages
ct %>%
  prop.table(2) %>%  # 2 is for columns percentages
  round(2) %>%
  `*`(100)

# Total percentages
ct %>%
  prop.table() %>%  # No argument for total percentages
  round(2) %>%
  `*`(100)

# Chi-squared test (but n is small)
chi <- ct %>% 
  chisq.test() %>% 
  print()

# Additional tables
chi$observed   # Observed frequencies (same as ct)
chi$expected   # Expected frequencies
chi$residuals  # Pearson's residual
chi$stdres     # Standardized residual

# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from environment

# Clear packages
p_unload(all)  # Remove all contributed packages

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # Mimics ctrl+L

# Clear mind :)
