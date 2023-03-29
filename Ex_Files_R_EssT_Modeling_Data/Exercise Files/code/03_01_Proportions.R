# Title:    Comparing proportions
# File:     03_01_Proportions.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(magrittr, pacman, survival, tidyverse)
# magrittr: for pipes
# pacman: for loading/unloading packages
# survival: for sample dataset
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Info on "NCCTG Lung Cancer Data" from survival
?lung

# Save data as df (dataframe)
df <- lung %>%
  select(status, sex) %>%
  as_tibble() %>%
  print()

# Recode 'sex' and 'status' from numeric to names
df %<>% 
  mutate(
    status = ifelse(status == 1, "alive", "dead"),
    sex    = ifelse(sex    == 1, "male", "female")
  ) %>%
  print()

# Create frequency table, save for reuse
ptable <- df %>%           # Save table for reuse
  select(sex, status) %>%  # Variables for table
  table() %>%              # Create 2 x 2 table
  print()                  # Show table

# CHI-SQUARED TEST #########################################

# Get chi-squared test for sex and status
ptable %>% chisq.test()

# PROPORTIONS TEST #########################################

# Get survival proportions by sex
df %>%
  group_by(sex, status) %>%  # Variables to group by
  summarize(n = n()) %>%     # Calculate n for each group
  mutate(freq = n / sum(n))  # Proportions by sex

# Proportions test
ptable %>% prop.test()

# Alternative Hypothesis: Is survival greater for female
# patients than for male patients? (With 80% CI)
ptable %>% 
  prop.test(
    alt = "greater",  # Specify directional hypothesis
    conf.level = .80  # Specify 80% confidence level
  ) 

# MANY PROPORTIONS #########################################

tibble(                             # Create new tibble
  n = c(rep(100, 5)),               # 100 trials 5 times
  # n = c(100, 100, 100, 100, 100)  # Or this way
  x = seq(65, 45, by = -5)          # Number of successes
  # x = c(65, 60, 55, 50, 45)       # Or this way
  ) %$%                             # Exposition pipe
  prop.test(x, n)                   # Proportion test

# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from environment

# Clear packages
p_unload(all)  # Remove all contributed packages

# Clear console
cat("\014")  # Mimics ctrl+L

# Clear mind :)
