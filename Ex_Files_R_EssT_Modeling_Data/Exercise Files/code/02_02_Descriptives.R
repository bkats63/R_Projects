# Title:    Computing descriptive statistics
# File:     02_02_Descriptives.R
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

# Use "StateData.xlxs" from exercise files
df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(state_code, 
    region,
    psychRegions,
    instagram:entrepreneur) %>% 
  mutate(
    region = as.factor(region),
    psychRegions = as.factor(psychRegions)
    ) %>%
  print()

# SUMMARY ##################################################

# Summary for entire table
df %>% summary()

# Summary for one variable
df %>%
  select(entrepreneur) %>%  # select() returns a data frame
  summary()

df %>%
  pull(entrepreneur) %>%  # pull() returns a vector
  summary()

# BOXPLOT STATISTICS #######################################

# Boxplot of "entrepreneur" with ggplot2
df %>%
  select(entrepreneur) %>%
  ggplot(., aes(y = entrepreneur, x = 1)) +
  geom_boxplot(notch = TRUE) +  # Boxplot with CI
  coord_flip()  # Display horizontal

# Boxplot stats: hinges, n, CI for median, outliers
df %>%
  pull(entrepreneur) %>%
  boxplot.stats()

# ADDITIONAL DESCRIPTIVES ##################################

# The "psych" package has additional descriptive statistics 
# with the describe() function
p_load(psych)

# Describe all variables
df %>% describe()  # Uses the internal codes for factors

# Describe one variable
df %>% 
  pull(entrepreneur) %>%  # Use pull(), not select()
  describe()

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
