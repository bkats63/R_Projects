# Title:    Computing frequencies
# File:     02_01_Frequencies.R
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

# Load data and transform pyschRegions to factor
df <- import("Data/StateData.xlsx") %>%
  as_tibble() %>%
  select(state_code, region, psychRegions) %>%
  print()

# SUMMARIZE DATAFRAMES #####################################

# Summary of all variables. 
df %>% summary()

# SUMMARIZE CATEGORICAL VARIABLES ##########################

# "region" is a character variable

# Summary() not very useful for character variables
df %>% 
  select(region) %>%
  summary()

# table() works better
df %>% 
  select(region) %>%
  table()

# SUMMARIZE FACTORS ########################################

# Convert "region" and "psychRegions" from character 
# variables to factors.
df %<>%  # Compound assignment operation = "df <- df %>%"
  mutate(
    region = as.factor(region),
    psychRegions = as.factor(psychRegions)
  ) %>%
  print()

# summary() works well with factors
df %>%
  select(region) %>%
  summary()

# Summary of all variables
df %>% summary()

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
