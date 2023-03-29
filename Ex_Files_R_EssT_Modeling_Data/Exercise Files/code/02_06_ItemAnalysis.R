# Title:    Conducting an item analysis
# File:     02_06_ItemAnalysis.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(GPArotation, magrittr, pacman, psych, rio, 
  tidyverse)
# GPArotation: Gradient Projection Algorithm Rotation for FA
# magrittr: for pipes
# pacman: for loading/unloading packages
# psych: for psychometric functions
# rio: for importing data
# tidyverse: for so many reasons

# LOAD DATA ################################################

# Import data from CSV, save as "b5" for "Big Five"
b5 <- import("data/b5.csv")

# REVERSE SCORING ##########################################

# Several variables need to be reverse-scored

# Create a simple function to reverse a 1-5 scale
rev_1_5 <- function(x, na.rm = T) (6 - x)
# rev_1_7 <- function(x, na.rm = T) (8 - x)
# rev_0_10 <- function(x, na.rm = T) (10 - x)
# rev_neg_pos <- function(x, na.rm = T) (0 - x)

# List the variables that need to be reversed (I'm using
# index numbers instead of names) and apply the function;
# save revised data as "df" (save as new object to avoid
# accidentally reversing again)
df <- b5 %>%
  as_tibble() %>%
  mutate_at(
    c(2, 4, 6, 8, 10, 12, 14, 21, 23, 25, 
      27, 32, 34, 36, 38, 42, 44, 46),
    rev_1_5)

# Can repeat code if you don't want to use a function:
# df %>% 
#   mutate(
#     x = 6 - x,
#     y = 6 - y
#   )

# Check reverse coding on 2nd variable
b5 %>% pull(2) %>% glimpse()  # Original values
df %>% pull(2) %>% glimpse()  # Recoded values

# EXPLORATORY ANALYSIS #####################################

# Quick check for range of value and valid cases for the
# 38 evaluation variables
df %>% describe(skew = F)

# Plot of means and SDs for entire data set using
# error.bars() from psych
df %>% 
  error.bars(
    ylim = c(1, 5),
    sd = T, # Use SD, not SE
    arrow.col = "gray", 
    eyes = F,  # No "cat eye" plots
    pch = 19,  # Solid circles
    main = "M & SD of Big Five Personality Variables",
    ylab = "Means",
    xlab = "Personality Variables"
  )

# KEY FOR SCALE SCORES #####################################

# Scoring key using index numbers for variables
key <- list(
  Extrav =  1:10,
  Neurot = 11:20,
  Agree  = 21:30,
  Consc  = 31:40,
  Open   = 41:50
  )

# Matrix of alphas, correlations, and correlations corrected
# for attentuation using scoreItems() from psych
?scoreItems
scoreItems(key, df)

# Add scale scores to data
df %<>% 
  mutate(
    Extrav = rowMeans(df[ 1:10], na.rm = T),
    Neurot = rowMeans(df[11:20], na.rm = T),
    Agree  = rowMeans(df[21:30], na.rm = T),
    Consc  = rowMeans(df[31:40], na.rm = T),
    Open   = rowMeans(df[41:50], na.rm = T)
  )

# Descriptives for scale scores
df %>% 
  select(Extrav:Open) %>%
  describe()

# Plot of means and SDs for new variables
df %>% 
  select(Extrav:Open) %>%
  error.bars(
    ylim = c(1, 5),
    sd = T,
    arrow.col = "gray", 
    eyes = F,
    pch = 19,
    main = "M & SD of Big Five Personality Factors",
    ylab = "Means",
    xlab = "Personality Factors"
  )

# Pairs panels: histograms, scatterplots, correlations (Can
# take a long time for a large dataset)
df %>% 
  select(Extrav:Open) %>%
  pairs.panels(
    hist.col = "gray", 
    jiggle = T,
    main = "Big Five Personality Factors"
  )

# CRONBACH'S ALPHA #########################################

# The factor "Openness to Experience" is usually the least
# consistent in Big 5 analyses, so we'll look more closely.

# Pairs panels: histograms, scatterplots, correlations; but
# this is a large dataset (n = 19k) with ten variables, so
# there are k * (k-1) / 2 = 45 pairs, so it can take a long
# time. Plan wisely.

# df %>% 
#   select(O1:O10) %>% 
#   pairs.panels(
#     hist.col = "gray", 
#     jiggle = T,
#     main = "Openness to Experience"
#   )

# Scale alpha, if item dropped, statistics, frequencies.
# Need to specify "psych::alpha so R knows which package to
# get the function from.
df %>%
  select(O1:O10) %>%
  psych::alpha()
# Note: O9 is "I spend time reflecting on things."

# Histogram of item O9 to see what might be happening
df %>%
  pull(O9) %>%
  hist()

# ITEM RESPONSE THEORY #####################################

# Information on IRT Factor Analysis
?irt.fa

# IRT Factor Analysis, item and test information plots
df %>%
  select(O1:O10) %>%
  irt.fa()

# Item plot is also produced with irt.fa but this allows 
# titles to be added and charts to be reproduced
df %>%
  select(O1:O10) %>%
  irt.fa %T>%  # T-pipe
  plot(main = "Openness to Experience") %>% 
  print()

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
