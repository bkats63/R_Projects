# Title:    Computing correlations
# File:     02_03_Correlations.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(corrplot, magrittr, pacman, rio, tidyverse)
# corrplot: for visualizing correlation matrices
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Save Google Correlate variables
df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(instagram:modernDance) %>% 
  print()

# CORRELATION MATRIX #######################################

# Correlation matrix for data frame
df %>% cor()

# Fewer decimal places
df %>%
  cor() %>%     # Compute correlations
  round(2) %>%  # Round to 2 decimals
  print()

# Visualize correlation matrix with corrplot() from
# corrplot package
df %>%
  cor() %>%
  corrplot(
    type   = "upper",     # Matrix: full, upper, or lower
    diag   = F,           # Remove diagonal
    order  = "original",  # Order for labels
    tl.col = "black",     # Font color
    tl.srt = 45           # Label angle
  )

# SINGLE CORRELATION #######################################

# Use cor.test() to test one pair of variables at a time.
# cor.test() gives r, the hypothesis test, and the
# confidence interval. This command uses the "exposition
# pipe," %$%, from magrittr, which passes the columns from
# the data frame (and not the data frame itself)

df %$% cor.test(instagram, privacy)

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
