# Title:    Predicting outcomes with quantile regression
# File:     04_03_RobustRegression.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(GGally, magrittr, pacman, quantreg, rio, 
  tidyverse)
# GGally: for scatterplot matrix
# magrittr: for pipes
# pacman: for loading/unloading packages
# quantreg: for quantile regression
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Save Google Correlate variables
df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(instagram:modernDance) %>% 
  print()

# EXPLORE DATA #############################################

# Scatterplot matrix of all variables (takes a moment)
df %>% ggpairs()

# Scatterplot of scrapbook and modernDance
df %>% ggplot(aes(scrapbook, modernDance)) +
  geom_point(size = 3)

# Scatterplot with least squares linear regression line
df %>% ggplot(aes(scrapbook, modernDance)) +
  geom_point(size = 3) +
  geom_smooth(
    method = lm,  # "Linear model" regression line
    se = F        # No standard error (confidence bands)
  )

# Scatterplot with median regression line added
df %>% ggplot(aes(scrapbook, modernDance)) +
  geom_point(size = 3) +
  geom_smooth(        # For linear regression
    method = lm,      # "Linear model" regression line
    se = F            # No confidence bands
  ) +
  geom_quantile(      # For quantile regression line
    quantiles = 0.5,  # Use 50th percentile (median) 
    color = "red",    # Make line red
    size = 1          # Adjust thickness of line
  )

# QUANTILE REGRESSION ######################################

# Compute model, display coefficients
fit <- df %>%
  rq(
    modernDance ~ scrapbook,  # y ~ x
    data = .,                 # Data source
    tau = 0.5                 # Quantile to use (median)
  ) %>%
  print()

# Confidence intervals for coefficients
fit %>% summary()

# t-tests for coefficients
fit %>% summary(se = "boot")

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
