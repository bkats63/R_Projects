# Title:    Predicting outcomes with lasso regression
# File:     04_02_LassoRegression.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(lars, magrittr, pacman, rio, tidyverse)
# lars: for lasso regression
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Import data
df <- import("data/winequality-red.csv") %>%
  as_tibble() %>%
  print()

# Fix variable names
df %<>%
  rename_all(list(~make.names(.))) %>%  
  print()

# Get descriptives
df %>% summary()

# Variables need to be on same scale
df %<>%        # Compound assignment pipe
  scale() %>%  # Converts all variables to M = 0 & SD = 1
  as_tibble()  # Converts (again) to tibble

# Get descriptives again
df %>% summary()

# Data as matrix for lars package
y <- df %>% select( quality) %>% as.matrix()
X <- df %>% select(-quality) %>% as.matrix()

# STEPWISE REGRESSION ######################################

# Compute model
fit_step <- lars(X, y, type = "stepwise")

# Plot results
fit_step %>% plot()      # Plot coefficients
legend(                  # Add legend
  "bottomleft",          # Position of legend
  lwd = 2,               # Line size
  col = (1:nrow(X)),     # Colors
  legend = colnames(X),  # Variable names
  cex = .7               # Font size
)

# View more results
fit_step %>% view()      # View object with results
fit_step$R2              # Show R^2 at each step
fit_step$R2 %>% plot()   # Plot R^2 at each step
fit_step %>% coef()      # Get coefficients at each step
fit_step                 # Step for each variable

# LASSO REGRESSION #########################################

# LASSO: Least Absolute Shrinkage and Selection Operator.
# Can also specify "forward.stagewise", which is like
# stepwise regression but with better generalizability, or
# "lar" for Least Angle Regression (LARS)

# Compute model
fit_lasso <- lars(X, y, type = "lasso")

# Plot results
fit_lasso %>% plot()     # Plot coefficients
legend(                  # Add legend
  "bottomleft",          # Position of legend
  lwd = 2,               # Line size
  col = (1:nrow(X)),     # Colors
  legend = colnames(X),  # Variable names
  cex = .7               # Font size
)

# View more results
fit_lasso$R2             # Show R^2 at each step
fit_lasso$R2 %>% plot()  # Plot R^2 at each step
fit_lasso %>% coef()     # Get coefficients at each step
fit_lasso                # Step for each variable

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
