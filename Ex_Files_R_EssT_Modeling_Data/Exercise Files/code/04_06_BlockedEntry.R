# Title:    Assessing predictions with blocked-entry models
# File:     04_06_BlockedEntry.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(jmv, magrittr, pacman, rio, tidyverse)
# jmv: for regression functions
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Open saved data
df <- import("data/b5_df.rds") %>%
  print()

# Get descriptives
df %>% summary()

# LINREG WITH JMV ##########################################

# jamovi is a free, open-source, desktop application
browseURL("https://www.jamovi.org/")

# I copied this command from jamovi app in "Syntax mode." 
# Here are the steps:
# 1. Download and install jamovi
# 2. Open b5_df_jamovi.omv
# 3. Click on the menu (three dots at top right of window)
# 4. Select "Syntax mode"
# 5. Option-click on syntax and select "Syntax > Copy"
# 6. Paste syntax into RStudio
# 7. Make sure you have the jmv package installed & loaded
# 8. MUST CHANGE DATA STATEMENT TO MATCH THE NAME IN R

# Blocked entry regression from jamovi
jmv::linReg(   # Use `linReg` function from `jmv` package
  data = df,   # Change "data = data" to "data = df"
  dep = Open,  # Outcome/dependent variable
  # "covs" are quantitative predictor/independent variables
  covs = vars(age, Extrav, Neurot, Agree, Consc),
  # "factors" are categorical predictors/IVs
  factors = vars(gender, engnat),
  blocks = list(      # List block of variables
    list(             # Block 1: Demographics
      "gender",
      "age"),
    list(             # Block 2: Native English speaker
      "engnat"),
    list(             # Block 3: Other personality vars
      "Extrav",
      "Neurot",
      "Agree",
      "Consc")),
  refLevels = list(   # Reference levels for categories
    list(
      var="gender",   # For "gender" variable...
      ref="Female"),  # Use "Female" as reference level
    list(
      var="engnat",   # For "English Native Speaker"...
      ref="No")),     # Use "No" as reference level
  r2Adj = TRUE,       # Add adjusted R^2
  aic = TRUE,         # Add Akaike information criterion
  bic = TRUE,         # Add Bayesian information criterion
  modelTest = TRUE,   # Overall F-test for model
  stdEst = TRUE,      # Standardized coefficients (betas)
  ciStdEst = TRUE)    # Confidence intervals for betas

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
