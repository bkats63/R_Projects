# Title:    Conducting a confirmatory factor analysis
# File:     02_07_CFA.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(lavaan, magrittr, pacman, rio, tidyverse)
# lavaan: "LAtent VAriable ANalysis" for CFA
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Import data from CSV, save as "b5" for "Big Five"
df <- import("data/b5.csv")

# Reverse scoring not needed for the procedures we'll use

# ANALYZE DATA #############################################

df %>% names() # to see the variable names

# Define the factors
cfa.model <- 'Extrav =~ E1 + E2 + E3 + E4 + E5 +
                        E6 + E7 + E8 + E9 + E10
              Neurot =~ N1 + N2 + N3 + N4 + N5 +
                        N6 + N7 + N8 + N9 + N10
              Agree  =~ A1 + A2 + A3 + A4 + A5 +
                        A6 + A7 + A8 + A9 + A10
              Consci =~ C1 + C2 + C3 + C4 + C5 +
                        C6 + C7 + C8 + C9 + C10
              Open   =~ O1 + O2 + O3 + O4 + O5 +
                        O6 + O7 + O8 + O9 + O10'

# Fit the CFA model (this may take a while)
fit <- cfa(cfa.model, data = df)

# Check the results, quick check with "User Model versus
# Baseline Model," which should have values above 0.9 or
# possibly 0.95
fit %>% summary(fit.measures = TRUE)

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
