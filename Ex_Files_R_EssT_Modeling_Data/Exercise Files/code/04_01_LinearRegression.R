# Title:    Predicting outcomes with linear regression
# File:     04_01_LinearRegression.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(GGally, magrittr, pacman, rio, tidyverse)
# GGally: for scatterplot matrix
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

# EXPLORE DATA #############################################

# Scatterplot matrix of all variables (takes a moment)
df %>% ggpairs()

# Scatterplot of museum and volunteering
df %>% 
  ggplot(aes(museum, volunteering)) +
  geom_point(size = 3) +
  geom_smooth(method = lm)

# BIVARIATE REGRESSION #####################################

# Compute and save bivariate regression
fit1 <- df %>%                      # Save as "fit1"
  select(volunteering, museum) %>%  # y, then x
  lm()                              # Linear model

# Show model
fit1

# Summarize regression model
fit1 %>% summary()

# Confidence intervals for coefficients
fit1 %>% confint()

# Predict values of "volunteering"
fit1 %>% predict()

# Prediction intervals for values of "volunteering"
fit1 %>% predict(interval = "prediction")

# Regression diagnostics
fit1 %>% lm.influence()
fit1 %>% influence.measures()

# Diagnostic plots; run command then hit return in Console
# 1: Residuals vs. Fitted
# 2: Normal Q-Q
# 3: Scale-Location
# 4: Residuals vs. Leverage
fit1 %>% plot()

# MULTIPLE REGRESSION ######################################

# Moving the outcome to the front and removing all unneeded
# variables can make things easier
df %<>%            # Compound assignment pipe; overwrites df
  select(
    volunteering,  # Outcome variable selected first
    everything()   # Selects all other variables in df
  ) %>%
  print()

# Three ways to specify model

# Most concise; uses first variable as outcome
df %>% lm()  # Or just lm(df)

# Identify outcome, infer rest; must specify dataset
lm(volunteering ~ ., data = df)

# Identify entire model
lm(volunteering ~ instagram + facebook + retweet +
   entrepreneur + gdpr + privacy + university + 
   mortgage + museum + scrapbook + modernDance, 
   data = df)

# Save model
fit2 <- df %>% lm()

# Show model
fit2

# Summarize regression model
fit2 %>% summary()

# Confidence intervals for coefficients
fit2 %>% confint()

# Can also get predicted values of "volunteering,"
# prediction intervals, and regression diagnostics using
# commands shown in bivariate regression

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
