# Title:    Comparing means with multiple categorical 
#           predictors: Factorial analysis of variance
# File:     03_06_FactorialANOVA.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(datasets, magrittr, pacman, tidyverse)
# datasets: for demonstration purposes
# magrittr: for pipes
# pacman: for loading/unloading packages
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Get info on "warpbreaks"
?warpbreaks

# Save data
df <- warpbreaks %>% 
  as_tibble() %>% 
  print()

# EXPLORE DATA #############################################

# Grouped boxplots
df %>% 
  ggplot(
    aes(
      x = wool,       # Grouping variable
      y = breaks,     # Outcome variable
      fill = tension  # Subgroup variable
    )
  ) + 
  geom_boxplot()

# Descriptive statistics by group
 df %>%
   group_by(wool, tension) %>%   # Grouping variables
   summarize(
     M = mean(breaks),           # Mean number of breaks
     SD = sd(breaks)             # Standard deviations
   ) %>%
   print.data.frame(digits = 3)  # Control decimals

# ANOVA ####################################################

# Model with interaction
fit <- df %>%
  aov(
    breaks ~ wool + tension + wool*tension, 
    # or: wool:tension, 
    data = .
  )

# Summary
fit %>% summary()

# Additional information on model
fit %>% model.tables()                    # Table of effects
# fit %>% model.tables(type = "effects")  # Same output
fit %>% model.tables(type = "means")      # All means

# POST-HOC TESTS ###########################################

# Tukey HSD (Honestly Significant Difference)
fit %>% TukeyHSD()

# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from environment

# Clear packages
detach("package:datasets", unload = T)  # For base packages
p_unload(all)  # Remove all contributed packages

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # Mimics ctrl+L

# Clear mind :)
