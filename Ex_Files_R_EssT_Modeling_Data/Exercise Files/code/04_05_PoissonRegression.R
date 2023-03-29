# Title:    Predicting outcomes with Poisson/log-linear
#           regression
# File:     04_05_PoissonRegression.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(datasets, magrittr, pacman, rio, tidyverse)
# datasets: for demonstration purposes
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Info on data
?InsectSprays

# Save to df
df <- InsectSprays

# EXPLORE DATA #############################################

# Inspect first 6 rows
df %>% head()

# Data summary
df %>% summary()

# Plot data
df %>%
  ggplot(
    aes(
      x = spray, 
      y = count, 
      fill = spray
    )
  ) + 
  geom_boxplot() +
  coord_flip() +
  theme(legend.position = "none")

# POISSON REGRESSION #######################################

# Poisson regression, also known as log-linear regression,
# models count/frequency data and contingency table. It is
# named after French mathematician Siméon Denis Poisson.

# Compute model
df %>%
  glm(                   # Generalized Linear Model
    count ~ spray,       # Count as a function of spray
    family = 'poisson',  # Use Poisson regression
    data = .             # Use df from pipe
  ) %>%
  summary()              # Summary table of results

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
