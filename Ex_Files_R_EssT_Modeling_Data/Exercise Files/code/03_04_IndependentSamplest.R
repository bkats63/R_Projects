# Title:    Comparing two means: Independent-samples t-test
# File:     03_04_IndepedentSamplest.R
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

# Get info on "sleep" dataset
?sleep

# Prepare and save data
df <- 
  sleep %>%
  as_tibble() %>%
  select(                   # Select and rename variables
    drug  = group,          # Select group, rename as drug
    sleep = extra           # Select extra, rename as sleep
  ) %>%
  mutate(
    drug = ifelse(          # Create labels
      drug == 1,            # Test if group = 1
      "L-Hyoscyamine",      # If true
      "L-Hyoscine"          # If false
    ), 
    drug = as_factor(drug)  # Convert from chr to fct
  ) %>%
  print()

# EXPLORE DATA #############################################

# Density plots by drug group
df %>%
  ggplot(
    aes(
      x = sleep, 
      fill = drug
    )
  ) + 
  geom_density(alpha = 0.5) +
  theme(legend.position = "bottom")

# Boxplot of "extra" sleep by drug group
df %>%
  ggplot(
    aes(
      x = drug, 
      y = sleep, 
      fill = drug
    )
  ) + 
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  theme(legend.position = "none")

# T-TESTS ##################################################

# Independent 2-group t-test (with defaults)
df %>%
  t.test(          # Generic t-test function
    sleep ~ drug,  # Sleep as a function of drug
    data = .       # Data is in df
  )

# t-test with options
df %>%
  t.test(
    sleep ~ drug, 
    data = .,
    alternative = "less",  # Use directional test
    conf.level = 0.80      # 80% CI (vs. 95%)
  )

# GROUP DATA IN SEPARATE VARIABLES #########################

# Create two groups of random data in separate variables
# Good because actual difference is known
set.seed(121)  # Set random number seed for replicability
a <- rnorm(30, mean = 23, sd = 5)
b <- rnorm(30, mean = 20, sd = 5)
t.test(a, b)

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
