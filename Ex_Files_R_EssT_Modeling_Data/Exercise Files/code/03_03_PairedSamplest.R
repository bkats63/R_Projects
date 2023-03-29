# Title:    Comparing paired means: Paired-samples t-test
# File:     03_03_PairedSamplest.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(GGally, magrittr, pacman, tidyverse)
# GGally: for parallel plots
# magrittr: for pipes
# pacman: for loading/unloading packages
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Create random data for a company sold units before (t1 for
# "time 1") and after (t2 for "time 2") marketing campaign

# Set seed for random numbers so data are reproducible
set.seed(40)  # Pick any number

# Create dataset
df <- 
  tibble(
    t1 = rnorm(30, mean = 100, sd = 20) %>% round(),
    t2 = t1 + rnorm(30, mean = 10, sd = 5) %>% round()
  ) %>%
  print()

# Check data
df %>% summary()

# EXPLORE DATA #############################################

# Density plots
df %>%
  ggplot() +
  geom_density(  # Density plot for t1
    aes(x = t1, fill = "t1"), 
    alpha = 0.7
  ) +
  geom_density(  # Density plot for t2
    aes(x = t2, fill = "t2"), 
    alpha = 0.7
  ) +
  scale_fill_manual(  # Colors for histograms and legend
    name = "Campaign",
    values = c(
      "t1" = "lightslategray", 
      "t2" = "mediumaquamarine"
    ),
    labels = c("Before", "After")
  ) +
  theme(
    legend.position = "bottom",       # Legend on bottom
    axis.title.x = element_blank()  # No title on X axis
  )

# Boxplots
df %>%
  ggplot() +
  geom_boxplot(aes( y = t1, x = 1), 
    fill = "lightslategray") +
  geom_boxplot(aes( y = t2, x = 2), 
    fill = "mediumaquamarine") +
  theme(axis.title.y = element_blank())

# Parallel coordinate plot
df %>%
  ggparcoord(
    splineFactor = T,  # Spline spread out categories
    alphaLines = 0.7   # Transparency
  )
        
# T-TESTS ##################################################

# Paired t-test; Time 2 scores needs to be listed first so
# that positive differences indicate improvements (i.e.,
# higher scores) from Time 1

df %$%          # Exposition pipe to make vectors available
  t.test(       # Generic t-test command
    t2,         # Minuend (number subtracted from) is first
    t1,         # Subtrahend (number subtracted) is second
    paired = T  # Used paired t-test
  )

# Paired t-test with options
df %$%
  t.test(
    t2,
    t1,
    paired = T,
    mu = 10,                  # Specify population mean
    alternative = "greater",  # Directional hypothesis
    conf.level = 0.80         # 80% confidence interval
  )

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
