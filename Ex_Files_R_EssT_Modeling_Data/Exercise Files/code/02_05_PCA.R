# Title:    Conducting a principal component analysis
# File:     02_05_PCA.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(GPArotation, magrittr, pacman, psych, rio, 
  tidyverse)
# GPArotation: Gradient Projection Algorithm Rotation for FA
# magrittr: for pipes
# pacman: for loading/unloading packages
# psych: for psychometric functions
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Import data from CSV, save as "df"
df <- import("data/b5.csv")

# Get column names
df %>% colnames()  

# PRINCIPAL COMPONENT ANALYSIS #############################

# Three methods in R
?prcomp     # Most common method
?princomp   # Slightly different method
?principal  # Method from psych package (my favorite)

# Principal components model using default method
pc <- df %>%
  prcomp(
    center = TRUE,  # Centers means to 0 (optional)
    scale = TRUE    # Sets unit variance (helpful)
  )

# Get summary stats
summary(pc)

# Screeplot of eigenvalues
plot(pc)

# VERY SIMPLE STRUCTURE ####################################

# Use "Very Simple Structure" to suggest number of factors
# Note: MAP = Minimum Absolute Partial correlation;
# n is the proposed maximum number of factors
df %>% 
  select(1:50) %>%
  vss(n = 10)

# Or use "nfactors" to do the same; includes VSS
df %>% 
  select(1:50) %>%
  nfactors(n = 10)

# FACTOR ANALYSIS ##########################################

# Factor analysis using minimum residual (minres) method and
# oblimin rotation, which is useful for simple structure.
# Need to enter desired number of factors (from VSS or
# nfactors above).

# Calculate and plot factors with fa()
df %>% 
  select(1:50) %>%      # Select variables
  fa(                   # Use fa() function
    nfactors = 5,       # Use five factors
    rotate = "oblimin"  # Oblimin oblique rotation
  ) %T>%                # T-pipe
  fa.diagram() %>%      # Diagram of factors and variables
  print()               # Print results

# HIERARCHICAL CLUSTERING ##################################

# Hierarchical clustering of items with iclust()
df %>% 
  select(1:50) %>% 
  iclust()

# PC WITH K FACTORS ########################################

# First PCA with no rotation, specify 5 factors
df %>% principal(nfactors = 5)

# Second PCA with oblimin (oblique) rotation
df %>% 
  principal(
    nfactors = 5, 
    rotate = "oblimin"
  ) %>% 
  plot()  # Plot position of variables on components

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
