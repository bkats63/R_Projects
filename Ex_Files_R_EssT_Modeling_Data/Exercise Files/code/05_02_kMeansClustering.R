# Title:    Grouping cases with k-means clustering
# File:     05_02_kMeansClustering.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(cluster, datasets, factoextra, magrittr,  
  pacman, rio, tidyverse)
# cluster:  for cluster analysis
# datasets: for sample data
# factoextra: for evaluating clusters
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Load data
?mtcars

# Select variables and save data
df <- mtcars %>%
  select(mpg, cyl, disp, hp, wt, qsec, gear, carb) %>%
  print()

# Scale data for clustering
df_z <- df %>%
  scale() %>%
  print()

# OPTIMAL NUMBER OF CLUSTERS ###############################

# Elbow method
df_z %>%
  fviz_nbclust(          # From factoextra
    FUN = kmeans,        # Use k-means
    method = "wss"       # "within cluster sums of squares"
  ) +
  geom_vline(            # Reference line
    xintercept = 2, 
    color = "red", 
    linetype = "dotted"
  )                      # Look for "bend" in curve

# Silhouette method
df_z %>%
  fviz_nbclust(
    FUN = kmeans,          # Use k-means
    method = "silhouette"  # Look for maximum width
) +
geom_vline(                # Reference line
  xintercept = 2, 
  color = "red", 
  linetype = "dotted"
  )  

# K-MEANS CLUSTERING #######################################

# Compute two clusters
km <- df_z %>% 
  kmeans(2) %>%
  print()

# Graph two clusters
df_z %>%
  clusplot(
    km$cluster,     # cluster data
    color  = TRUE,  # color
    lines  = 0,     # No lines connecting centroids
    labels = 2      # Labels clusters and cases
  )

# ADD CLUSTERS TO DF #######################################

df %<>%                          # Overwrite data
  rownames_to_column("car") %>%  # Create var with car names
  mutate(
    cluster = km$cluster         # Create cluster variable
  ) %>%
  select(                        # Reorder variables
    car,
    cluster,
    everything()
  ) %>%
  arrange(                       # Sort data
    cluster,                     # First by cluster
    car                          # Then by name of car
  ) %>%
  print()

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
