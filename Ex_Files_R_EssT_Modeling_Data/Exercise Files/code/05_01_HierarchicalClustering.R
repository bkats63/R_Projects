# Title:    Grouping cases with hierarchical clustering
# File:     05_01_HierarchicalClustering.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(cluster, factoextra, magrittr, pacman, rio, 
  tidyverse)
# cluster:  for cluster analysis
# factoextra: for evaluating clusters
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(
    state_code, 
    instagram:modernDance
  ) %>% 
  print()

# HIERARCHICAL CLUSTERING ##################################

# Calculate clusters
hc <- df %>%  # Get data
  dist %>%    # Compute distance/dissimilarity matrix
  hclust      # Compute hierarchical clusters

# Plot dendrogram
hc %>% plot(               # Generic X-Y plotting
  labels = df$state_code,  # Put state names on chart
  hang = -1,               # Line up names at bottom
  cex = 0.6                # Make font smaller
)

# OPTIMAL NUMBER OF CLUSTERS ###############################

# Elbow method
df %>%
  fviz_nbclust(          # From factoextra
    FUN = hcut, 
    method = "wss"       # "within cluster sums of squares"
  ) +
  geom_vline(            # Reference line
    xintercept = 5, 
    color = "red", 
    linetype = "dotted"
  )                      # Look for "bend" in curve

# Silhouette method
df %>%
  fviz_nbclust(
    FUN = hcut,
    method = "silhouette"  # Look for maximum width
)

# Gap method
# This compares the total intracluster variation for  
# values of k with their expected values from null  
# distributions (from Monte Carlo simulations)
df %>%
  select(-state_code) %>%  # Remove non-numeric variables
  clusGap(
    FUN = hcut, 
    nstart = 50,
    K.max = 10, 
    B = 100
  ) %>%
  fviz_gap_stat()          # Look for highest value

# HIERARCHICAL CLUSTERING ##################################

# Plot dendrogram (again) 
hc %>% plot(               # Generic X-Y plotting
  labels = df$state_code,  # Put state names on chart
  hang = -1,               # Line up names at bottom
  cex = 0.6                # Make font smaller
)

# Draw give boxes around clusters
hc %>% rect.hclust(  # Add rectangles to clusters
  k = 5,             # Draw five rectangles
  border = 2:6       # Use colors 2 through 6
)

# Fit the hierarchical clustering groups to data
y_hc = cutree(hc, 5)

# Visualize the give clusters in 2D plot
fviz_cluster(
  list(
    data = df %>%
    set_rownames(df$state_code) %>%
    select(-state_code),
    cluster = y_hc
  )
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
