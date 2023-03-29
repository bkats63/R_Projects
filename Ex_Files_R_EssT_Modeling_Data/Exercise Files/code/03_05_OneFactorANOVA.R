# Title:    Comparing multiple means: 
#           One-factor analysis of variance
# File:     03_05_OneFactorANOVA.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(magrittr, pacman, psych, rio, tidyverse)
# magrittr: for pipes
# pacman: for loading/unloading packages
# psych: for descriptive statistics
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Save data to "df" (for "data frame")

df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(
    state_code, 
    psychRegions,
    instagram:modernDance
  ) %>% 
  mutate(
    psychRegions = as.factor(psychRegions),
    psychRegions = fct_recode(psychRegions,
      "Friendly" = "Friendly and Conventional",
      "Relaxed" = "Relaxed and Creative",
      "Temperamental" = "Temperamental and Uninhibited"
    )
  ) %>%
  print()

# EXPLORE DATA #############################################

# Bar chart of group frequencies
df %>%
  ggplot() + 
  geom_bar(
    aes(
      x    = psychRegions,  # Variable to chart
      fill = psychRegions   # Color bars by variable
    )
  ) + 
  theme(legend.position = "none")

# Boxplots
df %>%
  ggplot(
    aes(
      x    = psychRegions,  # Grouping variable
      y    = volunteering,  # Outcome variable
      fill = psychRegions   # Color variable
    )
  ) + 
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  theme(legend.position = "none")

# Density plots
df %>%
  ggplot(
    aes(
      x    = volunteering,  # Outcome variable
      fill = psychRegions   # Color/grouping variable
    )
  ) + 
  geom_density(alpha = 0.5) +
  theme(legend.position = "bottom")

# Descriptive statistics by group
df %$%             # Exposition pipe
  describeBy(      # describeBy function from psych
    volunteering,  # Outcome variable
    psychRegions   # Grouping variable
  )

# ONE-WAY ANOVA ############################################

# Conduct one-way ANOVA
fit <- df %>%
  aov(
    volunteering ~ psychRegions,  # "as a function of"
    data = .
  ) 

# Show ANOVA table
fit %>% summary()

# POST-HOC TESTS ##########################################

# TukeyHSD has its own function
fit %>% TukeyHSD()

# Other post-hoc tests use the pairwise.t.test function; but
# you need to use source$variable notation instead of
# tidyverse style.
pairwise.t.test(
  df$volunteering,  # Outcome variable
  df$psychRegions,  # Grouping variable
  p.adj = "bonf"    # Adjustment method
)

# Information on methods available for post-hoc tests
?p.adjust

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
