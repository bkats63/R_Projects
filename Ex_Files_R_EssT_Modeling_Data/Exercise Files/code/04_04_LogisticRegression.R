# Title:    Predicting outcomes with logistic regression
# File:     04_04_LogisticRegression.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(broom, magrittr, pacman, rio, skimr, 
  tidyverse)
# broom: for tidying tables
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# skimr: for descriptives
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

df <- import("data/b5_df.rds") %>%
  print()

# Bar chart of "gender"
df %>%
  ggplot() + 
  geom_bar(
    aes(
      x    = gender,  # Variable to chart
      fill = gender   # Color bars by variable
    )
  ) + 
  theme(legend.position = "none")

# Logistic regression needs a 0/1 outcome variable, so
# we'll create a variable "female" where "Female" gets a 1
# (for "True") and "Male" gets a 0 (for "False")

df %<>%
  mutate(
    female = ifelse(       # Create new variable "female"
      gender == "Female",  # Test
      1,                   # Value if true
      0                    # Value if false
    )
  ) %>%
  select(gender, female, Extrav:Open) %>%
  print()

# Bar chart of "female"
df %>%
  ggplot() + 
  geom_bar(aes(x = female)) + 
  theme(legend.position = "none")

# EXPLORE DATA #############################################

# Explore data (wide output)
df %>% skim()

# BINOMIAL LOGISTIC REGRESSION #############################

# Compute model
fit <- glm(
  female ~ Extrav + Neurot + Agree + Consc + Open,
  data = df, 
  family = "binomial"
  )

# Summarize regression model
fit %>% summary()  # Standard output
fit %>% tidy()     # Tidy output

# Confidence intervals for coefficients
fit %>% confint() 

# PREDICTED VALUES #########################################

# Predicted propobability are in "fitted values"
fit %>% view()

# See the first few predicted values
predict(fit, type = 'response') %>% head()

# Add predicted values to df
df %<>%
  mutate(
    predicted = predict(fit, type = 'response'),
    pred_gender = ifelse(  # Create variable "pred_gender"
      predicted > 0.5,     # Test if predicted values > 0.5
      "P_Female",          # If true, predict female
      "P_Male"             # If false, predict male
    )
  ) %>%
  select(
    gender,
    female,
    predicted,
    pred_gender,
    everything()
  ) %>%
  print()
  
# VISUALIZE PROBABILITIES ##################################

# Boxplots of probabilities by actual values
df %>%
  ggplot(aes(x = gender, 
    y = predicted, 
    fill = gender)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("") +
  theme(legend.position = "none")

# Confusion matrix
df %$%                        # Exposition pipe
  table(gender, pred_gender)  # table(rows, columns)

# Side-by-side bar chart for confusion matrix
df %>%
  ggplot(aes(gender, fill = pred_gender)) + 
  geom_bar(position = position_dodge()) +
  theme(legend.position = "bottom")

# Row percentages for confusion matrix
df %$%
  table(gender, pred_gender) %>%
  prop.table(1) %>%  # 1 is for row percentages
  round(2) %>%       # Round to two decimal places
  `*`(100)           # Multiply by 100 to read as percent

# Plot logistic curve
df %>%
  ggplot(
    aes(
      x = predicted, 
      y = female
    )
  ) + 
  geom_point(alpha = .01) +
  stat_smooth(
    method = "glm", 
    method.args = list(family = binomial)
  ) + 
  xlab("Probability Female") +
  ylab("Respondent Is Female") 

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
