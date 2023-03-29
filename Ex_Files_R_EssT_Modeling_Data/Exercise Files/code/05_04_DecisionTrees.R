# Title:    Classifying cases with decision tree analysis
# File:     05_04_DecisionTrees.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(caret, magrittr, pacman, rattle, rio, 
  tidyverse)
# caret: for decision trees
# magrittr: for pipes
# pacman: for loading/unloading packages
# rattle: for plotting decision trees
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

df <- import("data/b5_df.rds") %>%
  select(gender, Extrav:Open) %>%
  print()

# PARTITION DATA ###########################################

# Set random seed
set.seed(313)

# Split data into train and test sets
train <- df %>% sample_frac(.70)
test <- df %>% anti_join(train)

# EXPLORE TRAINING DATA ####################################

# Bar chart of "gender"
train %>%
  ggplot() + 
  geom_bar(aes(x = gender, fill = gender)) + 
  theme(legend.position = "none")

# Density plots of Big 5 variables 
train %>%
  gather(var, val, Extrav:Open) %>%
  ggplot(aes(val, group = gender, fill = gender)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~var) +
  theme(legend.position='bottom')

# MODEL TRAINING DATA ######################################

# Train decision tree on training data (takes a moment)
dt <- train(
  gender ~ .,        # Use all variables to predict gender
  data = train,      # Use training data
  method = "rpart",  # Recursive partitioning
  trControl = trainControl(method = "cv")  # Cross-validate
)

# Show processing summary
dt

# Description of final training model
dt$finalModel

# Plot final training model
dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Gender from Big 5 Factors",
    sub = "Training Data"
  )

# Predict training set
gender_p <- dt %>%  # "predicted"
  predict(newdata = train)

# Accuracy of model on training data
table(
  actualclass = train$gender, 
  predictedclass = gender_p
) %>%
confusionMatrix() %>%
print()

# VALIDATE ON TEST DATA ####################################

# Predict test set
gender_p <- dt %>%
  predict(newdata = test)

# Accuracy of model on test data
table(
  actualclass = test$gender, 
  predictedclass = gender_p
) %>%
confusionMatrix() %>%
print()

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
