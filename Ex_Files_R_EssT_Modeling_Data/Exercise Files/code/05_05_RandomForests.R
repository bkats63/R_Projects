# Title:    Creating ensemble models with random forest 
#           classification
# File:     05_05_RandomForests.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(caret, magrittr, pacman, parallel, 
  randomForest, rio, tidyverse)
# caret: for decision trees
# magrittr: for pipes
# pacman: for loading/unloading packages
# parallel: for parallel processing
# randomForest: for random forests (obviously)
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Set random seed
set.seed(313)

# Import data, select random sample
df <- import("data/b5_df.rds") %>%
  select(gender, Extrav:Open) %>%
  sample_n(500) %>%                 # Sample 500 cases
  print()

# SPLIT DATA ###############################################

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
  theme(legend.position = 'bottom')

# MODEL TRAINING DATA ######################################

# Define parameters for the train function
control <- trainControl(
  method  = "repeatedcv",  # Repeated cross-validation
  number  = 10,            # Number of folds
  repeats = 3,             # Number of sets of folds
  search  = "random",      # Max number of tuning parameters
  allowParallel = TRUE     # Allow parallel processing
)

# Train decision tree on training data (can take a while)
rf <- train(
  gender ~ . ,          # Predict gender from all other vars
  data = train,         # Use training data
  method = "rf",        # Use random forests
  metric = "Accuracy",  # Use accuracy as criterion
  tuneLength = 15,      # Number of levels for parameters
  ntree = 800,          # Number of trees
  trControl = control   # Link to parameters
)

# Show processing summary
rf

# Plot accuracy by number of predictors
rf %>% plot()

# Accuracy of model with training data
rf$finalModel

# Plot error by number of trees; Red is error for "Male,"
# green is error for "Female," and black is error or "OOB,"
# or "out of bag" (i.e., the probability that any given 
# prediction is not correct within the test data, or the 
# overall accuracy)
rf$finalModel %>% plot()

# APPLY MODEL TO TEST DATA #################################

# Predict test set
gender_p <- rf %>%         # "predicted"
  predict(newdata = test)  # Use test data

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
