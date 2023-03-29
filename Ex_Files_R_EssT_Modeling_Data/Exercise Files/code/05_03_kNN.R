# Title:    Classifying cases with k-nearest neighbors
# File:     05_03_kNN.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(caret, magrittr, pacman, rio, tidyverse)
# caret: For k_NN algorithm
# magrittr: for pipes
# pacman: for loading/unloading packages
# rio: for importing data
# tidyverse: for so many reasons

# LOAD AND PREPARE DATA ####################################

# Set random seed
set.seed(333)

# Import Big 5 data
df <- import("data/b5_df.rds") %>%
  print()

# Boxplot and media for "Open"
df %>%
  pull(Open) %T>%  # use pull() for vector; T-pipe
  boxplot() %>%
  median()

# Dichotomize "Open" for outcome
df %<>%               # Overwrite data
  mutate(
    open_t = ifelse(  # open_t ("text")
      Open >= 4,      # Test
      "Yes",          # Value if true
      "No"            # Value if false
    )
  ) %>%
  print()

# Take random subsample to save time
df %<>% sample_n(4000)

# SPLIT DATA ###############################################

# Split data into train and test sets
train <- df %>% sample_frac(.70)
test <- anti_join(df, train)

# EXPLORE TRAINING DATA ####################################

# Bar chart of "open_t"
train %>%
  ggplot() + 
  geom_bar(
    aes(
      x    = open_t,  # Variable to chart
      fill = open_t   # Color bars by variable
    )
  ) + 
  theme(legend.position = "none")

# COMPUTE KNN MODEL ON TRAINING DATA #######################

# Define parameters
statctrl <- trainControl(
  method = "repeatedcv",  # Repeated cross-validation
  number = 10,            # Number of folds
  repeats = 3             # Number of complete sets of folds
) 

# Define and save model
fit <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,          # Use training data
  method = "knn", 
  trControl = statctrl,
  tuneLength = 20,       # 20 dif values for k
  na.action = "na.omit"
)

# Apply model to training data (takes a moment)
fit

# APPLY MODEL TO TEST DATA #################################

# Predict test set
open_p <- predict(  # Create new variable ("predicted")
  fit,              # Apply saved model
  newdata = test    # Use test data
)

# Accuracy of model on test data
table(
  actualclass = test$open_t,  # True outcome
  predictedclass = open_p     # Predicted outcome
) %>%
confusionMatrix() %>%         # Accuracy statistics
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
