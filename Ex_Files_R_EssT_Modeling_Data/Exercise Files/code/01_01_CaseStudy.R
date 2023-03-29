# Title:    Open Practice
# File:     05_03_kNN.R
# Project:  R_EssT_2; R Essential Training, Part 2:
#           Modeling Data

# INSTALL AND LOAD PACKAGES ################################

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(
  caret,         # Predictive analytics
  GGally,        # Scatterplot matrix
  magrittr,      # Pipes
  pacman,        # Load/unload packages
  parallel,      # Parallel processing
  randomForest,  # Random forests (obviously)
  rattle,        # Plot decision trees
  rio,           # Import/export data
  tictoc,        # Time operations
  tidyverse      # So many reasons
  )

# LOAD AND PREPARE DATA ####################################

# Import Big 5 data
df <- import("data/b5_df.rds") %>%
  print()

# Boxplot and median for "Open"
df %>%
  pull(Open) %T>%  # use pull() for vector; T-pipe
  boxplot() %>%
  median()

# Dichotomize "Open" for outcome
df %<>%                         # Overwrite data
  mutate(
    open_t = ifelse(            # open_t ("text")
      Open >= 4,                # Test
      "High",                   # Value if true
      "Low"                     # Value if false
    ),
    open_t = as_factor(open_t)  # Convert to factor
  ) %>%
  print()

# EXPLORE DATA #############################################

# Bar chart of "open_t"
df %>%
  ggplot() + 
  geom_bar(
    aes(
      x    = open_t,  # Variable to chart
      fill = open_t   # Color bars by variable
    )
  ) + 
  theme(legend.position = "none")

# Scatterplot matrix of all variables (takes a moment)
tic("Scatterplot matrix")
df %>% 
  select(          # Reorder variables
    open_t,        # Put dichotomous "open" first
    Open,          # Then quantitative "open"
    Extrav:Consc,  # Then other Big 5
    age:engnat     # Then demographics
  ) %>% 
  ggpairs()
toc()  # 15.404 sec to calculate, about 10 sec to appear

# Summary statistics
df %>% summary()

# T-TEST ON FULL DATA ######################################

# Just an example
df %>% t.test(Extrav ~ open_t, data = .)
# Not a good idea to do several t-tests

# LINEAR REGRESSION ON FULL DATA ###########################

# Simultaneous entry linear regression
fit_lm <- df %>%   # Use full data, save as "fit_lm"
  select(          # Put outcome first
    Open,          # Then quantitative "open"
    Extrav:Consc,  # Then other Big 5
    age:engnat     # Then demographics
  ) %>%
  lm()             # Default linear regression

# Show model summary
fit_lm %>% summary()

# Diagnostic plots; run command then hit return in Console
# 1: Residuals vs. Fitted
# 2: Normal Q-Q
# 3: Scale-Location
# 4: Residuals vs. Leverage
fit_lm %>% plot()

# SPLIT DATA FOR MACHINE LEARNING ##########################

# Set random seed
set.seed(333)

# Take random subsample to save time (total n = 18,837)
# df %<>% sample_n(1000)

# Split data into train and test sets
train <- df %>% sample_frac(.70)  # Sample 70% of data
test  <- anti_join(df, train)     # Use remaining cases

# KNN ON TRAINING DATA #####################################

# Define parameters
statctrl <- trainControl(
  method = "repeatedcv",  # Repeated cross-validation
  number = 10,            # Number of folds
  repeats = 3             # Number of complete sets of folds
) 

# Define and save model
fit_knn <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,          # Use training data
  method = "knn", 
  trControl = statctrl,
  tuneLength = 20,       # 20 dif values for k
  na.action = "na.omit"
)

# Apply model to training data (takes a moment)
tic("k-NN")
fit_knn
toc() # 0.04 sec for n = 700; 0.01 for full data (13k)??

# Predict training set
open_p <- fit_knn %>%  # "predicted"
  predict(newdata = train)

# Accuracy of model on training data
table(
  actualclass = train$open_t, 
  predictedclass = open_p
) %>%
confusionMatrix() %>%
print()  # .6180 accuracy on full training data

# KNN ON TEST DATA #########################################

# Predict test set
open_p <- predict(  # Create new variable ("predicted")
  fit_knn,          # Apply saved model
  newdata = test    # Use test data
)

# Accuracy of model on test data
table(
  actualclass = test$open_t,  # True outcome
  predictedclass = open_p     # Predicted outcome
) %>%
confusionMatrix() %>%         # Accuracy statistics
print()                       # .5772 accuracy on full data

# DECISION TREE ON TRAINING DATA ###########################

# Train decision tree on training data (takes a moment)
tic("Decision tree")
fit_dt <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,      # Use training data
  method = "rpart",  # Recursive partitioning
  trControl = trainControl(method = "cv")  # Cross-validate
)
toc()  # 1.1 sec for n = 700; 1.8 sec for full data

# Show processing summary
fit_dt

# Description of final training model
fit_dt$finalModel

# Plot final training model
fit_dt$finalModel %>%
  fancyRpartPlot(
    main = "Predicting Open",
    sub = "Training Data"
  )

# Predict training set
open_p <- fit_dt %>%  # "predicted"
  predict(newdata = train)

# Accuracy of model on training data
table(
  actualclass = train$open_t, 
  predictedclass = open_p
) %>%
confusionMatrix() %>%
print()  # .5802 accuracy on full training data

# DECISION TREE ON TEST DATA ###############################

# Predict test set
open_p <- fit_dt %>%
  predict(newdata = test)

# Accuracy of model on test data
table(
  actualclass = test$open_t, 
  predictedclass = open_p
) %>%
confusionMatrix() %>%
print()  # 0.5776 on full test data

# RANDOM FOREST ON TRAINING DATA ###########################

# Define parameters for the random forest
control <- trainControl(
  method  = "repeatedcv",  # Repeated cross-validation
  number  = 10,            # Number of folds
  repeats = 3,             # Number of sets of folds
  search  = "random",      # Max number of tuning parameters
  allowParallel = TRUE     # Allow parallel processing
)

# Train random forest on training data (can take a while)
tic("Random forest")
fit_rf <- train(
  open_t ~ age + gender + Extrav + Neurot + Agree + Consc,
  data = train,         # Use training data
  method = "rf",        # Use random forests
  metric = "Accuracy",  # Use accuracy as criterion
  tuneLength = 15,      # Number of levels for parameters
  ntree = 300,          # Number of trees
  trControl = control   # Link to parameters
)
toc()  # 32 sec when n = 700; 635 sec for full data

# Show processing summary
fit_rf

# Plot accuracy by number of predictors
fit_rf %>% plot()

# Accuracy of model with training data
fit_rf$finalModel

# Plot error by number of trees; Red is error for "High,"
# green is error for "Low," and black is error or "OOB,"
# or "out of bag" (i.e., the probability that any given 
# prediction is not correct within the test data, or the 
# overall accuracy)
fit_rf$finalModel %>% plot()

# Predict training data
open_p <- fit_rf %>%        # "predicted"
  predict(newdata = train)  # Use train data

# Accuracy of model on test data
table(
  actualclass = train$open_t, 
  predictedclass = open_p
) %>%
confusionMatrix() %>%
print()  # 0.7145 accuracy on full training data

# RANDOM FOREST ON TEST DATA ###############################

# Predict test set
open_p <- fit_rf %>%       # "predicted"
  predict(newdata = test)  # Use test data

# Accuracy of model on test data
table(
  actualclass = test$open_t, 
  predictedclass = open_p
) %>%
confusionMatrix() %>%
print()  # 0.5912 accuracy on full test data

# SUMMARIZING PREDICTIONS ##################################

# TOTAL n = 18,837
# TRAIN n = 13,186
# TEST  n =  5,651

# How many cases in "high"?
test %>% pull(open_t) %>% summary()
# 2870 / 5651 = .5078 (about 51%)

#      TRAIN  TEST
# KNN    62%   58%
# DT     58%   58%
# RF     71%   59%

# CLEAN UP #################################################

# Clear data
rm(list = ls())  # Removes all objects from environment

# Clear packages
p_unload(all)  # Remove all contributed packages

# Clear plots
graphics.off()  # Clears plots, closes all graphics devices

# Clear console
cat("\014")  # Mimics ctrl+L

# Clear R
#   You may want to use Session > Restart R, as well, which 
#   resets changed options, relative paths, dependencies, 
#   and so on to let you start with a clean slate

# Clear mind :)
