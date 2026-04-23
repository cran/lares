## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE,
  eval = TRUE, # Show actual results like old vignette
  cache = FALSE, # Disabled for CRAN to avoid massive tarball sizes
  cache.lazy = FALSE
)

## ----eval=TRUE----------------------------------------------------------------
library(lares)
library(dplyr)

## -----------------------------------------------------------------------------
# Install h2o (run once)
# install.packages("h2o")
library(h2o)

# Initialize h2o quietly for vignette
Sys.unsetenv("http_proxy")
Sys.unsetenv("https_proxy")
h2o.init(nthreads = -1, max_mem_size = "2G", ip = "127.0.0.1")
h2o.no_progress() # Disable progress bars

## -----------------------------------------------------------------------------
data(dft)

# Train an AutoML model
# Binary classification
model <- h2o_automl(
  df = dft,
  y = "Survived",
  target = "TRUE",
  ignore = c("Ticket", "Cabin", "PassengerId"),
  max_models = 10,
  max_time = 120,
  impute = FALSE
)

# View results
print(model)

## -----------------------------------------------------------------------------
names(model)

## -----------------------------------------------------------------------------
# All metrics
model$metrics

# Specific metrics
model$metrics$AUC
model$metrics$Accuracy
model$metrics$Logloss

## -----------------------------------------------------------------------------
# Confusion matrix plot
mplot_conf(
  tag = model$scores_test$tag,
  score = model$scores_test$score,
  subtitle = sprintf("AUC: %.3f", model$metrics$metrics$AUC)
)

## -----------------------------------------------------------------------------
# ROC curve
mplot_roc(
  tag = model$scores_test$tag,
  score = model$scores_test$score
)

## -----------------------------------------------------------------------------
# Gain and Lift charts for binary classification
mplot_gain(
  tag = model$scores_test$tag,
  score = model$scores_test$score
)

## -----------------------------------------------------------------------------
# Variable importance dataframe
head(model$importance, 15)

# Plot top 15 important variables
top15 <- head(model$importance, 15)
mplot_importance(
  var = top15$variable,
  imp = top15$importance
)

## ----eval=FALSE---------------------------------------------------------------
# # Calculate SHAP values (computationally expensive)
# shap <- h2o_shap(model)
# 
# # Plot SHAP summary
# plot(shap)

## -----------------------------------------------------------------------------
model <- h2o_automl(
  df = dft,
  y = "Survived",
  # Ignore specific columns
  ignore = c("Ticket", "Cabin", "PassengerId"),
  # Use only specific algorithms (exclude_algos also available)
  include_algos = c("GBM", "DRF"), # Gradient Boosting & Random Forest
  # Data split
  split = 0.7,
  # Handle imbalanced data
  balance = TRUE,
  # Remove outliers (Z-score > 3)
  no_outliers = TRUE,
  # Impute missing values (requires mice package if TRUE)
  impute = FALSE,
  # Keep only unique training rows
  unique_train = TRUE,
  # Reproducible results
  seed = 123
)

## -----------------------------------------------------------------------------
model_multiclass <- h2o_automl(
  df = dft,
  y = "Pclass",
  ignore = c("Cabin", "PassengerId"),
  max_models = 10,
  max_time = 60
)

# Multi-class metrics
model_multiclass$metrics

# Confusion matrix for multi-class
mplot_conf(
  tag = model_multiclass$scores_test$tag,
  score = model_multiclass$scores_test$score
)

## -----------------------------------------------------------------------------
model_regression <- h2o_automl(
  df = dft,
  y = "Fare",
  ignore = c("Cabin", "PassengerId"),
  max_models = 10,
  exclude_algos = NULL
)

# Regression metrics
model_regression$metrics

## -----------------------------------------------------------------------------
# Create splits
splits <- msplit(dft, size = 0.8, seed = 123)
splits$train$split <- "train"
splits$test$split <- "test"

# Combine
df_split <- rbind(splits$train, splits$test)

# Train using split column
model <- h2o_automl(
  df = df_split,
  y = "Survived",
  train_test = "split",
  max_models = 5
)

## -----------------------------------------------------------------------------
# New data (same structure as training)
new_data <- dft[1:10, ]

# Predict
predictions <- h2o_predict_model(new_data, model$model)
head(predictions)

## -----------------------------------------------------------------------------
# Get probabilities
predictions <- h2o_predict_model(new_data, model$model)
head(predictions)

## -----------------------------------------------------------------------------
# Complete model evaluation plots
mplot_full(
  tag = model$scores_test$tag,
  score = model$scores_test$score,
  subtitle = model$model@algorithm
)

## -----------------------------------------------------------------------------
# Model performance over trees
mplot_metrics(model)

## -----------------------------------------------------------------------------
# Save model and plots
export_results(model, subdir = "models", thresh = 0.5)

## ----eval=FALSE---------------------------------------------------------------
# # Load model
# loaded_model <- readRDS("models/Titanic_Model/Titanic_Model.rds")
# 
# # Make predictions with MOJO (production-ready)
# predictions <- h2o_predict_MOJO(
#   model_path = "models/Titanic_Model",
#   df = dft[1:10, ]
# )

## -----------------------------------------------------------------------------
# Quick prototype
model <- h2o_automl(dft, "Survived", max_models = 3, max_time = 30)

## -----------------------------------------------------------------------------
# Refine based on results
model <- h2o_automl(
  dft, "Survived",
  max_models = 20,
  no_outliers = TRUE,
  balance = TRUE,
  ignore = c("PassengerId", "Name", "Ticket", "Cabin"),
  model_name = "Titanic_Model"
)

## -----------------------------------------------------------------------------
# Check multiple metrics
model$metrics

# Visual inspection
mplot_full(
  tag = model$scores_test$tag,
  score = model$scores_test$score
)

# Variable importance
mplot_importance(
  var = model$importance$variable,
  imp = model$importance$importance
)

## -----------------------------------------------------------------------------
# Density plot
mplot_density(
  tag = model$scores_test$tag,
  score = model$scores_test$score
)

## -----------------------------------------------------------------------------
# Save everything
export_results(model, subdir = "my_project", thresh = 0.5)

## ----eval=FALSE---------------------------------------------------------------
# # Manually initialize h2o with more memory
# h2o::h2o.init(max_mem_size = "8G", nthreads = -1)

## -----------------------------------------------------------------------------
# Remove all models
h2o::h2o.removeAll()

# Shutdown h2o
h2o::h2o.shutdown(prompt = FALSE)

## -----------------------------------------------------------------------------
# Open h2o's web interface
# Navigate to: http://localhost:54321/flow/index.html

