## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.width = 7,
    fig.height = 5,
    warning = FALSE,
    message = FALSE,
    cache = TRUE
)
library(lares)
library(dplyr)
library(ggplot2)

## ----eval=TRUE----------------------------------------------------------------
# Define your input variables
paid_media_spends <- c("tv_spend", "digital_spend")
organic_vars <- c("newsletter")

# Generate hyperparameters
hyperparameters <- robyn_hypsbuilder(
    channels = c(paid_media_spends, organic_vars),
    media_type = "default",
    adstock = "weibull_pdf", # or "weibull_cdf", "weibull_pdf"
    date_type = "weekly",
    lagged = FALSE
)

# The output is a list ready to be passed to robyn_inputs()
print(hyperparameters)

## ----eval=FALSE---------------------------------------------------------------
# # Assuming you have a Robyn OutputCollect object
# # OutputCollect <- robyn_run(...)
# 
# # Select best models
# best_models <- robyn_modelselector(
#     InputCollect = InputCollect,
#     OutputCollect = OutputCollect,
#     metrics = c(
#         "rsq_train", "performance", "potential_improvement", "non_zeroes",
#         "incluster_models", "cluster_sd", "certainty", "baseline_dist"
#     ),
#     wt = c(2, 0.1, 0, 1, 0.1, 0, 1.5, 0)
# )
# 
# # View the results
# print(best_models)
# 
# # Plot the clusters
# plot(best_models)

## ----eval=FALSE---------------------------------------------------------------
# # Get performance for a specific model and date range
# performance <- robyn_performance(
#     InputCollect = InputCollect,
#     OutputCollect = OutputCollect,
#     select_model = "1_123_4",
#     # Calculate for specific date range
#     start_date = NULL, end_date = NULL,
#     non_promo = TRUE, # Add non-promo decomposition as well
#     marginals = TRUE, # Add mROAS or mCPA for spend levels
#     carryovers = FALSE # Add mean percentage of carryover
# )
# 
# # View performance table
# print(performance)
# 
# # Plot performance
# plot(performance)

## ----eval=FALSE---------------------------------------------------------------
# # Calculate marginal response
# marginal <- robyn_marginal(
#     InputCollect = InputCollect,
#     OutputCollect = OutputCollect,
#     select_model = "1_123_4",
#     metric_name = "tv_spend",
#     metric_value = 100000
# )

## ----eval=FALSE---------------------------------------------------------------
# # Load multiple models
# model_paths <- c("model_us.json", "model_uk.json", "model_de.json")
# models <- lapply(model_paths, Robyn::robyn_recreate)
# names(models) <- c("US", "UK", "DE")
# 
# # Allocate budget across models
# cross_allocation <- robyn_xmodels(
#     models = models,
#     total_budget = 5000000,
#     scenario = "max_response"
# )
# 
# # View results
# print(cross_allocation)

