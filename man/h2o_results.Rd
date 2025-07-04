% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_functions.R
\name{h2o_results}
\alias{h2o_results}
\title{Automated H2O's AutoML Results}
\usage{
h2o_results(
  h2o_object,
  test,
  train,
  y = "tag",
  which = 1,
  model_type,
  target = "auto",
  split = 0.7,
  ignore = NULL,
  quiet = FALSE,
  project = "ML Project",
  seed = 0,
  leaderboard = list(),
  plots = TRUE,
  ...
)
}
\arguments{
\item{h2o_object}{H2O Leaderboard (H2OFrame/H2OAutoML) or Model (h2o)}

\item{test, train}{Dataframe. Must have the same columns}

\item{y}{Variable or Character. Name of the dependent variable or response.}

\item{which}{Integer. Which model to select from leaderboard}

\item{model_type}{Character. Select "Classification" or "Regression"}

\item{target}{Value. Which is your target positive value? If
set to \code{'auto'}, the target with largest \code{mean(score)} will be
selected. Change the value to overwrite. Only used when binary
categorical model.}

\item{split}{Numeric. Value between 0 and 1 to split as train/test
datasets. Value is for training set. Set value to 1 to train with all
available data and test with same data (cross-validation will still be
used when training). If \code{train_test} is set, value will be overwritten
with its real split rate.}

\item{ignore}{Character vector. Columns too ignore}

\item{quiet}{Boolean. Keep quiet? If not, informative messages will be shown.}

\item{project}{Character. Your project's name}

\item{seed}{Integer. Set a seed for reproducibility. AutoML can only
guarantee reproducibility if max_models is used because max_time is
resource limited.}

\item{leaderboard}{H2O's Leaderboard. Passed when using
\code{h2o_selectmodel} as it contains plain model and no leader board.}

\item{plots}{Boolean. Create plots objects?}

\item{...}{Additional parameters on \code{h2o::h2o.automl}}
}
\value{
List. Trained model, predicted scores and datasets used, performance
metrics, parameters, importance data.frame, seed, and plots when \code{plots=TRUE}.
}
\description{
This is an auxiliary function to calculate predictions and results
when using the \code{h2o_automl()} function.
}
