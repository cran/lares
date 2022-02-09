## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, comment = "#>", results = "hide",
  fig.width = 7.1, fig.height = 5.5
)
h2o::h2o.init() # Just to be sure

## ----load, eval=TRUE----------------------------------------------------------
# install.packages("lares")
library(lares)

# The data we'll use is the Titanic dataset
data(dft)
df <- subset(dft, select = -c(Ticket, PassengerId, Cabin))

## ----class_2, results="show"--------------------------------------------------
r <- h2o_automl(df, y = Survived, max_models = 1, impute = FALSE, target = "TRUE")

## ----class_2_print, results="show"--------------------------------------------
plot(r)

## ----class_2_metrics, results="show"------------------------------------------
r$metrics

## ----class_2_metrics_plots, results="show"------------------------------------
r$plots$metrics

## ----class_2_importance, results="show"---------------------------------------
head(r$importance)

r$plots$importance

## ----class_3, results="show"--------------------------------------------------
r <- h2o_automl(df, Pclass, ignore = c("Fare", "Cabin"), max_time = 30, plots = FALSE)

## ----class_3_results, results="show"------------------------------------------
plot(r)

## ----regression, results="show"-----------------------------------------------
r <- h2o_automl(df, y = "Fare", ignore = "Pclass", exclude_algos = NULL, quiet = TRUE)
print(r)

## ----regression_plots, results="show"-----------------------------------------
plot(r)

## ----h2o_shutdown, echo=FALSE, include=FALSE, error=FALSE, message=FALSE------
h2o::h2o.shutdown(prompt = FALSE)
Sys.sleep(2)

