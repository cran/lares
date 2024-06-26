% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_metrics.R
\name{loglossBinary}
\alias{loglossBinary}
\title{Logarithmic Loss Function for Binary Models}
\usage{
loglossBinary(tag, score, eps = 0.001)
}
\arguments{
\item{tag}{Vector. Real known label}

\item{score}{Vector. Predicted value or model's result}

\item{eps}{Numeric. Epsilon value}
}
\description{
This function calculates log loss/cross-entropy loss for binary
models. NOTE: when result is 0.69315, the classification is neutral;
it assigns equal probability to both classes.
}
\seealso{
Other Model metrics: 
\code{\link{ROC}()},
\code{\link{conf_mat}()},
\code{\link{errors}()},
\code{\link{gain_lift}()},
\code{\link{model_metrics}()}
}
\concept{Model metrics}
