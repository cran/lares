% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{quants}
\alias{quants}
\title{Calculate cuts by quantiles}
\usage{
quants(values, splits = 10, return = "labels", n = 2)
}
\arguments{
\item{values}{Vector. Values to calculate quantile cuts}

\item{splits}{Integer. How many cuts should split the values?}

\item{return}{Character. Return "summary" or "labels"}

\item{n}{Integer. Determines the number of digits used in
formatting the break numbers.}
}
\value{
Factor vector or data.frame. Depending on \code{return} input:
\itemize{
  \item \code{labels} a factor ordered vector with each observation's quantile
  \item \code{summary} a data.frame with information on each quantile cut
}
}
\description{
This function lets the user quickly calculate cuts for quantiles
and discretize numerical values into categorical values.
}
\examples{
data(dft) # Titanic dataset
quants(dft$Age, splits = 5, "summary")
quants(dft$Age, splits = 5, "labels")[1:10]
}
\seealso{
Other Data Wrangling: 
\code{\link{balance_data}()},
\code{\link{categ_reducer}()},
\code{\link{cleanText}()},
\code{\link{date_cuts}()},
\code{\link{date_feats}()},
\code{\link{file_name}()},
\code{\link{formatHTML}()},
\code{\link{holidays}()},
\code{\link{impute}()},
\code{\link{left}()},
\code{\link{normalize}()},
\code{\link{num_abbr}()},
\code{\link{ohe_commas}()},
\code{\link{ohse}()},
\code{\link{removenacols}()},
\code{\link{replaceall}()},
\code{\link{replacefactor}()},
\code{\link{textFeats}()},
\code{\link{textTokenizer}()},
\code{\link{vector2text}()},
\code{\link{year_month}()},
\code{\link{zerovar}()}

Other Calculus: 
\code{\link{corr}()},
\code{\link{dist2d}()},
\code{\link{model_metrics}()}
}
\concept{Calculus}
\concept{Data Wrangling}
