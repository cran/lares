% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequencies.R
\name{freqs_plot}
\alias{freqs_plot}
\title{Combined Frequencies Plot for Categorical Features}
\usage{
freqs_plot(
  df,
  ...,
  top = 10,
  rm.na = FALSE,
  abc = FALSE,
  title = NA,
  subtitle = NA,
  quiet = FALSE
)
}
\arguments{
\item{df}{Data.frame}

\item{...}{Variables. Variables you wish to process. Order matters.
If no variables are passed, the whole data.frame will be considered}

\item{top}{Integer. Filter and plot the most n frequent for
categorical values. Set to NA to return all values}

\item{rm.na}{Boolean. Remove NA values in the plot? (not filtered for
numerical output; use na.omit() or filter() if needed)}

\item{abc}{Boolean. Do you wish to sort by alphabetical order?}

\item{title}{Character. Overwrite plot's title with.}

\item{subtitle}{Character. Overwrite plot's subtitle with.}

\item{quiet}{Boolean. Keep quiet? If not, informative messages will be shown.}
}
\value{
Plot. Result of the frequency of combined variables.
}
\description{
Plot frequencies of multiple categories within a data.frame in
a new fancy way. Tidyverse friendly, based on \code{lares::freqs()},
no limits on amount of features to evaluate.
}
\examples{
Sys.unsetenv("LARES_FONT") # Temporal
data(dft) # Titanic dataset

x <- freqs_plot(dft, Pclass, Survived)
x$data
plot(x)

freqs_plot(dft, Pclass, Survived, Sex, Embarked)

freqs_plot(dft, Pclass, Survived, Sex, Embarked, top = 15)
}
\seealso{
Other Frequency: 
\code{\link{freqs}()},
\code{\link{freqs_df}()},
\code{\link{freqs_list}()}

Other Exploratory: 
\code{\link{corr_cross}()},
\code{\link{corr_var}()},
\code{\link{crosstab}()},
\code{\link{df_str}()},
\code{\link{distr}()},
\code{\link{freqs}()},
\code{\link{freqs_df}()},
\code{\link{freqs_list}()},
\code{\link{lasso_vars}()},
\code{\link{missingness}()},
\code{\link{plot_cats}()},
\code{\link{plot_df}()},
\code{\link{plot_nums}()},
\code{\link{tree_var}()}

Other Visualization: 
\code{\link{distr}()},
\code{\link{freqs}()},
\code{\link{freqs_df}()},
\code{\link{freqs_list}()},
\code{\link{noPlot}()},
\code{\link{plot_chord}()},
\code{\link{plot_survey}()},
\code{\link{plot_timeline}()},
\code{\link{tree_var}()}
}
\concept{Exploratory}
\concept{Frequency}
\concept{Visualization}
