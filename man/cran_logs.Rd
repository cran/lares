% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cran.R
\name{cran_logs}
\alias{cran_logs}
\title{Download and plot daily downloads of CRAN packages}
\usage{
cran_logs(
  input = "lares",
  from = Sys.Date() - 31,
  to = Sys.Date() - 1,
  type = "daily",
  plot = TRUE
)
}
\arguments{
\item{input}{Character vector with package names or data.frame product of
\code{cranlogs::cran_downloads}.}

\item{from, to}{Dates. Range of dates to fetch downloads metrics.}

\item{type}{Character. Any of: "daily" or "total".}

\item{plot}{Boolean. Create a plot?}
}
\value{
List with data.frame and plot if \code{plot=TRUE}.
}
\description{
Download daily downloads stats from CRAN for any package, and plot.
It can also be used as an auxiliary function to plot
(\code{cranlogs::cran_downloads}) results.
}
\examples{
\donttest{
cran_logs(c("lares", "dplyr"), from = "2021-05-31")
}
}
