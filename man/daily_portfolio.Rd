% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stocks.R
\name{daily_portfolio}
\alias{daily_portfolio}
\title{Daily Portfolio Dataframe}
\usage{
daily_portfolio(hist, trans, cash, cash_fix = 0, window = "MAX")
}
\arguments{
\item{hist}{Dataframe. Result from \code{stocks_hist()}}

\item{trans}{Dataframe. Result from \code{stocks_file()$transactions}}

\item{cash}{Dataframe. Result from \code{stocks_file()$cash}}

\item{cash_fix}{Numeric. If, for some reason, you need to fix your
cash amount for all reports, set the amount here}

\item{window}{Character. Choose any of: "1W", "1M", "6M", "1Y", "YTD", "5Y", "MAX"}
}
\value{
data.frame. Processed at date and portfolio level.
}
\description{
This function creates a dataframe with all relevant metrics and values,
for the overall portfolio, for every day since inception.
}
\seealso{
Other Investment: 
\code{\link{daily_stocks}()},
\code{\link{etf_sector}()},
\code{\link{splot_change}()},
\code{\link{splot_divs}()},
\code{\link{splot_etf}()},
\code{\link{splot_growth}()},
\code{\link{splot_roi}()},
\code{\link{splot_summary}()},
\code{\link{splot_types}()},
\code{\link{stocks_file}()},
\code{\link{stocks_obj}()},
\code{\link{stocks_quote}()},
\code{\link{stocks_report}()}
}
\concept{Investment}
