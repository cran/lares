% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/facebook.R
\name{fb_accounts}
\alias{fb_accounts}
\title{Facebook Ad Accounts}
\usage{
fb_accounts(
  token,
  business_id = "904189322962915",
  type = c("owned", "client"),
  limit = 100,
  api_version = NULL,
  ...
)
}
\arguments{
\item{token}{Character. Valid access token with sufficient privileges. Visit the
\href{https://developers.facebook.com/tools/explorer}{Facebook API Graph Explorer}
to acquire one.}

\item{business_id}{Character. Business ID.}

\item{type}{Character vector. Values: owned, client.}

\item{limit}{Integer. Query limit by pagination.}

\item{api_version}{Character. Facebook API version.}

\item{...}{Additional parameters.}
}
\value{
data.frame with un-nested processed results fetched with API.
}
\description{
This returns all ad accounts for a FB Business Account FB.
For more information on Ad Insights' API, go to the
\href{https://developers.facebook.com/docs/marketing-api/insights/}{original documentaion}
}
\examples{
\dontrun{
# Query all accounts (owned and with permissions) of a Business ID
accounts <- fb_accounts(YOURTOKEN, YOURBUSINESS)
}
}
\seealso{
Other API: 
\code{\link{bring_api}()},
\code{\link{fb_ads}()},
\code{\link{fb_creatives}()},
\code{\link{fb_insights}()},
\code{\link{fb_process}()},
\code{\link{fb_report_check}()},
\code{\link{fb_rf}()},
\code{\link{fb_token}()},
\code{\link{gemini_ask}()},
\code{\link{gpt_ask}()},
\code{\link{queryGA}()},
\code{\link{slackSend}()}

Other Meta: 
\code{\link{fb_ads}()},
\code{\link{fb_creatives}()},
\code{\link{fb_insights}()},
\code{\link{fb_process}()},
\code{\link{fb_report_check}()},
\code{\link{fb_rf}()},
\code{\link{fb_token}()}
}
\concept{API}
\concept{Meta}
