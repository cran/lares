% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linkedin.R
\name{li_auth}
\alias{li_auth}
\title{OAuth Linkedin}
\usage{
li_auth(app_name = NA, client_id = NA, client_secret = NA)
}
\arguments{
\item{app_name}{Character. Your App's given name.}

\item{client_id}{Character. Your App's client ID.}

\item{client_secret}{Character. Your App's client secret.}
}
\value{
Character. String with token requested.
}
\description{
This function authenticates and creates a token for LinkedIn's
API REST
}
\seealso{
Other API: 
\code{\link{bring_api}()},
\code{\link{fb_accounts}()},
\code{\link{fb_ads}()},
\code{\link{fb_creatives}()},
\code{\link{fb_insights}()},
\code{\link{fb_process}()},
\code{\link{fb_report_check}()},
\code{\link{fb_rf}()},
\code{\link{fb_token}()},
\code{\link{gemini_ask}()},
\code{\link{gpt_ask}()},
\code{\link{li_profile}()},
\code{\link{queryGA}()},
\code{\link{slackSend}()}

Other LinkedIn: 
\code{\link{li_profile}()}
}
\concept{API}
\concept{LinkedIn}
