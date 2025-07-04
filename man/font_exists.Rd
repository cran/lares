% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_functions.R
\name{font_exists}
\alias{font_exists}
\title{Check if Font is Installed}
\usage{
font_exists(font = "Arial Narrow", font_dirs = NULL, quiet = FALSE, ...)
}
\arguments{
\item{font}{Character. Which font to check. No need to add .TFF.}

\item{font_dirs}{Character vector. Additional directories to check for fonts.}

\item{quiet}{Boolean. Keep quiet? If not, show message}

\item{...}{Additional parameters.}
}
\value{
Boolean result of the existing fonts check.
}
\description{
This function checks if a font is installed in your machine.
To list all available fonts, set \code{font = NULL}.
}
\examples{
font_exists(font = "Arial")
font_exists(font = "arial")
font_exists(font = "")
font_exists(font = NULL)
}
\seealso{
Other Tools: 
\code{\link{autoline}()},
\code{\link{bind_files}()},
\code{\link{bring_api}()},
\code{\link{chr2num}()},
\code{\link{db_download}()},
\code{\link{db_upload}()},
\code{\link{dont_sleep}()},
\code{\link{export_plot}()},
\code{\link{export_results}()},
\code{\link{files_functions}()},
\code{\link{formatColoured}()},
\code{\link{formatHTML}()},
\code{\link{glued}()},
\code{\link{grepm}()},
\code{\link{h2o_selectmodel}()},
\code{\link{haveInternet}()},
\code{\link{image_metadata}()},
\code{\link{importxlsx}()},
\code{\link{ip_data}()},
\code{\link{json2vector}()},
\code{\link{list_cats}()},
\code{\link{listfiles}()},
\code{\link{mail_send}()},
\code{\link{markdown2df}()},
\code{\link{move_files}()},
\code{\link{msplit}()},
\code{\link{myip}()},
\code{\link{quiet}()},
\code{\link{read.file}()},
\code{\link{statusbar}()},
\code{\link{tic}()},
\code{\link{try_require}()},
\code{\link{updateLares}()},
\code{\link{warnifnot}()},
\code{\link{what_size}()}
}
\concept{Tools}
