% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_mining.R
\name{remove_stopwords}
\alias{remove_stopwords}
\title{Remove stop-words and patterns from character vector}
\usage{
remove_stopwords(text, stop_words, exclude = NULL, sep = " ")
}
\arguments{
\item{text}{Character vector}

\item{stop_words}{Character vector. Words to exclude from text. Example:
if you want to exclude "a", whenever that word appears it will be excluded,
but when the letter "a" appears in a word, it will remain.}

\item{exclude}{Character. Pattern to exclude using regex.}

\item{sep}{Character. String that separate the terms.}
}
\value{
Character vector with removed texts.
}
\description{
Remove all stop-words and specific patterns from a character vector
}
\examples{
x <- c("A brown fox jumps over a dog.", "Another brown dog.")
remove_stopwords(x, stop_words = c("dog", "brown", "a"), exclude = "\\\\.")
}
\seealso{
Other Text Mining: 
\code{\link{cleanText}()},
\code{\link{ngrams}()},
\code{\link{replaceall}()},
\code{\link{sentimentBreakdown}()},
\code{\link{textCloud}()},
\code{\link{textFeats}()},
\code{\link{textTokenizer}()},
\code{\link{topics_rake}()}
}
\concept{Text Mining}
