\name{getfrom1ka}

\alias{getfrom1ka}

\title{Extract text from HTML content}

\description{
In 1ka JSON output, there are sometimes HTML tags in the text. This function extracts the text from that HTML content.
}

\usage{
getfrom1ka(
  identifier = NULL, key = NULL, survey = NULL,
  action = NULL, params = NULL
)
}

\arguments{
  \item{identifier}{String, the API user identifier.}
  \item{key}{String, the API private key.}
  \item{survey}{String, the API survey id.}
  \item{action}{String, the API action to be performed.}
  \item{params}{Named list containing further API parameters.}
}

\examples{
\dontrun{
identifier <- 'yourAPIid'

private_key <- 'yourAPIprivatekey'

survey_id <- '7050'

action <- 'getSurveyResponseData'

params <- list('usr_id' = '250235')

jsonlist <- getfrom1ka(
  identifier, private_key, survey_id, action, params
)
}
}


\author{
Adrian Dusa
}

\keyword{functions}
