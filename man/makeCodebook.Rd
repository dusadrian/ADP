\name{makeCodebook}

\alias{makeCodebook}

\title{Create a DDI Codebook}

\description{
Create a DDI Codebook from an SPSS sav file and a 1ka JSON file.
}

\usage{
makeCodebook(sav, json, lang = "si")
}

\arguments{
  \item{sav}{String, path to the SPSS file.}
  \item{json}{List, the R object containing the JSON file.}
  \item{lang}{String, the language of the DDI Codebook file.}
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

makeCodebook('path/to/your/file.sav', jsonlist)
}
}


\author{
Adrian Dusa
}

\keyword{functions}
