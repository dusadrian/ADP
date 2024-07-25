\name{qtext}

\alias{qtext}

\title{Extract text from HTML content}

\description{
In 1ka JSON output, there are sometimes HTML tags in the text. This function extracts the text from that HTML content.
}

\usage{
qtext(x)
}

\arguments{
  \item{x}{String, possibly containing HTML tags.}
}

\examples{
qtext("Text with no HTML tags.")

qtext("Text with <b>HTML</b> tags.")

qtext("<span style=\"font-size:14px;\">subquestion </span>Click and write subquestion")
}


\author{
Adrian Dusa
}

\keyword{functions}
