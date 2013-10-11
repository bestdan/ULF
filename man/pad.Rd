\name{pad}
\alias{pad}
\title{Pad numeric vars to strings of specified size}
\usage{
  pad(x, mx = NULL, fill = 0)
}
\arguments{
  \item{x}{Input object}

  \item{mx}{How many places do you want padded?}

  \item{fill}{What should it be padded with?}
}
\value{
  value Non-string object
}
\description{
  Pad numeric vars to strings of specified size
}
\examples{
pad(24,mx=4,fill=0)
}
\seealso{
  \code{\link{is.character}} which this function wraps
}
\keyword{alpha}
\keyword{color}

