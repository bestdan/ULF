\name{dollar}
\alias{dollar}
\title{Format numbers as dollars, i.e. adding commas and $ signs}
\usage{
  dollar(x, fixed = FALSE)
}
\arguments{
  \item{x}{numeric vector to be converted to dollars}

  \item{fixed}{string Should the results be left-aligned?}
}
\value{
  string Vector of formatted strings
}
\description{
  Format numbers as dollars, i.e. adding commas and $ signs
}
\examples{
dollar(10^seq(1,10))
dollar(10^seq(1,10),fixed=TRUE)
}
\seealso{
  \code{\link{nchar}} which this function wraps
}
\keyword{economics}
\keyword{finance}
\keyword{formating}

