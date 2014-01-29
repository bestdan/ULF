\name{percentize}
\alias{percentize}
\title{Create percent labels based on a numeric vector}
\usage{
percentize(x, rounder = 2)
}
\arguments{
  \item{x}{A numeric vector.}

  \item{rounder}{The number of places to round to, before
  creating the percent.}
}
\value{
A summary matrix of expected return, portfolio volatility,
and var5/10/15 for each portfolio
}
\description{
Create percent labels based on a numeric vector
}
\examples{
percentize(0.015565)
percentize(0.015565, rounder=5)
}
\seealso{
nothing
}
\keyword{functions,}
\keyword{labels,}
\keyword{little}
\keyword{percent,}
\keyword{string}
\keyword{useful}

