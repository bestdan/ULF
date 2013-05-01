\name{ptable}
\alias{ptable}
\title{A simple convenience function for calculating proportion tables}
\usage{
  ptable(x)
}
\arguments{
  \item{x}{A dataframe or matrix to calculate proportions
  from}
}
\value{
  numeric Vector of proportions within the vector
}
\description{
  A simple convenience function for calculating proportion
  tables
}
\examples{
x<-round(rnorm(100)*10,0)
plot(ptable(x))
}
\seealso{
  nothing
}
\keyword{functions}
\keyword{little}
\keyword{useful}

