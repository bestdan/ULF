\name{corr2cov}
\alias{corr2cov}
\title{Generate colors for a given set of cash, bonds & stocks.}
\usage{
  corr2cov(corMat, sds)
}
\arguments{
  \item{corMat}{The correlation matrix.}

  \item{sds}{A vector of standard deviations.}
}
\value{
  covMat A covariance matrix
}
\description{
  Generates a covariance matrix from a correlation matrix
  and standard deviations.
}
\examples{
corMat <- matrix(c(1,0,0,0,1,.2,0,.2,1),nrow=3)
vols <- c(0,5,20)
corr2cov(corMat,vols)
}
\author{
  Daniel P. Egan
}
\seealso{
  nothing
}
\keyword{asset}
\keyword{classes}
\keyword{colors,}
\keyword{finance,}
\keyword{functions,}
\keyword{little}
\keyword{portfolio,}
\keyword{useful}

