\name{rescale}
\alias{rescale}
\title{Rescale a vector to have the min & max values specified}
\usage{
rescale(x, smin = 0, smax = 1)
}
\arguments{
  \item{x}{Vector to be transformed}

  \item{smin}{The new scale minimum value, default = 0}

  \item{smax}{The new scale maximum value, default = 1}
}
\value{
A new vector
}
\description{
Rescale a vector to have the min & max values specified
}
\examples{
rescale(seq(3,10),smin=3,smax=7)
rescale(seq(3,10),smin=3,smax=-22)
}
\seealso{
Nothing.
}
\keyword{annualize,}
\keyword{convenience}
\keyword{finance,}
\keyword{portfolio,}

