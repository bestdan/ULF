\name{fvSaveInvest}
\alias{fvSaveInvest}
\title{Calculate the future value of savings}
\usage{
  fvSaveInvest(rate, deposit, term, frequency = "annual")
}
\arguments{
  \item{rate}{numeric - Annual percentage return, expressed
  as 5, e.g.}

  \item{deposit}{numeric - the deposit amount}

  \item{term}{numeric - how long you'll be depositing for,
  in years}

  \item{frequency}{string - "annual" or "monthly"}
}
\value{
  string Vector of formatted strings
}
\description{
  Calculate the future value of savings
}
\examples{
fvSaveInvest(7,1200,3,"annual")
fvSaveInvest(7,100,3,"monthly")
}
\seealso{
  \code{\link{nchar}} which this function wraps
}
\keyword{economics}
\keyword{finance}
\keyword{formating}

