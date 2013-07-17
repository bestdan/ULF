\name{RFRYieldCurveUpdate}
\alias{RFRYieldCurveUpdate}
\title{Modifies expected returns to reflect the risk-free-rate yield curve over time}
\usage{
  RFRYieldCurveUpdate(ret, b1, b2)
}
\arguments{
  \item{ret}{An mean expected return}

  \item{b1}{The first coefficient of the yield curve
  expressed in months}

  \item{b2}{The second (curve) coefficient of the yield
  curve expressed in months}
}
\value{
  numeric A vector of month-based mean expected returns
  including the expected risk-free-variation
}
\description{
  Modifies expected returns to reflect the risk-free-rate
  yield curve over time
}
\section{Note}{
  This takes a mean expected return from an AA frontier,
  and a functional specification of a yield curve, and
  returns the forward-looking mean return at various time
  horizons.
}
\examples{
temp<-RFRYieldCurveUpdate(1.5,0.0003,-.0000005)
plot(temp)
}
\seealso{
  nothing
}
\keyword{economics}
\keyword{finance}
\keyword{forecasting}

