\name{rolling_return}
\alias{rolling_return}
\title{Rolling correlation of a bunch of vars to an index.}
\usage{
  rolling_return(rets, window, ret.period = 365)
}
\arguments{
  \item{rets}{Vector of returns for the form 1.05 for a 5
  percent return.}

  \item{window}{The number of observations for data to be
  observed over.}

  \item{ret.period}{The number of periods per year the
  return is calculated over daily=365, monthly=12, etc..}
}
\value{
  A matrix of correlations
}
\description{
  Calculates rolling geometric average returns over the
  specified window. Note, this is not straight cumulative,
  but average cumulative.
}
\examples{
#rets<-c(1,seq(.7,1.4,.05))
#rets<-rep(1.014,15)
#1.014^12
#rolling_return(rets,2,12)
}
\seealso{
  Nothing.
}
\keyword{annualize,}
\keyword{convenience}
\keyword{finance,}
\keyword{portfolio,}

