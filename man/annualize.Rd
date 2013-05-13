\name{annualize}
\alias{annualize}
\title{Annualize, or actually change to any period, returns data.}
\usage{
  annualize(ret, period = 12, informat = "a",
    outformat = "c")
}
\arguments{
  \item{ret}{Vector of returns}

  \item{period}{At the data level, how many periods are
  there per year? For example, monthly data=12, trading
  days ~=252. 5 years=1/5}

  \item{informat}{What format is the input returns in? For
  a 5 percent return, a = 0.05, b = 1.05, c = 5}

  \item{outformat}{What format should the output returns be
  in? For a 5 percent return, a = 0.05, b = 1.05, c = 5}
}
\value{
  string name vector
}
\description{
  This function takes a return of a given formated input,
  and modifies the period it is expressed over. This does
  not only annualize, but can also monthly-ize, daily-ize,
  etc. .
}
\examples{
annualize(.05,12,"a","a")
annualize(1.05,12,"b","b")
annualize(5,12,"c","c")
annualize(5,12,"c","a")
annualize(5,12,"c","b")
}
\seealso{
  \code{\link{gsub}}
}
\keyword{annualize,}
\keyword{convenience}
\keyword{finance,}
\keyword{portfolio,}

