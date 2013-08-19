\name{ac_cols}
\alias{ac_cols}
\title{Generate colors for a given set of cash, bonds & stocks.}
\usage{
  ac_cols(ncash, nbond, neqs)
}
\arguments{
  \item{ncash}{The number of different cash options}

  \item{nbond}{The number of differen bond options}

  \item{neqs}{The number of differen equity options}
}
\value{
  A vector of colors to use
}
\description{
  Creates a set of N green, grey, and blue colors for use
  with N cash, bonds, & stocks.
}
\examples{
this.colors<-ac_cols(1,4,6)
barplot(seq(1,length(this.colors)),col=this.colors)
this.colors<-ac_cols(0,4,6)
barplot(seq(1,length(this.colors)),col=this.colors)
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

