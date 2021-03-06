% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/axis.lim.round.R
\name{axis.lim.round}
\alias{axis.lim.round}
\title{Round a number for use in nice axes labels & spacing}
\usage{
axis.lim.round(x, direction = "up")
}
\arguments{
\item{x}{The number to be "nicified" for axes.}

\item{direction}{Does the number need to be rounded up (max), or down(min)}
}
\value{
Another number
}
\description{
This function is useful for taking a max number from data, and converting to a "nice" round number.
}
\examples{
this.seq<- pi^seq(1,10,0.2)
up<- axis.lim.round(this.seq, direction="up")
down<-  axis.lim.round(this.seq, direction="down")
plot(this.seq~seq(1,length(this.seq)),
     ylim=c(min(down),max(up)))
lines(up~seq(1,length(this.seq)),col="green")
lines(down~seq(1,length(this.seq)),col="red")
}
\seealso{
Nothing.
}
\keyword{annualize,}
\keyword{convenience}
\keyword{finance,}
\keyword{portfolio,}

