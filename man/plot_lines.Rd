\name{plot_lines}
\alias{plot_lines}
\title{Plot all observations as lines against index. The first column is the index.}
\usage{
  plot_lines(mat, ...)
}
\arguments{
  \item{mat}{matrix or dataframe of data to plot}

  \item{...}{Further plotting parameters, e.g. col}
}
\value{
  A plot
}
\description{
  Plot all observations as lines against index. The first
  column is the index.
}
\examples{
obs<-20
rets<-data.frame(a=seq(.7,1.4,length.out=obs),b=runif(obs),c=rnorm(obs))
rets$y<-sin(rets$a+rets$b)
plot_lines(rets)
}
\seealso{
  Nothing.
}
\keyword{annualize,}
\keyword{convenience}
\keyword{finance,}
\keyword{portfolio,}

