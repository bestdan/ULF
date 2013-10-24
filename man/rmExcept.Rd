\name{rmExcept}
\alias{rmExcept}
\title{Remove everything from the environment except X}
\usage{
  rmExcept(x)
}
\arguments{
  \item{x}{A list of objects to not be removed, x=
  c("this","that)}
}
\value{
  Null - operates on the environment.
}
\description{
  Remove everything from the environment except X
}
\examples{
ls()
currentvars<-ls()
this <-seq(1:10)
that <- seq(2:20)
ls()
rmExcept(x=c(currentvars, "this"))  #Removes 'that'.
ls()
}

