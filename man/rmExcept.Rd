\name{rmExcept}
\alias{rmExcept}
\title{Remove everything from the environment except X}
\usage{
  rmExcept(vars, envir = .GlobalEnv)
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
oldvar <- "c"
currentvars<-ls()
this <-seq(1:10)
that <- seq(2:20)
other <- c(2,4)
ls()
rmExcept(c(currentvars,"this"))  #Removes 'that'.
ls()
}

