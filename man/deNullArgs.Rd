% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/deNullArgs.R
\name{deNullArgs}
\alias{deNullArgs}
\title{de-Null the arguments to a parent function.}
\usage{
deNullArgs(exclude = NULL)
}
\arguments{
\item{exclude}{A set of variables you want to keep \code{NULL}.}
}
\value{
Nothing, modifies parent environment
}
\description{
This function looks at the parent or calling function, and checks if any arguments that have
defaults are currently null. If they are, it replaces the NULL with the default.
}
\examples{
checkFun<- function(x=data.frame(x=5)){
 deNullArgs()
 print(x)
}

checkFun()
checkFun(x=NULL)
checkFun(x=10)
}
\author{
Daniel Egan
}

