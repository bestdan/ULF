\name{mysplit}
\alias{mysplit}
\title{Faster version of split}
\usage{
mysplit(x, f, drop = FALSE, ...)
}
\arguments{
  \item{x}{vector or data frame containing values to be
  divided into groups.}

  \item{f}{a factor in the sense that as.factor(f) defines
  the grouping, or a list of such factors in which case
  their interaction is used for the grouping.}

  \item{drop}{logical indicating if levels that do not
  occur should be dropped (if f is a factor or a list).}

  \item{...}{further potential arguments passed to
  methods.}
}
\value{
The value returned from split is a list of vectors
containing the values for the groups. The components of the
list are named by the levels of f (after converting to a
factor, or if already a factor and drop = TRUE, dropping
unused levels). The replacement forms return their right
hand side. unsplit returns a vector or data frame for which
split(x, f) equals value
}
\description{
Faster version of split
}
\examples{
x <- data.frame(a = 1:10, b = rep(1:2, each=5))
splitx <- mysplit(x, x$b)
}
\seealso{
None
}
\keyword{split}

