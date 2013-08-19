\name{XgetDividends}
\alias{XgetDividends}
\title{Get prices & returns from XIgnite API}
\usage{
  XgetDividends(symbol, start_date,
    end_date = as.character(Sys.Date()), token = "NA")
}
\arguments{
  \item{symbol}{The symbol to be queries}

  \item{start_date}{The beginning of the historical date
  range}

  \item{end_date}{The end of the historical date range.
  Defaults to today.}

  \item{token}{Your API Token}
}
\value{
  A dataframe of date and dividends
}
\description{
  Get prices & returns from XIgnite API
}
\examples{
<<<<<<< HEAD
data<-XgetDividends("AGG","2004-01-01")
=======
data<-XgetDividends("AGG","2004-01-01",token="56709B5C32C441D782B558DCCC923CBB")
>>>>>>> cc6773c792a756d9950f8a94ecb260e42f7af8dd
head(data,20)
}
\seealso{
  Nothing.
}
\keyword{annualize,}
\keyword{convenience}
\keyword{finance,}
\keyword{portfolio,}

