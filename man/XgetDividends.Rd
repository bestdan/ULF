\name{XgetDividends}
\alias{XgetDividends}
\title{Get prices & returns from XIgnite API}
\usage{
XgetDividends(symbol, start_date, end_date = as.character(Sys.Date()),
  token = "NA", ShowURL = FALSE)
}
\arguments{
  \item{symbol}{The symbol to be queries}

  \item{start_date}{The beginning of the historical date
  range}

  \item{end_date}{The end of the historical date range.
  Defaults to today.}

  \item{token}{Your API Token}

  \item{ShowURL}{Should the XIgnite API ULR be shown (helps
  with debugging)}
}
\value{
A dataframe of date and dividends
}
\description{
Get prices & returns from XIgnite API
}
\examples{
# Note: No examples will work without a valid token.
# data<-XgetDividends("AGG","2004-01-01")
# head(data,20)
}
\seealso{
Nothing.
}
\keyword{annualize}
\keyword{convenience}
\keyword{finance}
\keyword{portfolio}

