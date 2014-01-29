\name{XgetSymbols}
\alias{XgetSymbols}
\title{Get prices & returns from XIgnite API}
\usage{
XgetSymbols(symbol, start_date, end_date = as.character(Sys.Date()), adjtype,
  quotetype = "LastClose", token = "NA", ShowURL = FALSE)
}
\arguments{
  \item{symbol}{The symbol to be queries}

  \item{start_date}{The beginning of the historical date
  range}

  \item{end_date}{The end of the historical date range.
  Defaults to today.}

  \item{adjtype}{The type of adjustment, if any, to be
  made. Potentially "None", "SplitOnly",
  "CashDividendOnly", "SplitAndProportionalCashDividend",
  "SplitAndCashDividend", "All"}

  \item{quotetype}{The type of data to be returns. Options
  are Last, Open, High, Low, Volume, LastClose (the
  default), ChangeFromOpen, PercentChangeFromOpen,
  ChangeFromLastClose, PercentChangeFromLastClose,}

  \item{token}{Your XIgnite API Token}

  \item{ShowURL}{Should the XIgnite API ULR be shown (helps
  with debugging)}
}
\value{
A dataframe of date and values
}
\description{
Get prices & returns from XIgnite API
}
\examples{
# Note: no examples will work without a token.
# data<-XgetSymbols("EEM", start_date="2013-01-01", adjtype="SplitOnly")
# head(data,20)
}
\seealso{
Nothing.
}
\keyword{annualize,}
\keyword{convenience}
\keyword{finance,}
\keyword{portfolio,}

