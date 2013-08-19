\name{XgetSymbols}
\alias{XgetSymbols}
\title{Get prices & returns from XIgnite API}
\usage{
  XgetSymbols(symbol, start_date,
    end_date = as.character(Sys.Date()), adjtype,
    quotetype = "LastClose", token = "NA")
}
\arguments{
  \item{symbol}{The symbol to be queries}

  \item{start_date}{The beginning of the historical date
  range}

  \item{end_date}{The end of the historical date range.
  Defaults to today.}

  \item{adjtype}{The type of adjustment, if any, to be
<<<<<<< HEAD
  made. Potentially}

  \item{token}{Your API Token "None", "SplitOnly",
=======
  made. Potentially "None", "SplitOnly",
>>>>>>> cc6773c792a756d9950f8a94ecb260e42f7af8dd
  "CashDividendOnly", "SplitAndProportionalCashDividend",
  "SplitAndCashDividend", "All"}

  \item{quotetype}{The type of data to be returns. Options
  are Last, Open, High, Low, Volume, LastClose (the
  default), ChangeFromOpen, PercentChangeFromOpen,
  ChangeFromLastClose, PercentChangeFromLastClose,}
<<<<<<< HEAD
=======

  \item{token}{Your XIgnite API Token}
>>>>>>> cc6773c792a756d9950f8a94ecb260e42f7af8dd
}
\value{
  A dataframe of date and values
}
\description{
  Get prices & returns from XIgnite API
}
\examples{
<<<<<<< HEAD
data<-XgetSymbols("EEM","2013-01-01",adjtype="SplitOnly")
=======
data<-XgetSymbols("EEM","2013-01-01",adjtype="SplitOnly",token="56709B5C32C441D782B558DCCC923CBB")
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

