\name{logn_ptile_bal_varret}
\alias{logn_ptile_bal_varret}
\title{Calculates the projected balance of a portfolio from a lognormal distribution}
\usage{
  logn_ptile_bal_varret(start_amt, contribution, t, ret,
    vol, p)
}
\arguments{
  \item{start_amt}{numeric Starting balance}

  \item{contribution}{numeric Monthly contribution amount}

  \item{t}{Time horizon, expressed in months}

  \item{ret}{vector Vector or Annual return, e.g. 5
  percent, not 0.05}

  \item{vol}{numeric Annual volatility of the portfolio}

  \item{p}{numeric Percentile of cumulative return
  distribution to calculate}
}
\value{
  numeric Vector of account balances through time
}
\description{
  Calculates the projected balance of a portfolio from a
  lognormal distribution
}
\examples{
logn_ptile_balance(0,100,12,6,12,0.5)
logn_ptile_balance(100,100,12,6,12,0.05)
sapply(seq(0.05,0.95,0.05),function(x) logn_ptile_balance(0,100,12,6,12,x))
}
\seealso{
  nothing
}
\keyword{economics}
\keyword{finance}
\keyword{forecasting}

