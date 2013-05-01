#' Calculates the projected balance of a portfolio from a lognormal distribution
#'
#' @param start_amt numeric Starting balance
#' @param contribution numeric Monthly contribution amount
#' @param t Time horizon, expressed in months
#' @param ret numeric Annual return, e.g. 5 percent, not 0.05
#' @param vol numeric Annual volatility of the portfolio
#' @param p numeric Percentile of cumulative return distribution to calculate
#' @return numeric Vector of account balances through time
#' @keywords finance economics forecasting
#' @seealso nothing
#' @export
#' @examples
#' logn_ptile_balance(0,100,12,6,12,0.5)
#' logn_ptile_balance(100,100,12,6,12,0.05)
#' sapply(seq(0.05,0.95,0.05),function(x) logn_ptile_balance(0,100,12,6,12,x))

logn_ptile_balance<-function(start_amt,contribution,t,ret,vol,p) {
  ret<-log((ret/100)+1)/12  # Converts  5% annual return of "5" into monthy return
  vol<-(vol/100)
  tseq<-seq(1,t)
  p<-qnorm(p)
  balance<-(start_amt+contribution)*exp((ret*tseq) +p*vol*sqrt(tseq/12))
  for (i in 2:(length(balance))) {
    this.tseq<-seq(0,t-i)
    this.contr.value<-contribution*exp((ret*this.tseq) +p*vol*sqrt(this.tseq/12))
    this.contr.value<-c(rep(0,(i-1)),this.contr.value)
    balance<- balance + this.contr.value
  }
  return(balance)
}

