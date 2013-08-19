#' Calculates Risk Aversion from a set of benchmark returns and a risk-free rate of return
#'
#' @param bench The vector of benchmark returns
#' @param risk.free The numeric or vector of risk free rates of return to remove from the benchmark returns
#' @return risk.aversion The numeric estimate of risk aversion
#' @keywords finance economics forecasting
#' @seealso nothing
#' @export
#' @examples
#' risky.returns<-rnorm(1000,5,sd=11)
#' riskfree.returns<-rnorm(1000,1,sd=1)
#' bl.compute.riskaversion(risky.returns,riskfree.returns)


bl.compute.riskaversion<-function(bench, risk.free =0 ) {
  lambda <- (bench - risk.free) / var(bench,na.rm=TRUE)
  lambda<- mean(lambda,na.rm=TRUE)
  return( as.double(lambda) )
}
