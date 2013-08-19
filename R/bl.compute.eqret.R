#' Calculates the Black-Litterman Equilibrium Returns
#'
#' @param risk.aversion The numeric estimate of risk aversion. 
#' @param cov The covariance matrix of the assets. 
#' @param cap.weight The global portfolio allocation across assets. Must sum to 1. 
#' @param risk.free The forward-looking risk-free rate of return. 
#' @return risk.aversion The numeric estimate of risk aversion
#' @keywords finance economics forecasting
#' @seealso nothing
#' @export
#' @examples
#' this.cov<-matrix(runif(9),ncol=3)
#' diag(this.cov)<- 1
#' this.cov[upper.tri(this.cov)]<- this.cov[lower.tri(this.cov)]
#' bl.compute.eqret(3, this.cov, c(.3,.3,.4), 1)


bl.compute.eqret <- function(
  risk.aversion,  # Risk Aversion
  cov,            # Covariance matrix
  cap.weight,     # Market Capitalization Weights
  risk.free = 0   # Risk Free Interest Rate
){
  return( risk.aversion * cov %*% cap.weight +  risk.free)    
}
