#' Generate colors for a given set of cash, bonds & stocks. 
#' @description Generates a covariance matrix from a correlation matrix and standard deviations. 
#' @author Daniel P. Egan
#' @param corMat The correlation matrix. 
#' @param sds A vector of standard deviations. 
#' @return covMat A covariance matrix
#' @keywords useful little functions, finance, portfolio, colors, asset classes
#' @seealso nothing
#' @export
#' @examples
#' corMat <- matrix(c(1,0,0,0,1,.2,0,.2,1),nrow=3)
#' vols <- c(0,5,20)
#' corr2cov(corMat,vols)


corr2cov<- function(corMat,sds) {
  covMat<- corMat * sds * (rep(sds, each=nrow(corMat))) 
  return(covMat)
}
 