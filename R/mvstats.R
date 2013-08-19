#' Function for calcualting Mean Variance statistics
#' @section Note: This is really only useful in specific cases where you can specify the asset allocation by risk level ahead of time. 
#' 
#' @param aa The asset allocation weights
#' @param retvec A vector of expected returns
#' @param covmat The covariance matrix
#' @return A summary matrix of expected return, portfolio volatility, and var5/10/15 for each portfolio
#' @keywords useful little functions, finance, portfolio
#' @seealso nothing
#' @export
#' @examples
#' alloc<-c(0.25,0.25,0.08,0.07,0.25,0.1)
#' rets<-seq(2,12,length.out=length(alloc))
#' vols<-rets*2.25
#' cormat<-diag(1,length(alloc))
#' covmat<-vols * cormat * vols
#' mvstats(aa=alloc,retvec=rets,covmat=covmat)


mvstats<- function(aa,retvec,covmat) {
  # Create summary matrix for outcomes at each risk level. 
  sum_mat<-as.data.frame(matrix(NA,nrow=1,ncol=2))
  names(sum_mat)<-c("ret","vol")
    # Average returns
    sum_mat$ret<- sum(aa*retvec)
    # Vols
    sum_mat$vol<-sqrt(sum(t(aa) %*% (covmat) *aa))
  return(sum_mat)
}
