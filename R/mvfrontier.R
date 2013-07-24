#' Function for calcualting Mean Variance frontier
#' @section Note: This is really only useful in specific cases where you can specify the asset allocation by risk level ahead of time. 
#' 
#' @param risk_levels A vector of numeric risk levels, from 0 to 1, to calculate optimal portfolios from
#' @param aaf The asset allocation function, based on risk_levels, to calculate proportions of assets. 
#' @param retvec A vector of expected returns
#' @param covmat The covariance matrix
#' @param maxcashpar At what risk level does the portfolio have zero cash? 
#' @param cashsplit What proportion of 'cash' is allocated to VTIP?
#' @return A summary matrix of expected return, portfolio volatility, and var5/10/15 for each portfolio
#' @keywords useful little functions, finance, portfolio
#' @seealso nothing
#' @export
#' @examples
#' risk_levels<-seq(0,1,0.1) #Risk Levels
#' maxpar<- (-1/length(risk_levels))  #This defines the slope in graph below for linear stocks
#' aaf_v1<-function(risk){
#'   eq_alloc<-c(0.25,0.25,0.08,0.07,0.25,0.1)
#'   bnd_alloc<-c(0.5,0.5)
#'   this.risky_alloc<- eq_alloc*risk
#'   safe_wt<- (1-risk) #proportion in cash and bonds
#'   bnd_wt<-(safe_wt)*bnd_alloc
#'   n_aa<-c(bnd_wt,this.risky_alloc)
#'   return(n_aa)
#' }
#' rets<-seq(2,12,length.out=8)
#' vols<-rets*2.25
#' cormat<-diag(1,8)
#' covmat<-vols * cormat * vols
#' mvfrontier(risk_levels,aaf="aaf_v1",retvec=rets,covmat=covmat,maxcashpar=maxpar)


mvfrontier<- function(risk_levels,aaf,retvec,covmat,maxcashpar,cashsplit) {
  # Create summary matrix for outcomes at each risk level. 
  sum_mat<-as.data.frame(matrix(NA,nrow=length(risk_levels),ncol=3))
  names(sum_mat)<-c("risk","ret","vol")
  row.names(sum_mat)<- paste("r_",risk_levels,sep="")
  for (i in seq_along(risk_levels)) {
    risk<- risk_levels[i]
    #print(risk)
    sum_mat$risk[i]<- risk
    aaf.out<-function(x, type, ...) {
      switch(type,
      "aaf_v2b"           = aaf_v2b(x,maxcashpar),
      "aaf_v2a"           = aaf_v2a(x),
      "aaf_v1"            = aaf_v1(x),
      "aaf_v1_simple"    = aaf_v1_simple(x),
      "aaf_v2c"          = aaf_v2c(x,maxcashpar)  ,
      "aaf_v2d"          = aaf_v2d(x,maxcashpar),  
      "aaf_v3"           = aaf_v3(x,maxcashpar) , 
      "aaf_v3b"          = aaf_v3b(x,maxcashpar=maxcashpar,cashsplit=cashsplit)  ,
      "aaf_bm"           = aaf_bm(x),
      "aaf_bm_dom"       = aaf_bm_dom(x))
    }
    #this.aa<-aaf.out(i,"aaf_v1")
    this.aa<- aaf.out(risk, aaf)
    sum_mat[i,c(2,3)]<- mvstats(this.aa,retvec,covmat)
  }
  return(sum_mat)
}
