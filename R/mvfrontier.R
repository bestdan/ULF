#' Function for calcualting Mean Variance frontier
#' @section Note: This is really only useful in specific cases where you can specify the asset allocation by risk level ahead of time. 
#' 
#' @param risk_levels A vector of numeric risk levels, from 0 to 1, to calculate optimal portfolios from
#' @param aaf The asset allocation function, based on risk_levels, to calculate proportions of assets. 
#' @param retvec A vector of expected returns
#' @param covmat The covariance matrix
#' @param periods Number of periods, per year, the data represents. I.e. monthly=12. 
#' @param maxcashpar At what risk level does the portfolio have zero cash? 
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
#' mvfrontier(risk_levels,aaf="aaf_v1",retvec=rets,covmat=covmat,periods=1,maxcashpar=maxpar)


mvfrontier<- function(risk_levels,aaf,retvec,covmat,periods=12,maxcashpar) {
  # Create summary matrix for outcomes at each risk level. 
  sum_mat<-as.data.frame(matrix(NA,nrow=length(risk_levels),ncol=6))
  names(sum_mat)<-c("risk","ret","vol","var15","var10","var05")
  row.names(sum_mat)<- paste("r_",risk_levels,sep="")
  for (i in seq_along(risk_levels)) {
    risk=risk_levels[i]
    sum_mat$risk[i]<- risk
    FUN <- match.fun(aaf) #Find the named asset allocation function 
      if (aaf=="aaf_v2b") this.aa<-FUN(risk,maxcashpar)  
      if (aaf=="aaf_v2a") this.aa<-FUN(risk)
      if (aaf=="aaf_v1") this.aa<-FUN(risk)
    # Average returns
    sum_mat$ret[i]<- sum(this.aa*retvec)
    # Vols
    sum_mat$vol[i]<-sqrt(sum(t(this.aa) %*% (covmat) *this.aa))
    #this.port.vol<-sum_mat$vol[i]
    ##sum_mat$var15[i]<-  (-1.04*this.port.vol)  + this.ret
    #sum_mat$var10[i]<- (-1.3*this.port.vol) + this.ret #one sided 
    #sum_mat$var05[i]<- (-1.65*this.port.vol) + this.ret #one sided 
    rm(this.aa)
  }
  return(sum_mat)
}

