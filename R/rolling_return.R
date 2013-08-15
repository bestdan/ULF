#' Rolling correlation of a bunch of vars to an index.
#' @description Calculates rolling geometric average returns over the specified window. Note, this is not straight cumulative, but average cumulative. 
#' @param rets Vector of returns for the form 1.05 for a 5 percent return. 
#' @param window The number of observations for data to be observed over. 
#' @param ret.period The number of periods per year the return is calculated over daily=365, monthly=12, etc..
#' @return A matrix of correlations
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' #rets<-c(1,seq(.7,1.4,.05))  
#' #rets<-rep(1.014,15) 
#' #1.014^12
#' #rolling_return(rets,2,12)


rolling_return<-function(rets,window,ret.period=365){
  this.len<-length(rets) #number of ret observations
  retvec<-rep(NA,this.len) #holder for output
  for (i in this.len:window) {   #decrement from number of rets to window
    these.rets<-cumprod(rets[(i-window+1):i])
    this.ret<-these.rets[window]  
    retvec[i]<-this.ret^(ret.period/window)
  }
  return(retvec)
}


