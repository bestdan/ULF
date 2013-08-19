#' Rolling correlation of a bunch of vars to an index.
#' @description Calculates rolling geometric average returns over the specified window. Note, this is not straight cumulative, but average cumulative. 
#' @param rets Vector of returns for the form 1.05 for a 5 percent return. 
#' @param this.window The number of observations for data to be observed over. 
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


rolling_return <-function (rets, this.window, ret.period = 252) {
  this.len <- length(rets)     # How many datapoints do we have? 
  retvec <- rep(NA, this.len)  # Create the output vector. 
  for (i in this.len:this.window) {  #Move backwards from the max period until the winow size. 
    these.rets <- cumprod(rets[(i - this.window + 1):i])  # Cumprd up just that window.. 
    this.ret <- these.rets[this.window]                   # Get the last observation
    retvec[i] <- this.ret^(ret.period/this.window)
  }
  return(retvec)
}

