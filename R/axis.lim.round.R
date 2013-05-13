#' Round a number in log space for use in nice axes. 
#' @description This function is useful for taking a max number from data, and converting to a "nice" round number. 
#' @param x The number to be "nicified" for axes. 
#' @return Another number
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' axis.lim.round(pi^seq(1,10,0.2))


# Rounder - for making nice axis labels -----------------------------------
axis.lim.round<-function(x) {
  this.10<-trunc(log10(x)) #number of 10
  out<-round(x/(10^this.10)+1,0)*(10^this.10)
  return(out)   
}


