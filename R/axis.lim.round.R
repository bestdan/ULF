#' Round a number for use in nice axes labels & spacing
#' @description This function is useful for taking a max number from data, and converting to a "nice" round number. 
#' @param x The number to be "nicified" for axes. 
#' @param direction Does the number need to be rounded up (max), or down(min)
#' @return Another number
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' this.seq<- pi^seq(1,10,0.2)
#' up<- axis.lim.round(this.seq, direction="up")
#' down<-  axis.lim.round(this.seq, direction="down")
#' plot(this.seq~seq(1,length(this.seq)),
#'      ylim=c(min(down),max(up)))
#' lines(up~seq(1,length(this.seq)),col="green")
#' lines(down~seq(1,length(this.seq)),col="red")

axis.lim.round<-function(x,direction="up") {
  this.10<-10^trunc(log10(x)) #number of digits
  if (direction=="up") {
    out<- ceiling(x/this.10)*this.10
  } else {
    out<- floor(x/this.10)*this.10
  }
  return(out)     
}

