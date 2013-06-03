#' Weight a vector of numbers to one, i.e. find their percentage within the total
#'
#' @param x Vector to be transformed
#' @return A vector which sums to 1
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' reproportion(seq(0,10))
#' reproportion(seq(3,10))

 
reproportion<-function(x){ #Function to rescale a set of numbers to a given range
  total<-sum(x,na.rm=TRUE)
  y<- x/total
  return(y)
}
