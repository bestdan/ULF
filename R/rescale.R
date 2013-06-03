#' Rescale a vector to have the min & max values specified
#'
#' @param x Vector to be transformed
#' @param smin The new scale minimum value, default = 0
#' @param smax The new scale maximum value, default = 1
#' @return A new vector
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' rescale(seq(3,10),smin=3,smax=7)
#' rescale(seq(3,10),smin=3,smax=-22)

rescale<-function(x,smin=0,smax=1){ 
  #stopifnot(smax>smin)
  y<- x-min(x,na.rm=TRUE)
  y<- y/max(y,na.rm=TRUE)
  new.range<- (smax-smin)
  y<- y*new.range
  y<- y + smin
  return(y)
}
