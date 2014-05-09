#' Format numbers as dollars, i.e. adding commas and $ signs
#'
#' @param x numeric vector to be converted to dollars
#' @param fixed string Should the results be left-aligned? 
#' @return string Vector of formatted strings
#' @keywords finance economics formating 
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' dollar(10^seq(1,10))
#' dollar(10^seq(1,10),fixed=TRUE)


dollar<-function(x, fixed=FALSE, rounder=2){
  x<- round(x, rounder)
  if (fixed==TRUE) {
    return(paste("$",prettyNum(x,big.mark=",",preserve.width="common"),sep=""))  
  } else {
    return(paste("$",prettyNum(x,big.mark=",",preserve.width="individual"),sep=""))
  }
}
