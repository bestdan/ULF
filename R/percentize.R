#' Create percent labels based on a numeric vector
#' 
#' @param x A numeric vector. 
#' @param rounder The number of places to round to, before creating the percent.
#' @return A summary matrix of expected return, portfolio volatility, and var5/10/15 for each portfolio
#' @keywords useful little functions, labels, percent, string
#' @seealso nothing
#' @export
#' @examples
#' percentize(runif(10),rounder=3)

percentize<-function(x,rounder=2) {
  x<-round(x,rounder)
  y<-paste0(x*100,"%")
  return(y)
}
