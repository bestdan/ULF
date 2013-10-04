#' Create percent labels based on a numeric vector
#' 
#' @param x A numeric vector. 
#' @param rounder The number of places to round to, before creating the percent.
#' @return A summary matrix of expected return, portfolio volatility, and var5/10/15 for each portfolio
#' @keywords useful little functions, labels, percent, string
#' @seealso nothing
#' @export
#' @examples
#' percentize(0.015565)
#' percentize(0.015565, rounder=5)



percentize<-function(x,rounder=2) {
  x<-prettyNum(round(x,rounder)*100, digits=max(1,(rounder-2)))
  y<-paste0(x,"%")
  return(y)
}


