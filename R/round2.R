#' Round to two places. Just a convenience thang...
#' @param x A numeric vector. 
#' @return A vector of rounder values. 
#' @keywords useful little functions, percent, 
#' @seealso nothing
#' @export
#' @examples
#' round2(runif(10))

round2<-function(x) { round(x,2)}
