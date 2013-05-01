#' A simple convenience function for calculating proportion tables
#'
#' @param x A dataframe or matrix to calculate proportions from
#' @return numeric Vector of proportions within the vector
#' @keywords useful little functions
#' @seealso nothing
#' @export
#' @examples
#' x<-round(rnorm(100)*10,0)
#' plot(ptable(x))

ptable<- function(x) {
  prop.table(table(x))
}
