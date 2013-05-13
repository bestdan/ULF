#' Flip a matrix - rotate180, then flip. See example. 
#' 
#' @param x The matrix to be rotated
#' @return The rotated matrix
#' @keywords useful little functions, finance, portfolio, colors, asset classes
#' @seealso nothing
#' @export
#' @examples
#' x<-matrix(seq(1,9),ncol=3)
#' matrix.flip(x)

matrix.flip <- function(x) {
  matrix.mirror(matrix.rotate180(x))
}
