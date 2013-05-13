#' Rotate a matrix 180 degrees. See example. 
#' 
#' @param x The matrix to be rotated
#' @return The rotated matrix
#' @keywords useful little functions, finance, portfolio, colors, asset classes
#' @seealso nothing
#' @export
#' @examples
#' x<-matrix(seq(1,9),ncol=3)
#' matrix.rotate180(x)

matrix.rotate180 <- function(x) { 
  xx <- rev(x);
  dim(xx) <- dim(x);
  xx;
}

