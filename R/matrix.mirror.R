#' Mirror a matrix, 
#' 
#' @param x The matrix to be rotated
#' @return The rotated matrix
#' @keywords useful little functions, finance, portfolio, colors, asset classes
#' @seealso nothing
#' @export
#' @examples
#' x<-matrix(seq(1,9),ncol=3)
#' matrix.mirror(x)


matrix.mirror <- function(x) {
  xx <- as.data.frame(x);
  xx <- rev(xx);
  xx <- as.matrix(xx);
  xx;
}

