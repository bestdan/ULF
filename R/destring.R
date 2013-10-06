#' Automatically make an object nonstring
#' @description Automatically add an alpha transparency level to a color.
#' @param x Input object
#' @return value Non-string object
#' @keywords color alpha 
#' @seealso \code{\link{is.character}} which this function wraps
#' @export
#' @examples
#' destring("24")


destring <- function(x) {
  ## convert factor to strings
  if (is.character(x)) {
    as.numeric(x)
  } else if (is.factor(x)) {
    as.numeric(levels(x))[x]
  } else if (is.numeric(x)) {
    x
  } else {
    stop("could not convert to numeric")
  }
}
