#' Pad numeric vars to strings of specified size
#' @param x Input object
#' @return value Non-string object
#' @keywords color alpha 
#' @seealso \code{\link{is.character}} which this function wraps
#' @export
#' @examples
#' pad(24,mx=4,fill=0)

pad <- function(x,mx=NULL,fill=0) {
  lx <- nchar(as.character(x))
  mx.calc <- max(lx,na.rm=TRUE)
  if (!is.null(mx)) {
    if (mx<mx.calc) {
      stop("number of maxchar is too small")
    }
  } else {
    mx <- mx.calc
  }
  px <- mx-lx
  paste(sapply(px,function(x) paste(rep(fill,x),collapse="")),x,sep="")
}
