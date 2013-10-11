#' Replace a set of old things with new things. :)
#' @description Automatically add an alpha transparency level to a color.
#' @param x Input object
#' @param old A vector of old things to be replaced in x
#' @param new A vector or new things to replace the old things
#' @return value A version of X with all olds replaced by news
#' @keywords color alpha 
#' @seealso \code{\link{is.character}} which this function wraps
#' @export
#' @examples
#' replace2(LETTERS,old=c("A","C","E"),new=seq(1,3))

replace2<-function(x,old,new) {
  y <- x
  if (length(old) !=length(new)) stop("Old and New must have the same length")
  for (i in 1:length(old)) {
    y[x==old[i]] <- new[i]
  }
  return(y)
}
