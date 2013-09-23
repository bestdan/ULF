#' Collapse a vector into a comma separated string
#'
#' @author Patrick Burns
#' @param x vector A vector of something, the elements of which are to be collapsed 
#' into a comma separated character string.
#' @return string A comma separated string pf elements
#' @keywords paste
#' @seealso \code{\link{paste0}}
#' @export
#' @note This is particularly useful for giving RMySQL a vector of something which it needs to read in the 
#' format (1,2,3) rather than (1 2 3).
#' @examples
#' x <- c(1,2,3)
#' pasteC(x)

pasteC <- function(x){
  p <- paste(x, collapse=", ")
  p
}