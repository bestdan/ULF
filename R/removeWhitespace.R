#' @name removeWhitespace
#' @title Remove whitespace from strings.
#' @param x input string with whitespace to be removed.
#' @return \code{string} A string with now whitespace...
#' @keywords trim string
#' @seealso \code{\link{gsub}} 
#' @export
#' @examples
#' removeWhitespace("   blah    ")

removeWhitespace <-  function (x) gsub("^\\s+|\\s+$", "", x)

