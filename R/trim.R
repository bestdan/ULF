#' Pull dataframe names
#'
#' @param x input string
#' @return string name vector
#' @keywords trim
#' @seealso \code{\link{gsub}} 
#' @export
#' @examples
#' trim("   blah    ")

trim <-  function (x) gsub("^\\s+|\\s+$", "", x)

