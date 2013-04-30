#' Pull dataframe names
#'
#' @param x input string
#' @return string name vector
#' @keywords trim
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' trim("   blah    ")

trim <-  function (x) gsub("^\\s+|\\s+$", "", x)

