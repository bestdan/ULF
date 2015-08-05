#' @title Make a named vector easily
#' @param ddata A vector of data you want to be 
#' @author Daniel Egan
#' @param vdata The values to be named
#' @param dnames The names to be given to the values.
#' @return A named vector.
#' @export
#' @examples
#' df <- data.frame(value=seq(1:14), labels=LETTERS[seq(1:14)])
#' as.named.vector(df$value, df$labels)

as.named.vector <- function(vdata, dnames){
  vdata <- as.vector(vdata)
  names(vdata) <- dnames
  return(vdata)
}