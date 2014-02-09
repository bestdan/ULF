#' Create percent labels based on a numeric vector
#' 
#' @param x A numeric vector. 
#' @param rounder The number of places to round to, before creating the percent.
#' @param trailing How many places should be shown, after the decimal. For 0.XX\%, choose 2. 
#' @return \code{string} A correctly formated number.
#' @keywords useful little functions, labels, percent, string
#' @seealso round2
#' @keywords ULF
#' @export
#' @examples
#' percentize(0.015565)  # Should be 2%
#' percentize(0.015565, rounder=1) # Should be 1.6%
#' percentize(0.015565, rounder=1, trailing=2) # Should be 1.60%
#' percentize(0.0045, rounder=1)
#' percentize(0.0045, rounder=2)
#' percentize(0.0004, rounder=3, trailing=2)


percentize<-function(x,rounder=0, trailing=0) {
  x <- as.numeric(prettyNum(round(x*100,rounder+1), digits=max(0,(rounder+1))))
  x <- formatC(x, digits=max(rounder,trailing), format="f")
  y <- paste0(x,"%")
  return(y)
}


