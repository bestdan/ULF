#' Automatically add an alpha transparency level to a color. 
#' @description Automatically add an alpha transparency level to a color.
#' @param col input color vector
#' @param alpha input transparency level (0 - 100)
#' @return string color vector
#' @keywords color alpha 
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' x<-seq(0,100)
#' y<-sin(x)
#' cols<-colors(1)[x]
#' plot(y~x,col=colors(1)[x],pch=19,cex=5)
#' alpha.cols<-add.alpha(cols,0.5)
#' plot(y~x,col=alpha.cols,pch=19,cex=5)

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


