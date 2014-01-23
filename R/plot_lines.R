#' Plot all observations as lines against index. The first column is the index. 
#'
#' @param mat matrix or dataframe of data to plot
#' @param thisColors A set of colors to use in the plot. 
#' @param ... Further plotting parameters, e.g. col
#' @return A plot
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#'  obs<-20
#'  rets<-data.frame(a=seq(.7,1.4,length.out=obs),b=runif(obs),c=rnorm(obs))  
#'  rets$y<-sin(rets$a+rets$b)
#'  plot_lines(rets)
#'  rets<-data.frame(matrix(runif(obs*20),ncol=20,nrow=obs))  
#'  for (i in 1:ncol(rets)) {
#'  rets[,i]<- rets[,i] + i
#'  }
#'  index<-seq(1,obs)
#'  plot_lines(cbind(index,rets))
plot_lines<- function (mat, thisColors=NULL, ...) 
{
  if (is.null(thisColors)) {
    ncols <- ncol(mat) - 1
    thisColors <- rainbow(ncols)
  }
  xmin <- min(mat[, 1], na.rm = TRUE)
  xmax <- max(mat[, 1], na.rm = TRUE)
  ymin <- min(mat[, -1], na.rm = TRUE)
  ymax <- max(mat[, -1], na.rm = TRUE)
  plot(mat[, 2] ~ mat[, 1], ylim = c(ymin, ymax), xlim = c(xmin, 
                                                           xmax), type = "l", col = thisColors[1], ...)
  for (i in 3:ncol(mat)) {
    lines(mat[, i] ~ mat[, 1], col = thisColors[(i - 1)])
  }
}
