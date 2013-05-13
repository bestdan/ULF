#' Plot all observations as lines against index. The first column is the index. 
#'
#' @param mat matrix or dataframe of data to plot
#' @return A plot
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' obs<-20
#' rets<-data.frame(a=seq(.7,1.4,length.out=obs),b=runif(obs),c=rnorm(obs))  
#' rets$y<-sin(rets$a+rets$b)
#' plot_lines(rets)

plot_lines<-function(mat,...) {
  xmin<-min(mat[,1],na.rm=TRUE)
  xmax<-max(mat[,1],na.rm=TRUE)
  ymin<-min(mat[,-1],na.rm=TRUE)
  ymax<-max(mat[,-1],na.rm=TRUE)
  plot(mat[,2]~mat[,1],ylim=c(ymin,ymax),xlim=c(xmin,xmax),type="l",col=1,...)
  for(i in 3:ncol(mat)) {
    lines(mat[,i]~mat[,1],col=i-1)  
  }
}
