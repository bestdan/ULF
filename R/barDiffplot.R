#' @title A barplot showing differences
#' @description A matched barplot with values overlaid, and differences higlighted
#' @param data A matrix with series in rows, and observations in columns. 
#' @param cols The plot color for when row 1 is greater than row 2, when row 2 is greater than row 1, and the minimum baseline between them.
#' @return A plot
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' data<- data.frame(x=seq(100,10,-10), y=seq(10,100,10))
#' data<- t(data)
#' barplot(data)


barDiffplot<- function(data,cols=c("red","green", "dark green", space=0, border=NA)) {
  if(nrow(data)!=2) stop("Can only use two rows of data.")
  ylims<- range(data)
  barplot(data[1,], col=cols[2], ylim=ylims, axes=FALSE, space=space, border=border)
  par(new=T)
  barplot(data[2,], col=cols[1], ylim=ylims, axes=FALSE, space=space, border=border)
  par(new=T)
  barplot(pmin(data[2,], data[1,]), col=cols[3], ylim=ylims, space=space, border=border)
}
