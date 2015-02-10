#' @title A barplot showing differences
#' @description A matched barplot with values overlaid, and differences higlighted
#' @param data A matrix with series in rows, and observations in columns. 
#' @param cols The plot color for when row 1 is greater than row 2, when row 2 is greater than row 1, and the minimum baseline between them.
#' @param tspace The space between bars
#' @param tborder The border color. 
#' @return A plot
#' @export
#' @examples
#' data<- data.frame(x=seq(100,10,-10), y=seq(10,100,10))
#' data<- t(data)
#' barDiffplot(data)
#' barDiffplot(data, tspace=1, tborder="black")


barDiffplot<- function(data,
                       cols=c("red","green", "dark green"), 
                       tspace=0, tborder=NA) {
  if(nrow(data)!=2) stop("Can only use two rows of data.")
  
  ylims<- range(data)
  
  barplot(data[1,], col=cols[2], ylim=ylims, axes=FALSE, space=tspace, border=tborder)
  par(new=T)
  barplot(data[2,], col=cols[1], ylim=ylims, axes=FALSE, space=tspace, border=tborder)
  par(new=T)
  barplot(pmin(data[2,], data[1,]), col=cols[3], ylim=ylims, space=tspace, border=tborder)
}
