#' Generate colors for a given set of cash, bonds & stocks. 
#' @description Creates a set of N green, grey, and blue colors for use with N cash, bonds, & stocks. 
#' @param ncash The number of different cash options
#' @param nbond The number of differen bond options
#' @param neqs The number of differen equity options
#' @return A vector of colors to use
#' @keywords useful little functions, finance, portfolio, colors, asset classes
#' @seealso nothing
#' @export
#' @import fBasics
#' @examples
#' this.colors<-ac_cols(1,4,6)
#' barplot(seq(1,length(this.colors)),col=this.colors)
#' this.colors<-ac_cols(0,4,6)
#' barplot(seq(1,length(this.colors)),col=this.colors)

ac_cols<-function(ncash,nbond,neqs) {
  # Function to allocate asset class colors
  cols<- NA
  if(ncash>0) {
    cash_cols<-monoPalette(n=ncash,name=c("greenmono"))
    cols<-c(cols,cash_cols) }
  if(nbond>0) {
    bnd_cols<-greyPalette(n=nbond)
    cols<-c(cols,bnd_cols)}
  if(neqs>0) {
    eq_cols<-monoPalette(n=neqs,name=c("bluemono"))
    cols<-c(cols,eq_cols)}
  cols<-cols[-1]
  return(cols)
}
