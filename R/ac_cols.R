#' Function to generate colors for a given set of cash, bonds & stocks. 
#' 
#' @param ncash The number of different cash options
#' @param nbond The number of differen bond options
#' @param nbond The number of differen equity options
#' @return A vector of colors to use
#' @keywords useful little functions, finance, portfolio, colors, asset classes
#' @seealso nothing
#' @export
#' @import fBasics
#' @examples
#' this.colors<-ac_cols(1,4,6)
#' barplot(seq(1,length(this.colors)),col=this.colors)

ac_cols<-function(ncash,nbond,neqs) {
  # Function to allocate asset class colors
  cash_cols<-monoPalette(n=ncash,name=c("greenmono"))
  bnd_cols<-greyPalette(n=nbond)
  eq_cols<-monoPalette(n=neqs,name=c("bluemono"))
  ac_cols<-c(cash_cols,bnd_cols,eq_cols)
  return(ac_cols)
}
