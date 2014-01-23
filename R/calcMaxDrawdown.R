#'   Calculate the maximum drawdown in a series of returns
#' @description A function which reinvests dividends back where they came from. 
#' @note Can be use with negative returns to calculate maximum upswing. 
#' @author Daniel Egan
#' @param data A vector in sorted chronological return, of returns expressed as 0.05 for a 5 percent return
#' @return A number representing the maximum cumulative drawdown in the data series. 
#' @export
#' @examples
#' ex_data<- c(0, 0.05, -0.02, -0.05, 0.20, -0.30)
#' calcMaxDrawdown(ex_data)

calcMaxDrawdown <- function(data) {
  delta<- outer(cumprod(data+1), cumprod(data+1),"/")
  delta[upper.tri(delta)]<-1
  delta<- delta-1
  maxDD <- min(delta) # Only look at chronologically correct returns
  return(maxDD)
}
  