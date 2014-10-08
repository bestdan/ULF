#' Abbreviate numbers
#' @param x numeric vector to be converted to dollars
#' @return string Vector of formatted strings with abbreviations.
#' @keywords finance economics formating 
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' options(scipen=20)
#' x<- 10^seq(1,11)*4
#' cbind(formatC(x,big.mark = ",",format = "f", digits=0), unlist(sapply(x, numabbr)))


numabbr<- function(x) {
  
  temp<- floor(log10(max(x)))
  
  if(temp<3) {
    return(x)
  }
  temp<- switch(as.character(temp),
                  "3"= paste0((x/ 10^3 ),"k"), 
                  "4"= paste0((x/ 10^3),"k"),
                  "5"= paste0((x/ 10^3),"k"),
                  "6"= paste0((x/ 10^6),"mm"),
                  "7"= paste0((x/ 10^6),"mm"),
                  "8"= paste0((x/ 10^6),"mm"),
                  "9"= paste0((x/ 10^9),"tr"),
                  "10"= paste0((x/ 10^9),"tr"),
                  "11"= paste0((x/ 10^9),"tr"))
  return(temp)  
}


