#' Abbreviate numbers
#' @param x numeric vector to be converted to dollars
#' @return string Vector of formatted strings
#' @keywords finance economics formating 
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' options(scipen=10)
#' x<- 10^seq(1,10)*1.3
#' unlist(sapply(x, numabbr))


numabbr<- function(x) {
  
  temp<- round(log10(max(x)),0)
  
  if(temp<3) {
    return(x)
  }
  temp<- switch(as.character(temp),
                  "3"= paste0((x/ 10^3 ),"k"), 
                  "4"= paste0((x/ 10^3),"k"),
                  "5"= paste0((x/ 10^3),"k"),
                  "6"= paste0((x/ 10^6),"m"),
                  "7"= paste0((x/ 10^6),"m"),
                  "8"= paste0((x/ 10^6),"m"),
                  "9"= paste0((x/ 10^9),"tr"),
                  "10"= paste0((x/ 10^9),"tr"))
  return(temp)  
}

