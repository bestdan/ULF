#' Abbreviate numbers
#' @param x numeric vector to be converted to dollars
#' @param r Number of digits after the decimal to round to.
#' @return string Vector of formatted strings with abbreviations.
#' @keywords finance economics formating 
#' @export
#' @examples
#' options(scipen=20)
#' x<- 10^seq(1,11)*4.567
#' cbind(formatC(x,big.mark = ",",format = "f", digits=0), unlist(sapply(x, numabbr)))
#' cbind(formatC(x,big.mark = ",",format = "f", digits=0), unlist(sapply(x, function(x) 
#'   numabbr(x, r=0))))


numabbr<- function(x,r=2) {
  
  temp<- floor(log10(max(x)))
  
  if(temp<3) {
    return(round(x,r))
  }
  
  abbr<- function(y,e,r) {
    round((x/ 10^e ),r)
  }
  
  temp<- switch(as.character(temp),
                  "3"= paste0(abbr(x,3,r),"k"), 
                  "4"= paste0(abbr(x,3, r),"k"),
                  "5"= paste0(abbr(x,3, r),"k"),
                  "6"= paste0(abbr(x,6, r),"mm"),
                  "7"= paste0(abbr(x,6, r),"mm"),
                  "8"= paste0(abbr(x,6, r),"mm"),
                  "9"= paste0(abbr(x,9, r),"tr"),
                  "10"= paste0(abbr(x,9, r),"tr"),
                  "11"= paste0(abbr(x,9, r),"tr"))
  return(temp)  
}


