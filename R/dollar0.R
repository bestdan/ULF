#' @title Format numbers as dollars with no cents
#' @author Daniel Egan
#' @param x numeric vector to be converted to dollars
#' @return string Vector of formatted strings
#' @export
#' @examples
#' dollar0(10^seq(0,4, .5))
#' dollar0(-10^seq(0,4, .5))


dollar0<-function(x){
  x<- round(as.numeric(x), 0)    
  neg <- ifelse(sign(x)<0, "-", "")
  
  x<- abs(x)

  y<- paste(neg,"$", formatC(x,digits = 0,format = "f",big.mark = ",") ,sep="")

    return(y)
}
