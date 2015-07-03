#' @name logMean
#' @title Finds the mean in log-space
#' @description Takes the data, logs it according to the 'base' input, finds the mean, and raises 'base' to this mean. 
#' @author Daniel Egan
#' @param ldata A vector of data to take the mean of. 
#' @param base What base should the logged data be based on? Defaults to 10. 
#' @return The log-mean
#' @export
#' @examples
#'  rdata<- 10^rnorm(30, mean=4,sd=3)
#' #' Note that means based on skew variables are unreliable. 
#' format(round(as.numeric(mean(rdata)),0),digits=20,big.mark = ",")
#' #' LogMeans obey CLT. 
#' format(round(logMean(rdata),0),big.mark = ",")


logMean<- function(ldata, base=10){
  ldata<- pmax(ldata,1)
  ldata<- log(ldata, base)
  thisMean<- mean(ldata)
  result<- base^thisMean
  return(result)
}
