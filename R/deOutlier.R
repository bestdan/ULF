#' Remove outliers in data
#' @description Removes or NA's observations which are a factor greater than the MAD
#' @param x Input data
#' @param maxMADRatio The maximum allowable deviation, as a factor of the median absolute deviation. Defaults to 200x of the MAD. 
#' @param action What should happen to outliers? Either 'NA' or 'remove', which subsets. 
#' @return value A version of X with all olds replaced by news
#' @keywords color alpha 
#' @seealso \code{\link{is.character}} which this function wraps
#' @export
#' @examples
#' x<- c(rnorm(100), 1000, 2000)
#' y<- deOutlier(x)
#' setdiff(y,x)

deOutlier<- function(x, maxMADRatio=200, action="NA") {
  if ( (action %in% c("NA","remove"))==FALSE) stop("Please use 'NA' or 'remove' for action")
  # How far away from the average is it? 
  deviation<- abs( (x-median(x))/mad(x))
  
  if (action=="NA") {
    y<- x
    y[deviation > maxMADRatio]<-NA
  } else {
    if (action=="remove") y<- x[deviation < maxMADRatio]
  }
  return(y)
}


