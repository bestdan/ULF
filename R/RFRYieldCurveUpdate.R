#' Modifies expected returns to reflect the risk-free-rate yield curve over time
#'
#' @section Note: This takes a mean expected return from an AA frontier, and a functional specification of a yield curve, and returns the forward-looking mean return at various time horizons. 
#' @param ret An mean expected return
#' @param b1 The first coefficient of the yield curve expressed in months
#' @param b2 The second (curve) coefficient of the yield curve expressed in months
#' @return numeric A vector of month-based mean expected returns including the expected risk-free-variation
#' @keywords finance economics forecasting
#' @seealso nothing
#' @export
#' @examples
#' temp<-RFRYieldCurveUpdate(1.5,0.0003,-.0000005)
#' plot(temp)




RFRYieldCurveUpdate<- function (ret, b1, b2) {
  months<-seq(1,360)
  rets<- ret/100 + b1*months + b2*(months^2)
  rets<-cbind(months,rets*100)
  return(rets)
}
