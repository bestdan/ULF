#' Calculate the future value of savings 
#'
#' @param rate numeric - Annual percentage return, expressed as 5, e.g.
#' @param deposit numeric - the deposit amount
#' @param term numeric - how long you'll be depositing for, in years
#' @param frequency string - "annual" or "monthly"
#' @return string Vector of formatted strings
#' @keywords finance economics formating 
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' fvSaveInvest(7,1200,3,"annual")
#' fvSaveInvest(7,100,3,"monthly")

fvSaveInvest<-function(rate,deposit,term,frequency="annual"){
  rate<-(1+(rate/100))
  #  Annual 
  if (frequency=="annual") {    
    endval<-deposit*(
      (rate^(term+1)-1) / (rate-1)) -deposit
  }
  # Monthly
  if (frequency=="monthly") {    
    rate<-rate^(1/12) # monthly interest rate
    term<- term*12    # monthly term
    endval<-deposit*(
      (rate^(term+1)-1) / (rate-1)) - deposit
  }
  return(endval)
}

