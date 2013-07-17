#' Calculates the projected balance of a portfolio from a lognormal distribution
#'
#' @param start_amt numeric Starting balance
#' @param contribution numeric Monthly contribution amount
#' @param t Time horizon, expressed in months
#' @param ret vector Vector or Annual return, e.g. 5 percent, not 0.05
#' @param vol numeric Annual volatility of the portfolio
#' @param p numeric Percentile of cumulative return distribution to calculate
#' @return numeric Vector of account balances through time
#' @keywords finance economics forecasting
#' @seealso nothing
#' @export
#' @examples
#' logn_ptile_balance(0,100,12,6,12,0.5)
#' logn_ptile_balance(100,100,12,6,12,0.05)
#' sapply(seq(0.05,0.95,0.05),function(x) logn_ptile_balance(0,100,12,6,12,x))



logn_ptile_bal_varret<- function (start_amt, contribution, t, ret, vol, p) 
{
  tseq <- seq(1, t)
  if(length(ret)!=length(tseq)) { 
    print(paste0("Time horizon t and return sequence are not the same length. t=",length(tseq)," and ret=", length(ret)))
    break 
  }
  ret <- log((ret/100) + 1)/12  #Note that ret is now a *vector* of returns, which needs to be the same length as t
  #print(ret)
  vol <- (vol/100)
  
  p <- qnorm(p)
  
  print("Fix me.")
  # THIS ISN"T RIGHT. IT's multiplying the rets as if they've had them the entire time. 
  # Not sure there is a CFS for this. 
  
  
  balance <- (start_amt + contribution) * exp((ret * tseq) + p * vol * sqrt(tseq/12)) #Note that both start amt and first contribution have returns applied to them. 
  for (i in 2:(length(balance))) {
    this.tseq <- seq(1, (t-i+1)) #Create a vector of the right number of months. 
    this.ret<- ret[((i):length(ret))] #Grab the returns for that vector
    #print(paste0("length(this.tseq)= ",length(this.tseq), "and length(this.ret)= ",length(this.ret)))
    
    this.contr.value <- contribution * exp((this.ret * this.tseq) + 
                                             p * vol * sqrt(this.tseq/12))  #not sure if this additional contr is right. 
    this.contr.value <- c(rep(0, (i-1)), this.contr.value)
    #print(paste0("length(this.tseq) ",length(this.contr.value), " and length(balance)=  ",length(balance)))
    balance <- balance + this.contr.value
  }
  return(balance)
}

these.rets<- rep(1,12)
these.rets<- seq(0,2,length.out=12)

dollar(logn_ptile_bal_varret(100, 0, 12, these.rets, 0, .5))


warnings()