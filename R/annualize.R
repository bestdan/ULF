#' @title Annualize, or actually change to any period, returns data. 
#' @author Daniel Egan
#' @description This function takes a return of a given formated input, and modifies the period it is expressed over. This does not only annualize, but can also monthly-ize, daily-ize, etc. .
#' @param ret Vector of returns
#' @param period At the data level, how many periods are there per year? For example, monthly data=12, trading days ~=252. 5 years=1/5
#' @param informat What format is the input returns in? For a 5 percent return, a = 0.05, b = 1.05, c = 5
#' @param outformat What format should the output returns be in? For a 5 percent return, a = 0.05, b = 1.05, c = 5
#' @return string name vector
#' @keywords finance, portfolio, annualize, convenience
#' @seealso \code{\link{gsub}} 
#' @export
#' @examples
#' annualize(.05,12,"a","a")
#' annualize(1.05,12,"b","b")
#' annualize(5,12,"c","c")
#' annualize(5,12,"c","a")
#' annualize(5,12,"c","b")


annualize<-function(ret,period=12,informat="a",outformat="c"){
  x<-switch(informat,
         a = (((ret+1)^period)-1)*100,
         b = (((ret)^period)-1)*100,
         c = (((ret/100+1)^period)-1)*100)
  y<-switch(outformat,
            a = x/100,
            b = (x/100)+1,
            c = x)
  return(y)
}

