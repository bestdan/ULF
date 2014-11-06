#' Get prices & returns from XIgnite API
#'
#' @param symbol The symbol to be queries
#' @param start_date The beginning of the historical date range
#' @param end_date The end of the historical date range. Defaults to today. 
#' @param token Your API Token  
#' @param ShowURL Should the XIgnite API ULR be shown (helps with debugging)
#' @return An xts object of date and dividends
#' @keywords finance portfolio annualize convenience
#' @import xts
#' @seealso Nothing. 
#' @export
#' @examples
#' # Note: No examples will work without a valid token. 
#' # data<-XgetDividends("AGG","2004-01-01")
#' # head(data,20)

XgetDividends <- function(symbol, start_date, end_date = as.character(Sys.Date()), token = "NA", ShowURL = FALSE) {
  
  library(xts)
  
  # Check for xignite token existence
  if (nchar(token) < 5) {
    stop("You need to supply a valid token.")
  }
  
  # Convert dates to xignite ready format
  start_date <- as.character(as.Date(start_date, format = '%Y-%m-%d'), format = "%m/%d/%Y")
  end_date   <- as.character(as.Date(end_date, format = '%Y-%m-%d'), format = "%m/%d/%Y")
  
  # Build the request string
  # Note that date_format = "5/23/2000"
  this.url <- paste0("http://www.xignite.com/xGlobalHistorical.csv/GetCashDividendHistory?Identifier=", symbol,
                   "&IdentifierType=Symbol&StartDate=", start_date, "&EndDate=", end_date,
                   "&_DownloadFile=true&_fields=Dividends.PayDate,Dividends.DividendAmount&_csvflatten=true")
  
  # Check for a valid token, add it to the request string
  if(token != "NA") {
    this.url <- paste0(this.url, "&_Token=", token)
  }
  
  # Debug element for request URL
  if (ShowURL == TRUE) {
    print(this.url)
  }
  
  # Execute request query, persist results
  result <- as.xts(read.zoo(file = this.url, format = "%m/%d/%Y", sep = ",", header = T))
  
  # Format results
  names(result) <- symbol
  
  return(result)
}
