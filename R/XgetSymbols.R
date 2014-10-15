#' Get prices & returns from XIgnite API
#'
#' @param symbol The symbol to be queries
#' @param start_date The beginning of the historical date range
#' @param end_date The end of the historical date range. Defaults to today. 
#' @param adjtype The type of adjustment, if any, to be made. Potentially
#' "None",
#' "SplitOnly",
#' "CashDividendOnly",
#' "SplitAndProportionalCashDividend",
#' "SplitAndCashDividend",
#' "All"
#' @param quotetype The type of data to be returns. Options are
#' Last, 
#' Open,
#' High,
#' Low,
#' Volume,
#' LastClose (the default),
#' ChangeFromOpen,
#' PercentChangeFromOpen,
#' ChangeFromLastClose,
#' PercentChangeFromLastClose,
#' @param token Your XIgnite API Token  
#' @param ShowURL Should the XIgnite API ULR be shown (helps with debugging)
#' @return An xts object of date and values
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @import xts 
#' @examples
#' # Note: no examples will work without a token. 
#' # data<-XgetSymbols("EEM", start_date="2013-01-01", adjtype="SplitOnly")
#' # head(data,20)



XgetSymbols <- function(symbol, start_date, end_date = as.character(Sys.Date()), adjtype, quotetype = "LastClose", token = "NA", ShowURL = FALSE) {
  library(xts)
  
  # Check for xignite token existence
  if (nchar(token) <5) {
    stop("You need to supply a valid token.")
  }
  
  # Check for adjustment type
  if (adjtype %in% c("None","SplitOnly", "CashDividendOnly", "SplitAndProportionalCashDividend", "SplitAndCashDividend", "All") == FALSE) {
    stop("Incorrect adjtype input. Fail.")
  }
  
  # Check for ticker quote type
  if (quotetype %in% c("Last", "Open", "High", "Low", "Volume", "LastClose", "ChangeFromOpen", "PercentChangeFromOpen", "ChangeFromLastClose", "PercentChangeFromLastClose") == FALSE) {
    stop("Incorrect quotetype input. Fail.")
  }
  
  # Convert dates to xignite ready format
  start_date <- as.character(as.Date(start_date, format = '%Y-%m-%d'), format = "%m/%d/%Y")
  end_date   <- as.character(as.Date(end_date, format = '%Y-%m-%d'), format = "%m/%d/%Y")
  
  # Build the request string
  # Note that date_format = "5/23/2000" 
  this.url <- paste0("http://www.xignite.com/xGlobalHistorical.csv/GetGlobalHistoricalQuotesRange?Identifier=", symbol,
                   "&IdentifierType=Symbol&AdjustmentMethod=", adjtype, "&StartDate=", start_date, "&EndDate=", end_date,
                   "&_DownloadFile=true&_fields=GlobalQuotes.Date,", "GlobalQuotes.", quotetype, "&_csvflatten=true")
  
  # Check for a valid token, add it to the request string
  if(token != "NA") {
    this.url <- paste0(this.url, "&_Token=", token)
  }
  
  # Debug element for request URL
  if (ShowURL==TRUE) {
    print(this.url)
  }
  
  # Execute request query, persist results
  result <- as.xts(read.zoo(file = this.url, format = "%m/%d/%Y", sep = ",", header = T))

  # Format results
  names(result) <- symbol
  
  return(result)
}
