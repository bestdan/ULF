#' @title Get prices & returns from XIgnite API
#' @author Daniel Egan
#' @param symbol The symbol to be queries
#' @param start_date The beginning of the historical date range
#' @param end_date The end of the historical date range. Defaults to today. 
#' @param adjtype The type of adjustment, if any, to be made. Potentially
#' \itemize{
#'   \item{None}
#'   \item{SplitOnly}
#'   \item{CashDividendOnly}
#'   \item{SplitAndProportionalCashDividend}
#'   \item{SplitAndProportionalCashDividend}
#'   \item{SplitAndCashDividend}
#'   \item{All}
#' }
#' @param quotetype The type of data to be returns. Options are: 
#' \itemize{
#'   \item{Last}
#'   \item{Open}
#'   \item{High}
#'   \item{Low}
#'   \item{Volume}
#'   \item{Close (the default)}
#'   \item{PercentChangeFromOpen}
#'   \item{ChangeFromLastClose}
#'   \item{PercentChangeFromLastClose}
#' }
#' @param YAMLfile A locally held yaml file with XIgnite Credentials. 
#' @param token Your XIgnite API Token  
#' @param ShowURL Should the XIgnite API ULR be shown (helps with debugging)
#' @return An xts object of date and values
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @import xts yaml
#' @examples
#' # Note: Examples will no work without a valid XIgnite token. 
#' \dontrun{
#' data<-XgetSymbols("EEM", start_date="2013-01-01", adjtype="SplitOnly", 
#'    YAMLfile = "~/Documents/Betterment/credentials/XIgniteCredentials.yaml")
#' head(data,20)
#' }


XgetSymbols<-function(symbol,
                      start_date,end_date=as.character(Sys.Date()),
                      adjtype,
                      quotetype="LastClose",
                      YAMLfile=NULL,
                      token=NULL, ShowURL=FALSE) {

  if(is.null(YAMLfile) & is.null(token)) {
    stop("Need to supply eitehr YAMLfile or token.")
  }
  
  # Grab credentials from YAML file if available
  if(!is.null(YAMLfile)) {
    temp<- yaml.load_file(YAMLfile)
    token<- temp$XIgniteToken
  }
   
  # Validate token
  if (nchar(token) <5) {
    stop("Token does not appear to be valid.")
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
  if (ShowURL == TRUE) {
    print(this.url)
  }
  
  # Execute request query, persist results
  result <- as.xts(read.zoo(file = this.url, format = "%m/%d/%Y", sep = ",", header = T))

  # Format results
  names(result) <- symbol
  
  return(result)
}
