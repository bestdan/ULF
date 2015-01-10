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
#' @return A dataframe of date and values
#' @keywords XIgnite 
#' @export
#' @import timeSeries yaml
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
  
  # Validate AdjType
  if (adjtype %in% c("None","SplitOnly",
                     "CashDividendOnly","SplitAndProportionalCashDividend", "SplitAndCashDividend","All")==FALSE) {
    stop("Incorrect adjtype input. Fail.")
  }
  
  
  if (quotetype %in% c("Last","Open","High","Low","Volume","LastClose","ChangeFromOpen",
                     "PercentChangeFromOpen","ChangeFromLastClose","PercentChangeFromLastClose")==FALSE) {
    stop("Incorrect quotetype input. Fail.")
    
  }
  
  start_date<- as.numeric(unlist(strsplit(start_date,"-")))
  start_date<- paste(start_date[2],start_date[3],start_date[1],sep="/")
  end_date<- as.numeric(unlist(strsplit(end_date,"-")))
  end_date<- paste(end_date[2],end_date[3],end_date[1],sep="/")
  
  # Note that date_format= "5/23/2000" 
  this.url<-paste0("http://www.xignite.com/xGlobalHistorical.csv/GetGlobalHistoricalQuotesRange?Identifier=",symbol,
                   "&IdentifierType=Symbol&AdjustmentMethod=",adjtype,"&StartDate=",start_date,"&EndDate=",end_date,
                   "&_DownloadFile=true&_fields=GlobalQuotes.Date,","GlobalQuotes.",quotetype,"&_csvflatten=true")
  if(token!="NA") this.url<-paste0(this.url,"&_Token=",token)
  
  if (ShowURL==TRUE) print(this.url)
  
  result<-read.csv(this.url)
  
  names(result)<-c("Date","Value")
  
  result<-subset(result,Value!=0)
  row.names(result)<-result[,1]
  names(result)<-c("Date",symbol)
  result<-as.timeSeries(result)
  
  return(result)
}
