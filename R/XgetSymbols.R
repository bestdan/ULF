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
#' @return A dataframe of date and values
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @import timeSeries 
#' @examples
#' # Note: no examples will work without a token. 
#' # data<-XgetSymbols("EEM", start_date="2013-01-01", adjtype="SplitOnly")
#' # head(data,20)



XgetSymbols<-function(symbol,start_date,end_date=as.character(Sys.Date()),adjtype,quotetype="LastClose",token="NA", ShowURL=FALSE) {
  require(timeSeries)
  if (length(token) <5) {
    stop("You need to supply a valid token.")
  }
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
  #result<-subset(result,select=c(-X))
  names(result)<-c("Date","Value")
  result<-subset(result,Value!=0)
  row.names(result)<-result[,1]
  names(result)<-c("Date",symbol)
  result<-as.timeSeries(result)
  return(result)
}
