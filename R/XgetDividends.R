#' @title Get prices & returns from XIgnite API
#' @author Daniel Egan
#' @param symbol The symbol to be queries
#' @param start_date The beginning of the historical date range
#' @param end_date The end of the historical date range. Defaults to today. 
#' @param YAMLfile A locally held yaml file with XIgnite Credentials. 
#' @param token Your API Token  
#' @param ShowURL Should the XIgnite API ULR be shown (helps with debugging)
#' @return A dataframe of date and dividends
#' @keywords finance portfolio annualize convenience
#' @import timeSeries yaml
#' @seealso Nothing. 
#' @export
#' @examples
#' # Note: No examples will work without a valid token. 
#' \dontrun{
#' data<-XgetDividends("AGG","2004-01-01", 
#'    YAMLfile = "~/Documents/Betterment/credentials//XIgniteCredentials.yaml")
#' head(data,20)
#' }


XgetDividends<-function(symbol,
                        start_date,end_date=as.character(Sys.Date()),
                        YAMLfile=NULL, token=NULL,
                        ShowURL=FALSE) {
  
  if(is.null(YAMLfile) & is.null(token)) {
    stop("Need to supply eitehr YAMLfile or token.")
  }
  
  # Grab credentials from YAML file if available
  if(!is.null(YAMLfile)) {
    temp<- yaml.load_file(YAMLfile)
    token<- temp$XIgniteToken
  }
  
  if (nchar(token) <5) {
    stop("You need to supply a valid token.")
  }
  
  start_date<-as.numeric(unlist(strsplit(start_date,"-")))
  start_date<-paste(start_date[2],start_date[3],start_date[1],sep="/")
  end_date<-as.numeric(unlist(strsplit(end_date,"-")))
  end_date<-paste(end_date[2],end_date[3],end_date[1],sep="/")
  
  # Note that date_format= "5/23/2000" 
  this.url<-paste0("http://www.xignite.com/xGlobalHistorical.csv/GetCashDividendHistory?Identifier=",symbol,
                   "&IdentifierType=Symbol&StartDate=",start_date,"&EndDate=",end_date,
                   "&_DownloadFile=true&_fields=Dividends.PayDate,Dividends.DividendAmount&_csvflatten=true")
  if(token!="NA") this.url<-paste0(this.url,"&_Token=",token)
  if (ShowURL==TRUE) print(this.url)
  result<- read.csv(this.url)
  
  
  names(result)<- c("Date", "Value")
  result <- aggregate(Value~Date, data=result, sum)
  result <- subset(result, Value!=0)  
  row.names(result) <- result[,1]
  names(result) <- c("Date",symbol)
  result <- as.timeSeries(result)
  return(result)
}
