#' Get prices & returns from XIgnite API
#'
#' @param symbol The symbol to be queries
#' @param start_date The beginning of the historical date range
#' @param end_date The end of the historical date range. Defaults to today. 
#' @param token Your API Token  
#' @return A dataframe of date and dividends
#' @keywords finance, portfolio, annualize, convenience
#' @import timeSeries
#' @seealso Nothing. 
#' @export
#' @examples
#' data<-XgetDividends("AGG","2004-01-01")
#' head(data,20)

XgetDividends<-function(symbol,start_date,end_date=as.character(Sys.Date()),token="NA") {
  start_date<-as.numeric(unlist(strsplit(start_date,"-")))
  start_date<-paste(start_date[2],start_date[3],start_date[1],sep="/")
  end_date<-as.numeric(unlist(strsplit(end_date,"-")))
  end_date<-paste(end_date[2],end_date[3],end_date[1],sep="/")
  
  # Note that date_format= "5/23/2000" 
  this.url<-paste0("http://www.xignite.com/xGlobalHistorical.csv/GetCashDividendHistory?Identifier=",symbol,
                   "&IdentifierType=Symbol&StartDate=",start_date,"&EndDate=",end_date,
                   "&_DownloadFile=true&_fields=Dividends.PayDate,Dividends.DividendAmount&_csvflatten=true")
  if(token!="NA") paste0(this.url,"&_Token=",token)
  print(this.url)
  result<-read.csv(this.url)
  names(result)<-c("Date","Value")
  result<-aggregate(Value~Date,result,sum)
  result<-subset(result,Value!=0)  
  row.names(result)<-result[,1]
  names(result)<-c("Date",symbol)
  result<-as.timeSeries(result)
  return(result)
}
