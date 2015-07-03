#' @title Get Index Constituents from XIgnite
#' @param symbol The index symbol to be use. See \url{http://www.xignite.com/product/XigniteIndexComponents/api/legacy/v1/}
#' @param YAMLfile A local YAML formatted file which has your XIgnite credentials
#' @param token Your XIgnite API Token, if you want to specify at command line. 
#' @param ShowURL Should the XIgnite API ULR be shown (helps with debugging)
#' @return A dataframe of date and values
#' @export
#' @importFrom yaml yaml.load_file
#' @examples
#' # Note: no examples will work without a token. 
#' \dontrun{
#' temp<- XgetIndexConstituents("^DJI", 
#'    YAMLfile = "~/Documents/Betterment/credentials//XIgniteCredentials.yaml")
#' head(temp)
#' }
#' 

XgetIndexConstituents<-function(symbol,
                                YAMLfile=NULL,token="NA", 
                                ShowURL=FALSE) {
  
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
  
  
  this.url<-paste0("http://www.xignite.com/xIndexComponents.xml/GetIndexComponents?IdentifierType=Symbol&Identifier=",
                   symbol, 
                   "&_DownloadFile=true")
  
  #' Add on credentials
  this.url<-paste0(this.url,"&_Token=",token)
  
  if (ShowURL==TRUE) print(this.url)
  result<-read.csv(this.url)
    
  return(result)
}
