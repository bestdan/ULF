#' Binds rows/columns of dataframes which do not have perfectly overlapping row/column names/sizes. 
#'
#' @param ... A set of data frames to be combined   
#' @return A dataframe
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' obs<-20
#' dfx<- data.frame(a=seq(.7,1.4,length.out=obs),b=runif(obs),c=rnorm(obs))  
#' dfy<- data.frame(x=seq(.7,1.4,length.out=obs),y=runif(obs),z=rnorm(obs))  
#' dfz<- rbind.data.frame.NA(dfx,dfy)
#' dfz


rbind.data.frame.NA<-function(...){
  N<-unique(unlist(lapply(list(...),names)))
  result<-NULL
  for(DF in list(...)){
    x<-as.data.frame(lapply(N,function(x)if(x %in% names(DF)) DF[,x]
                            else NA))
    names(x)<-N
    result<-rbind(result,x)
  }
  result
}

