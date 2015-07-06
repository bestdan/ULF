#' @title Gather all variables in the environment. 
#' @description Gathers all variables in the current environment an makes a list of them. Often used
#' in conjunction with \code{\link{unpackVars}} to pass arguments back and forth.
#' @author Daniel Egan
#' @param vars Optional. A set of specific vars you want to pass through. 
#' @return A list containing all the data. 
#' @export
#' @examples
#' x <- data.frame(x=rep(5,5))
#' y <- 10
#' 
#' temp<- gatherVars()
#' temp
#' 
#' tryThis<- function(){
#'   a<- data.frame(x=c(1,2,3))
#'   b<- 2
#'   temp<- gatherVars()
#'   return(temp)
#' }
#' tryThis()

gatherVars<- function(vars=NULL){
  #print(ls())
  #require(ULF)
  vnames<- ls(envir = parent.frame())
  
  #Filter out undesired vars
  if(!is.null(vars)) { 
    for(v in vnames){
      if(v %in% vars==FALSE) 
        vnames<- vnames[-which(vnames == v)]
    }
  }
  
  allTheThings <- list()
  
  for(v in vnames) {
    thisThing<- get(v, envir = parent.frame())
    allTheThings<- append(allTheThings, list(thisThing))

  }
  
  
  #allTheThings<- lapply(vnames, function(x) get(x, envir = parent.frame()))
  names(allTheThings)<- vnames
  
  return(allTheThings)
}
