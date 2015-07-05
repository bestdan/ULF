#' @title Gather all variables in the environment. 
#' @description Gathers all variables in the current environment an makes a list of them. Often used
#' in conjunction with \code{\link{unpackVars}} to pass arguments back and forth.
#' @author Daniel Egan
#' @param vars Optional. A set of specific vars you want to pass through. 
#' @return A list containing all the data. 
#' @export
#' @examples
#' x <- 5
#' y <- 10
#' 
#' temp<- gatherVars()
#' temp

gatherVars<- function(vars=NULL){
  #print(ls())
  #require(ULF)
  vnames<- ls(pos = 1)
  for(v in vnames){
    if(class(get(v))=='function' ) 
      vnames<- vnames[-which(vnames == v)]
  }
  
  #Filter out undesired vars
  if(!is.null(vars)) { 
    for(v in vnames){
      if(v %in% vars==FALSE) 
        vnames<- vnames[-which(vnames == v)]
    }
  }
  
  allTheThings <- lapply(vnames, function(x) get(x, pos = 1))
  names(allTheThings)<- vnames
  
  return(allTheThings)
}
