#' @title de-Null the arguments to a parent function. 
#' @description This function looks at the parent or calling function, and checks if any arguments that have
#' defaults are currently null. If they are, it replaces the NULL with the default. 
#' @param exclude A set of variables you want to keep \code{NULL}. 
#' @author Daniel Egan
#' @return Nothing, modifies parent environment
#' @export
#' @examples
#' checkFun<- function(x=data.frame(x=5)){
#'  deNullArgs() 
#'  print(x)
#' }
#' 
#' checkFun()
#' checkFun(x=NULL)
#' checkFun(x=10)


deNullArgs<- function(exclude=NULL){
  
  #Don't have time to check for all concistency here, but would be interesting to completely write
  thisFun<- as.character( sys.call(-1)[[1]] )
  
  theseArgs<- formals(thisFun)
  
  argNames<- names(theseArgs)
  
  if(!is.null(exclude)) {
    argNames <- argNames[-which(argNames %in% exclude)]    
  }
  
  for(argName in argNames){
    #' Skip if not null. 
    if(!is.null(get(argName, envir = parent.frame()))) next

    thisDefault <- theseArgs[[argName]]

    assign(argName, eval(thisDefault), envir = parent.frame())
    
  }
  
}

