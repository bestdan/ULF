#' @title de-Null the arguments to a parent function. 
#' @description This function looks at the parent or calling function, and checks if any arguments that have
#' defaults are currently null. If they are, it replaces the NULL with the default. 
#' @author Daniel Egan
#' @return Nothing, modifies parent environment
#' @export
#' @examples
#' checkFun<- function(x=5){
#' deNullArgs()
#' print(x)
#' }
#' 
#' checkFun(x=NULL)


deNullArgs<- function(){
  
  #Don't have time to check for all concistency here, but would be interesting to completely write
  thisFun<- as.character( sys.call(-1)[[1]] )
  
    theseArgs<- formals(thisFun)
      argNames<- names(theseArgs)
      for(argName in argNames){
        
        print(argName)
      }
  
}
  
