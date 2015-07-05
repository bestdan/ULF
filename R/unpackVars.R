#' @title Unpack all vars into the environment. 
#' @description Gathers all variables in the current environment an makes a list of them. Often used
#' in conjunction with \code{\link{unpackVars}} to pass arguments back and forth.
#' @author Daniel Egan
#' @param vars Optional. A list of variables to be unpacked.
#' @param varlist Optional
#' @return Nothing. Changes the environment above.
#' @export
#' @examples
#' x <- 5
#' y <- 10
#' z <- 15
#' 
#' temp<- gatherVars()
#' adder<- function(stuff){
#'  unpackVars(stuff, varlist=c('x', 'y'))
#'  if(!exists('z', inherits=FALSE )) z<- 100
#'  #' this shows that Z was not unpacked. 
#'  x+y+z
#' }
#' 
#' adder(temp)
#' 

unpackVars<- function(vars, varlist=NULL){
  
  for(i in 1:length(vars)) {
    if(!is.null(varlist)) {
      if(names(vars[i]) %in% varlist == FALSE) next  
    }
    assign(names(vars[i]), value=unlist(vars[i][1]), pos = 1)  
  }
  
}

