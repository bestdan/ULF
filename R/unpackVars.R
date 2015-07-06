#' @title Unpack all vars into the environment. 
#' @description Gathers all variables in the current environment an makes a list of them. Often used
#' in conjunction with \code{\link{unpackVars}} to pass arguments back and forth.
#' @author Daniel Egan
#' @param vars Optional. A list of variables to be unpacked.
#' @param keep Optional - a vector of names of vars to \strong{keep}. Cannot be specified with 'exclude'. 
#' @param exclude Optional - a vector of names of vars to \strong{exclude}. Cannot be specified with 'keep'. 
#' @return Nothing. Changes the environment above.
#' @export
#' @examples
#' x <- 5
#' y <- 10
#' z <- 15
#' 
#' temp<- gatherVars()
#' adder<- function(stuff){
#'  unpackVars(stuff, keep=c('x', 'y'))
#'  if(!exists('z', inherits=FALSE )) z<- 100
#'  #' this shows that Z was not unpacked. 
#'  x+y+z
#' }
#' 
#' adder(temp)
#' 

unpackVars<- function(vars, keep=NULL, exclude=NULL){
  
  if(!is.null(keep) & !is.null(exclude)) stop("Cannot specify both 'varlist' and 'exclude'")
  
  for(i in 1:length(vars)) {
    if(!is.null(keep)) {
      if(names(vars[i]) %in% keep == FALSE) next  
    }
    
    if(!is.null(exclude)) {
      if(names(vars[i]) %in% exclude == TRUE) next  
    }

    assign(names(vars)[[i]], value=vars[[i]], envir = parent.frame())  
  }
  
}
