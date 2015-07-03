#' Named list
#' @description Returns a named list automatically, rather than losing the names like 'list' does. 
#' @author Daniel Egan
#' @references llist from HMisc. This is a 100% copy. 
#' @param ... The variables to be returned
#' @param labels TRUE by default. 
#' @return List, consisting of the named version of variables. 
#' @keywords tax capitalgains tlh
#' @examples
#' a <- 1:3 
#' b <- 4:6
#' label(b) <- 'B Label'
#' llist(a,b)
#' llist(a,b,d=0)
#' llist(a,b,0)

nlist<- function (..., labels = TRUE) {
  dotlist <- list(...)
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for (i in 1:length(dotlist)) {
    vname[i] <- if (length(lname) && lname[i] != "") 
      lname[i]
    else name[i]
    lab <- vname[i]
    if (labels) {
      lab <- attr(dotlist[[i]], "label")
      if (length(lab) == 0) 
        lab <- vname[i]
    }
    label(dotlist[[i]]) <- lab
  }
  names(dotlist) <- vname[1:length(dotlist)]
  return(dotlist)
}
