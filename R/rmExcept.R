#' Remove everything from the environment except X
#'
#' @param x A list of objects to not be removed, x= c("this","that)
#' @return Null - operates on the environment. 
#' @export
#' @examples
#' ls()
#' currentvars<-ls()
#' this <-seq(1:10)
#' that <- seq(2:20)
#' ls()
#' rmExcept(x=c(currentvars, "this"))  #Removes 'that'. 
#' ls()

rmExcept<- function(x) {
  rm(list=(ls()[-which(ls() %in% x)]))
}