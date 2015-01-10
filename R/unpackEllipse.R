#' @title Unpack arguments from ellipse
#' @description A convenience function for unpacking the contents of ellipses in a function input. Writes the ellipses to the
#' higher environment. 
#' @param params An ellipse style parameters
#' @return contents of ellipses as objects to higher environment. 
#' @export
#' @examples 
#' print("hello")

unpackEllipse <- function(params){
     #params <- as.list(substitute(list(x)))[-1L]
     if(length(params)>0) {
     for (i in 1:length(params)) {
       assign(names(params)[i],value=params[i])
     }
   }
}

#   testFun<- function(input,...) {
#   params <- as.list(substitute(list(x)))[-1L]
#   things<- unpackEllipse(params)
#   print(things)
#   }
#   testFun("hi",hello="hello",there="there")
# 
# for (file in files) {
#   source(paste0("R//",file))
# }
