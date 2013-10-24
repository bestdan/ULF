#' Remove everything from the environment except X
#'
#' @param vars A list of objects to not be removed, x= c("this","that)
#' @param envir What environment should they be removed from. 
#' @return Null - operates on the environment. 
#' @export
#' @examples
#' ls()
#' oldvar <- "c"
#' currentvars<-ls()
#' this <-seq(1:10)
#' that <- seq(2:20)
#' other <- c(2,4)
#' ls()
#' rmExcept(c(currentvars,"this"))  #Removes 'that'. 
#' ls()

rmExcept<-  function(vars, envir = .GlobalEnv) { 
  # vars <- c(vars, "dnrm") 
  keep <- match(x = vars, table = ls(envir = envir)) 
  if(any(is.na(keep))) { 
    stop(paste("Some of the variables were not found in", 
               environmentName(envir))) 
  } 
  rm(list = ls(envir = envir)[-keep], envir = envir) 
  # cat("Removed all but", length(keep), "objects from", 
  #    environmentName(envir), fill = TRUE) 
} 


 