#' @name replaceName
#' @title Replace object name
#' @description Replaces the object \code{oldName} with \code{newName}
#' @author Daniel Egan
#' @param df The object with names to be replaced
#' @param oldName The old name in the object
#' @param newName The replacement name
#' @return A new object with updated names 
#' @export
#' @keywords BMTFxns tax lots 
#' @examples
#' thisDF<- data.frame("A" = c(1,2,3), "B"=(3,4,5))
#' replaceName(thisDF, "A","C")


replaceName<- function(df, oldName, newName) {
  
  if(oldName %in% names(df) == FALSE) {
    warning(paste("Could not find",oldName,"in object names"))
    return(df)
  }
  
  names(df)[names(df)==oldName]<- newName
  return(df)
}
