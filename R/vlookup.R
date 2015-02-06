#' @name vlookup
#' @title Vlookup-like functionality
#' @author Daniel Egan
#' @description Searches a data frame or matrix for a specific term, and returns all matching instances.
#' @param term The search term of the matrix
#' @param idata A dataframe or matrix to be searched. 
#' @param searchColumns Optional - if you only want to find the search term in one specific column, then use this. Accepts names or numbers. 
#' @param outputColumns Optional - returns all by default, but can be subsetted. 
#' @return All matching rows and columns.
#' @export
#' @examples
#' thisData<- data.frame(name=LETTERS[1:10], number=seq(1,10), othername=LETTERS[24:15], stringsAsFactors=FALSE)
#' thisData
#' vlookup("U", thisData)
#' vlookup("U", thisData, searchColumns="name")
#' vlookup("U", thisData, searchColumns="othername")
#' vlookup("U", thisData, searchColumns="othername", outputColumn="name")
#' vlookup("U", thisData, searchColumns=3, outputColumn=1)


vlookup<- function(term,idata,searchColumns="all", outputColumns=NULL) {
  
  rows<- ifelse(searchColumns=="all", 
                which(idata ==term, arr.ind = TRUE)[1],
                which(idata[,searchColumns] ==term, arr.ind = TRUE)[1])
  
  if(is.null(outputColumns)) {
    outputColumns<- seq(1,ncol(idata))
  }
  
  return(idata[rows,outputColumns])
  
}

