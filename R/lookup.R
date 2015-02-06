#' @name lookup
#' @title Lookup-like functionality
#' @author Daniel Egan
#' @description Searches a data frame or matrix for a specific term, and returns all matching instances.
#' @param term The search term of the matrix
#' @param idata A dataframe or matrix to be searched. 
#' @param searchColumns Optional - if you only want to find the search term in one specific column, then use this. Accepts names or numbers. 
#' @param outputColumns Optional - returns all by default, but can be subsetted. 
#' @return All matching rows and columns.
#' @export
#' @examples
#' thisData<- data.frame(name=LETTERS[1:10], number=seq(1,10), othername=LETTERS[6:15], stringsAsFactors=FALSE)
#' thisData
#' lookup("F", thisData)
#' lookup("U", thisData, searchColumns="name")
#' lookup("U", thisData, searchColumns="othername")
#' lookup("U", thisData, searchColumns="othername", outputColumn="name")
#' lookup("U", thisData, searchColumns=3, outputColumn=1)


lookup<- function(term,idata,searchColumns="all",searchRows="all", outputColumns="all",outputRows="all") {
  
  if(searchRows !="all") {
    idata<- idata[searchRows,]
  }
  
  if(searchColumns !="all") {
    idata<- idata[,searchColumns]
  }
  
  matchingCells <- which(idata ==term, arr.ind = TRUE)
  
  
  if(outputColumns=="all") {
    outputColumns<- seq(1,ncol(idata))
  }
  
  if(outputRows=="all") {
    outputRows<- seq(1,nrow(idata))
  }
  
  idata <- idata[outputRows,outputColumns]
  
  idata<- idata[,matchingCells[,2] ]
  if(length(idata)<1) return(NULL)
  
  return(idata)
  
}

