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
#' lookup("K", thisData, searchColumns="name")
#' lookup("K", thisData, searchColumns="othername")
#' lookup("K", thisData, searchColumns="othername", outputColumn="name")
#' lookup("K", thisData, searchColumns=3, outputColumn=1)


lookup<- function(term,idata,searchColumns="all",searchRows="all", outputColumns="all",outputRows="all") {
  
  if(searchRows !="all") {
    idata<- idata[searchRows,]
  }
  
  if(searchColumns =="all") {
    searchColumns<- seq(1,ncol(idata))
  }
  
  matchingCells <- which(idata[,searchColumns] ==term, arr.ind = TRUE)
  
  if(length(matchingCells)<1) {
    return(NULL)
  }
  
  if(is.null(dim(matchingCells))) {
    idata<- idata[matchingCells[1],]  
  } else {
    idata<- idata[matchingCells[,1],]  
  }
  
  
  if(outputColumns=="all") {
    outputColumns<- seq(1,ncol(idata))
  }
  
  idata <- idata[,outputColumns]
  
  if(length(idata)<1) return(NULL)
  
  return(idata)
  
}

