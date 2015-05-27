#' @name lookup
#' @title Lookup-like functionality
#' @author Daniel Egan
#' @description Searches a data frame or matrix for a specific term, and returns all matching instances.
#' @param term The search term of the matrix
#' @param idata A dataframe or matrix to be searched. 
#' @param searchColumns Optional - if you only want to find the search term in one specific column, then use this. Accepts names or numbers. 
#' @param searchRows Optionan - If you want to search only specific rows. 
#' @param outputColumns Optional - returns all by default, but can be subsetted. 
#' @param outputRows Optional - returns all by default, but can be subsetted. 
#' @return All matching rows and columns.
#' @export
#' @examples
#' thisData<- data.frame(name=LETTERS[1:10], number=seq(1,10), othername=LETTERS[6:15], 
#'   dates=seq(from=as.Date("2000-01-01"), by=1, length.out=10), stringsAsFactors=FALSE)
#' str(thisData)
#' thisData
#' lookup("F", thisData)
#' lookup("K", thisData, searchColumns="name")
#' lookup("K", thisData, searchColumns="othername")
#' lookup("K", thisData, searchColumns="othername", outputColumn="name")
#' lookup("K", thisData, searchColumns=3, outputColumn=1)
#' lookup(as.Date("2000-01-01"), thisData)


lookup<- function(term, idata, 
                  searchColumns="all", searchRows="all", 
                  outputColumns="all", outputRows="all") {
  
  if(class(term)=="Date") {
    term<- as.character(term)
  }
  
  classes<- unlist(lapply(idata,class))
  
  for(var in names(classes)[classes=="Date"]){
    idata[,var]<- as.character(idata[,var])
  }
  
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



