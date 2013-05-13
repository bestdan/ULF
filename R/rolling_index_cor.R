#' Rolling correlation of a bunch of vars to an index.
#' @description Calculates rolling correlations over the specified window, where the first column is the index. 
#' @param rets Vector of data for the correlation to be calculated over. The first column should be the index. 
#' @param window The number of observations for data to be observed over. 
#' @return A matrix of correlations
#' @keywords finance, portfolio, annualize, convenience
#' @seealso Nothing. 
#' @export
#' @examples
#' obs<-20
#' rets<-data.frame(a=seq(.7,1.4,length.out=obs),b=runif(obs),c=rnorm(obs))  
#' rets$y<-sin(rets$a+rets$b)
#' rolling_cor_to_index(rets,2)
#' these.cors<-cbind(seq(1,obs),rolling_cor_to_index(rets,2))
#' plot_lines(these.cors)

rolling_cor_to_index<-function(rets,window){
  this.len<-nrow(rets) #number of ret observations
  corvec<-data.frame(matrix(NA,ncol=ncol(rets),nrow=nrow(rets))) #holder for output
  names(corvec)<-names(rets)
  for (i in this.len:(window+1)) {   #decrement from number of rets to window
    these.rets<-rets[(i-window):i,]
    these.cor<-cor(these.rets,use="pairwise.complete")
    corvec[i,]<-these.cor[,1]
  }
  return(corvec)
}
