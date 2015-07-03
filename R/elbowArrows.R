#' @name elbowArrows
#' @title Arrows with Elbows
#' @description Puts an arrow with a 90* elbow on a graph
#' @author Daniel Egan
#' @param x0 The source x point
#' @param x1 The target x point
#' @param y0 The source y point
#' @param y1 The target y point
#' @param first Should the elbow be based on running the y-axis change first, or the x-axis change. 
#' @param ... other graphing parameters. 
#' @return A new object with updated names 
#' @export
#' @examples
#' plot(seq(1,10))
#' elbowArrows(2,6,2,6, first='y', code=2, col="blue", lwd=4)
#' elbowArrows(2,6,2,6, first='x', code=2, col="red", lwd=4)


elbowArrows<- function(x0, x1, y0, y1, first='y', ...){
  
  if(first=='y') {
    #' In this case, we move through the y-change first. 
    lines(c(as.numeric(x0), as.numeric(x0)), c(as.numeric(y0),as.numeric(y1)), ...)
    arrows(x0=x0, x1 = x1, y0=y1, y1=y1,  ...)
  } else {
    lines(c(x0, x1), c(y0,y0), ...)
    #arrows(x0=x0, x1 = x1, y0=y0, y1=y0, code=0, ...)
    arrows(x0=x1, x1 = x1, y0=y0, y1=y1, ...)
  }
  
}

