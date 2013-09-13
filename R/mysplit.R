#' Faster version of split
#'
#' @param x vector or data frame containing values to be divided into groups.
#' @param f a ‘factor’ in the sense that 
#' as.factor(f) defines the grouping, or a list of such factors 
#' in which case their interaction is used for the grouping.
#' @param drop logical indicating if levels that do not occur should be dropped (if f is a factor or a list).
#' @param ... further potential arguments passed to methods.
#' @return The value returned from split is a list of vectors containing the values for the groups. 
#' The components of the list are named by the levels of f (after converting to a factor, or if 
#' already a factor and drop = TRUE, dropping unused levels).
#' The replacement forms return their right hand side. unsplit returns a vector or data frame for which split(x, f) equals value
#' @keywords split 
#' @seealso None
#' @export
#' @examples
#' x <- data.frame(a = 1:10, b = rep(1:2, each=5))
#' splitx <- mysplit(x, x$b)
#' 
mysplit <- function(x, f, drop = FALSE, ...){ 
  f <- as.factor(f) 
  tmp <- lapply(x, function(xi) split(xi, f, drop = drop, ...)) 
  rn <- split(rownames(x), f, drop = drop, ...) 
  tmp <- unlist(unname(tmp), recursive = FALSE) 
  tmp <- split(tmp, factor(names(tmp), levels = unique(names(tmp)))) 
  tmp <- lapply(setNames(seq_along(tmp), names(tmp)), function(i) { 
    t <- tmp[[i]] 
    names(t) <- names(x) 
    attr(t, "row.names") <- rn[[i]] 
    class(t) <- "data.frame" 
    t 
  }) 
  tmp 
} 