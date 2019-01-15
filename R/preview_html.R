#' @author Daniel Egan
#' @name preview_html
#' @title preview_html
#' @description Pushes unrendered output to the RStudio preview pane
#' @param x The object you wish to render in html
#' @export
#' @examples
#' library(DT)
#' x <- data.frame(alpha  = LETTERS,
#'      nums = round(runif(n = length(LETTERS)),2))
#' z <- DT::datatable(x)
#' preview_html(z)

preview_html <- function(x){
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  sink(file = htmlFile)
  print(x)
  sink()
  viewer <- getOption("viewer")
  return(invisible(viewer(htmlFile)))
}
