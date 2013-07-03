#' Get simple (VTI for domestics) weights from AAF_V1
#' @param risk A vector of numeric risk levels, from 0 to 1, to calculate optimal portfolios from
#' @return A named vector of asset allocation weights
#' @keywords useful little functions, finance, portfolio
#' @seealso nothing
#' @export
#' @examples
#' aaf_v1_simple(.5)

aaf_v1_simple<-function(risk) {
  aa<- aaf_v1(risk)
  n_aa<- c(aa["tip"],aa["shy"],sum(aa[c("vti","iws","iwn")]),aa["efa"],aa["eem"] )
  names(n_aa)<-c("tip","shy","vti","efa","eem")
  return(n_aa)
}

