#' Pull dataframe names that match a specific pattern
#' @description Grabs all the names in a data frame which match a specific patter. 
#' @param df input dataframe
#' @param pattern input string
#' @return string name vector
#' @keywords color alpha 
#' @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' temp<-data.frame(var_1=seq(1:10),var2=seq(1:10),v2=seq(1:10))
#' df_names("_",temp)

df_names <-
  function(pattern,df) {
  #Function to find names matching X within Y
    names(df)[grep(pattern,names(df))]
}
