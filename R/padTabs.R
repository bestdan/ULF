
padTabs<-function(x,n,padString=" ") {
  #   x<- c("hi","hello","the")
  #   n<-3
  #   padString<-" "
  xmax<-  max(nchar(x))
  newLen<-xmax+n
  pax<- newLen-nchar(x)
  newStrings<-sapply(x, function(y) paste0(y, paste0(rep(padString, (newLen-nchar(y))),collapse="")))
  return(newStrings)
}


# Non-reactive data.  -----------------------------------------------------
