library(testthat)

#' Remove any random PDF files.
pdfFiles<- list.files(path = "testthat",pattern = ".pdf")
if(length(pdfFiles)>0){
  for (i in 1:length(pdfFiles))
    file.remove(paste0("testthat/",pdfFiles[i]))
}

testsResults <- test_dir("testthat")

if (any(testsResults$failed!=0)) {
  failures<- testsResults[testsResults$failed!=0,]
  print(failures$test)
  stop("At least one test failed.")
} 

#' Remove any random PDF files.
pdfFiles<- list.files(path = "testthat",pattern = ".pdf")
if(length(pdfFiles)>0){
  for (i in 1:length(pdfFiles))
    file.remove(pdfFiles[i])
}
