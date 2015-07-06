

context("gatherVars")

test_that("Global works", {
  rm(list=ls())
  x <- data.frame(x=rep(5,5))
  y <- 10
  
  temp<- gatherVars()
  
  expect_true(all(names(temp) == c('x','y')))
  expect_true(class(temp$x) == 'data.frame')
  expect_true(class(temp$y) == 'numeric')
  
})


test_that("encapsulation works", {
  rm(list=ls())
  # These are decoys. 
  x <- data.frame(x=rep(5,5))
  y <- 10
  
  tryThis<- function(){
    a<- data.frame(a=c(1,2,3))
    b<- "hello"
    temp<- gatherVars()
    return(temp)
  }
  results<- tryThis()
  
  expect_true(all(names(results) == c('a','b')))
  expect_true(class(results$a) == 'data.frame')
  expect_true(class(results$b) == 'character')

})
